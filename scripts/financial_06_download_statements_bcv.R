# Archive all linked BCV financial-statement PDFs; content extraction is a later stage.
# BCV_STATEMENTS_REFRESH_PDFS=true rechecks source files while retaining prior PDF vintages.
library(tidyverse)
source("R/source_helpers.R")
source("R/bcv_statement_helpers.R")
ensure_project_dirs()

# Keep listing snapshots, staged downloads, immutable PDFs, and audit receipts together.
base_url <- "https://www.bcv.org.ve/bcv/estados-financieros"
raw_dir <- "data/raw/bcv_financial_statements"
registry_path <- "registry/bcv_financial_statements.csv"
for (directory in c("pages", "staging", "pdf")) {
  dir.create(file.path(raw_dir, directory), recursive = TRUE, showWarnings = FALSE)
}
refresh <- Sys.getenv("BCV_STATEMENTS_REFRESH_PDFS", "false")
if (!refresh %in% c("true", "false")) {
  rlang::abort("BCV_STATEMENTS_REFRESH_PDFS must be true or false", class = "bcv_statement_config_error")
}
run_id <- format(Sys.time(), "%Y%m%dT%H%M%S")
checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Discover the complete pager before publishing an inventory; failed pages are explicit.
first_path <- file.path(raw_dir, "pages", "page_000.html")
first_response <- tryCatch(
  httr2::request(base_url) %>% httr2::req_timeout(seconds = 45) %>%
    httr2::req_retry(max_tries = 2, retry_on_failure = TRUE) %>% httr2::req_perform(path = first_path),
  error = identity
)
if (inherits(first_response, "error")) {
  write_csv(tibble(run_id, listing_url = base_url, status = "error", details = conditionMessage(first_response)),
             file.path(raw_dir, "discovery_log.csv"))
  rlang::abort(conditionMessage(first_response), class = "bcv_statement_discovery_error")
}
first_page <- xml2::read_html(first_path)
last_page <- bcv_statement_last_page(first_page, base_url)
listing_urls <- c(base_url, if (last_page > 0L) paste0(base_url, "?page=", seq_len(last_page)) else character())
page_paths <- file.path(raw_dir, "pages", sprintf("page_%03d.html", seq.int(0L, last_page)))
responses <- list(first_response)
if (last_page > 0L) {
  requests <- map(listing_urls[-1], ~ httr2::request(.x) %>% httr2::req_timeout(seconds = 45) %>%
                    httr2::req_retry(max_tries = 2, retry_on_failure = TRUE))
  responses <- c(responses, httr2::req_perform_parallel(requests, paths = page_paths[-1],
    on_error = "continue", progress = FALSE, max_active = 4))
}
discovery_log <- tibble(run_id, listing_url = listing_urls,
  status = map_chr(responses, ~ if (inherits(.x, "error")) "error" else "ok"),
  details = map_chr(responses, ~ if (inherits(.x, "error")) conditionMessage(.x) else ""))
write_csv(discovery_log, file.path(raw_dir, "discovery_log.csv"))
if (any(discovery_log$status == "error")) {
  rlang::abort("BCV listing download failed; inspect discovery_log.csv", class = "bcv_statement_discovery_error")
}
links <- map2_dfr(page_paths, listing_urls, ~ bcv_statement_links(xml2::read_html(.x), .y))
write_csv(links, file.path(raw_dir, "discovered_links.csv"))

# Preserve each document URL once, including every printed title and listing location.
documents <- links %>% group_by(source_url) %>%
  summarise(title_raw = paste(sort(unique(title_raw)), collapse = " | "),
    listing_urls = paste(sort(unique(listing_url)), collapse = " | "), .groups = "drop") %>%
  mutate(source_filename = basename(URLdecode(str_remove(source_url, "[?].*$"))),
    label_date = lubridate::dmy(str_extract(title_raw, "\\b[0-9]{2}/[0-9]{2}/[0-9]{4}\\b")),
    filename_year = as.integer(str_extract(source_filename, "(?:19|20)[0-9]{2}")),
    date_review_required = !is.na(label_date) & !is.na(filename_year) & year(label_date) != filename_year,
    first_seen_at = checked_at, last_seen_at = checked_at, linked_on_latest_scan = TRUE,
    raw_file = NA_character_, pdf_md5 = NA_character_, page_count = NA_integer_, size_bytes = NA_real_,
    downloaded_at = NA_character_, checked_at = checked_at, status = "pending", error_message = NA_character_) %>%
  arrange(source_url)
if (file.exists(registry_path)) {
  previous <- read_csv(registry_path, show_col_types = FALSE,
    col_types = cols(.default = col_character(), date_review_required = col_logical(),
      linked_on_latest_scan = col_logical(), label_date = col_date(), filename_year = col_integer(),
      page_count = col_integer(), size_bytes = col_double()))
  if (anyNA(previous$source_url) || anyDuplicated(previous$source_url) > 0L) {
    rlang::abort("BCV statement registry contains missing or duplicate URLs", class = "bcv_statement_registry_error")
  }
  matches <- match(documents$source_url, previous$source_url)
  for (column in c("first_seen_at", "raw_file", "pdf_md5", "page_count", "size_bytes", "downloaded_at")) {
    present <- which(!is.na(matches))
    documents[[column]][present] <- previous[[column]][matches[present]]
  }
  # Retain prior discoveries even when BCV removes their links from its website.
  documents <- bind_rows(documents, previous %>% anti_join(documents, by = "source_url") %>%
    mutate(linked_on_latest_scan = FALSE))
}
write_csv(documents, registry_path)
message(sprintf("BCV financial statements: %s listing pages, %s linked PDF URLs.", length(listing_urls), n_distinct(links$source_url)))

# Reuse only matching, readable cached PDFs; refresh failures retain the previous archive.
download_rows <- which(documents$linked_on_latest_scan)
cached_rows <- integer()
if (refresh == "false") {
  for (i in download_rows) {
    path <- documents$raw_file[[i]]
    if (is.na(path) || !file.exists(path)) next
    cached <- tryCatch(validate_bcv_statement_pdf(path), error = identity)
    if (!inherits(cached, "error") && identical(cached$pdf_md5[[1]], documents$pdf_md5[[i]])) {
      documents$status[[i]] <- "cached"
      documents$error_message[[i]] <- NA_character_
      cached_rows <- c(cached_rows, i)
    } else {
      documents$error_message[[i]] <- if (inherits(cached, "error")) conditionMessage(cached) else "Cached PDF checksum mismatch"
    }
  }
}
pending_rows <- setdiff(download_rows, cached_rows)

# Bound network concurrency and checkpoint every batch for resumable initial downloads.
for (batch in split(pending_rows, ceiling(seq_along(pending_rows) / 20L))) {
  staged_paths <- file.path(raw_dir, "staging", sprintf("document_%04d.pdf", batch))
  requests <- map(documents$source_url[batch], ~ httr2::request(.x) %>%
    httr2::req_timeout(seconds = 60) %>% httr2::req_retry(max_tries = 2, retry_on_failure = TRUE))
  downloaded <- httr2::req_perform_parallel(requests, paths = staged_paths,
    on_error = "continue", progress = FALSE, max_active = 4)
  for (j in seq_along(batch)) {
    i <- batch[[j]]
    result <- if (inherits(downloaded[[j]], "error")) downloaded[[j]] else
      tryCatch(validate_bcv_statement_pdf(staged_paths[[j]]), error = identity)
    if (inherits(result, "error")) {
      documents$status[[i]] <- "error"
      documents$error_message[[i]] <- conditionMessage(result)
      unlink(staged_paths[[j]])
      next
    }
    # Content-addressed filenames preserve old vintages and deduplicate identical PDFs.
    archive_path <- file.path(raw_dir, "pdf", paste0(result$pdf_md5[[1]], ".pdf"))
    archive_matches <- file.exists(archive_path) &&
      identical(unname(tools::md5sum(archive_path)), result$pdf_md5[[1]])
    if (!archive_matches && !file.copy(staged_paths[[j]], archive_path, overwrite = TRUE)) {
      rlang::abort(paste("Cannot archive BCV PDF:", archive_path), class = "bcv_statement_archive_error")
    }
    for (column in names(result)) documents[[column]][[i]] <- result[[column]][[1]]
    documents$raw_file[[i]] <- archive_path
    documents$downloaded_at[[i]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    documents$status[[i]] <- "downloaded"
    documents$error_message[[i]] <- NA_character_
    unlink(staged_paths[[j]])
  }
  write_csv(documents, registry_path)
  write_csv(documents %>% filter(linked_on_latest_scan) %>% mutate(run_id = run_id, .before = 1),
               file.path(raw_dir, "download_log.csv"))
  message(sprintf("BCV statements: %s downloaded, %s cached, %s failed, %s pending.",
    sum(documents$status == "downloaded"), sum(documents$status == "cached"),
    sum(documents$status == "error"), sum(documents$status == "pending")))
}

# Save both the current state and append-only run receipts, including failures.
write_csv(documents, registry_path)
run_receipt <- documents %>% filter(linked_on_latest_scan) %>% mutate(run_id = run_id, .before = 1)
write_csv(run_receipt, file.path(raw_dir, "download_log.csv"))
history_path <- file.path(raw_dir, "download_history.csv")
write_csv(run_receipt, history_path, append = file.exists(history_path))
if (any(documents$status[download_rows] == "error")) {
  rlang::abort("BCV financial-statement downloads failed; inspect download_log.csv", class = "bcv_statement_download_error")
}
message(sprintf("BCV archive complete: %s linked documents, %s unique PDF hashes; content extraction pending.",
  length(download_rows), n_distinct(documents$pdf_md5[download_rows])))
