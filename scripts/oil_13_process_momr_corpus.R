# Process every registered MOMR into monthly production and crude-price observations.
# Successful per-report results are cached by PDF and parser hashes for resumable updates.
# A failed report never replaces validated final outputs; inspect the audit CSV first.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
source("R/opec_corpus_helpers.R")
ensure_project_dirs()

reports <- readr::read_csv("registry/opec_momr_reports.csv", show_col_types = FALSE) %>%
  filter(.data$report_month <= floor_date(Sys.Date(), "month")) %>%
  mutate(pdf_path = file.path("data/raw/opec", format(.data$report_month, "%Y-%m"), "momr.pdf")) %>%
  arrange(.data$report_month)
if (anyDuplicated(reports$report_month)) rlang::abort("Duplicate registry report months", class = "opec_registry_error")
dir.create("data/interim/opec", recursive = TRUE, showWarnings = FALSE)
parser_hash <- paste(unname(tools::md5sum(c("R/opec_corpus_helpers.R",
                                          "scripts/oil_13_process_momr_corpus.R"))), collapse = "_")
outcomes <- list()
observations <- list()

# Keep audit state after every report so an interrupted run can be diagnosed and resumed.
for (i in seq_len(nrow(reports))) {
  report <- reports[i, ]
  month <- report$report_month[[1]]
  path <- report$pdf_path[[1]]
  key <- format(month, "%Y-%m")
  valid_signature <- file.exists(path) && identical(rawToChar(readBin(path, "raw", n = 5L)), "%PDF-")
  if (!valid_signature) {
    outcomes[[key]] <- tibble(report_month = month, source_type = c("secondary", "direct", "price"),
                              status = if_else(report$status[[1]] == "unavailable", "unavailable_pdf", "missing_pdf"),
                              observations = 0L, details = paste(path, report$notes[[1]]))
  } else {
    pdf_hash <- unname(tools::md5sum(path))
    cache_path <- file.path("data/interim/opec", paste0(key, ".rds"))
    cached <- if (file.exists(cache_path)) readRDS(cache_path) else NULL
    if (!is.null(cached) && identical(cached$pdf_hash, pdf_hash) && identical(cached$parser_hash, parser_hash) &&
        identical(cached$source_url, report$pdf_url[[1]])) {
      outcomes[[key]] <- cached$audit
      observations[[key]] <- cached$data
    } else {
      # Cache text independently of parser version; PDF changes invalidate both caches.
      text_path <- file.path("data/interim/opec", paste0(key, "_text.rds"))
      text_cache <- if (file.exists(text_path)) readRDS(text_path) else NULL
      pages <- tryCatch({
        if (!is.null(text_cache) && identical(text_cache$pdf_hash, pdf_hash)) text_cache$pages else {
          extracted_pages <- pdftools::pdf_text(path)
          saveRDS(list(pdf_hash = pdf_hash, pages = extracted_pages), text_path)
          extracted_pages
        }
      }, error = identity)
      # Detect a valid PDF for the wrong month before assigning any observation dates.
      if (!inherits(pages, "error")) {
        cover_pattern <- paste0(month.name[lubridate::month(month)], "\\s*", lubridate::year(month))
        cover_page <- which(nzchar(str_trim(pages)))[[1]]
        # June 2004 carries an erratum sheet before its cover.
        if (str_detect(pages[[cover_page]], fixed("Erratum Corrigendum"))) cover_page <- cover_page + 1L
        if (!str_detect(pages[[cover_page]], regex(cover_pattern, ignore_case = TRUE))) {
          pages <- rlang::error_cnd("opec_report_identity_error", message = "Report month does not match the PDF cover")
        }
      }
      result <- map(c("secondary", "direct", "price"), function(kind) {
        data <- if (inherits(pages, "error")) pages else tryCatch(
          extract_opec_monthly_table(pages, month, kind), error = identity
        )
        if (inherits(data, "error")) {
          # The direct table starts in April 2012, with January-March observations.
          absent <- inherits(data, "opec_table_absent") && kind == "direct" && month < as.Date("2012-04-01")
          return(list(data = tibble(), audit = tibble(
            report_month = month, source_type = kind,
            status = if_else(absent, "not_published", "error"), observations = 0L,
            details = conditionMessage(data)
          )))
        }
        data <- data %>% mutate(report_month = month, source_id = "opec", source_pdf_url = report$pdf_url[[1]],
                                pdf_file = path, pdf_md5 = pdf_hash, parser_version = parser_hash)
        list(data = data, audit = tibble(report_month = month, source_type = kind,
                                        status = "ok", observations = nrow(data), details = NA_character_))
      })
      outcomes[[key]] <- map_dfr(result, "audit")
      observations[[key]] <- map_dfr(result, "data")
      if (all(outcomes[[key]]$status %in% c("ok", "not_published"))) {
        saveRDS(list(pdf_hash = pdf_hash, parser_hash = parser_hash,
                    source_url = report$pdf_url[[1]], data = observations[[key]], audit = outcomes[[key]]), cache_path)
      }
    }
  }
  write_csv(bind_rows(outcomes), "data/interim/opec_corpus_audit.csv")
  message(sprintf("MOMR %s: %s", key, paste(outcomes[[key]]$status, collapse = ", ")))
}

# Always expose staged observations and coverage; only promote an error-free corpus.
audit <- bind_rows(outcomes)
corpus <- bind_rows(observations)
write_csv(corpus, "data/interim/opec_corpus_observations.csv")
if (any(audit$status %in% c("error", "missing_pdf"))) {
  rlang::abort("Incomplete OPEC corpus; inspect data/interim/opec_corpus_audit.csv. Final datasets were not replaced.",
               class = "opec_corpus_incomplete")
}
# Resolve names through a reviewed catalog; new countries or crudes require an explicit mapping.
aliases <- read_csv("registry/opec_series_aliases.csv", show_col_types = FALSE)
if (anyDuplicated(aliases[c("domain", "source_label")])) {
  rlang::abort("Duplicate OPEC alias mappings", class = "opec_alias_error")
}
corpus <- corpus %>% mutate(
  domain = if_else(.data$source_type == "price", "price", "production"),
  source_label = str_remove(.data$label, "(?<=[A-Za-z])1$|\\*$")
) %>% left_join(aliases, by = c("domain", "source_label"), relationship = "many-to-one")
unknown <- corpus %>% filter(is.na(.data$series_id)) %>% distinct(.data$domain, .data$source_label)
write_csv(unknown, "data/interim/opec_unmapped_series.csv")
if (nrow(unknown) > 0L) rlang::abort("Unmapped OPEC series; inspect opec_unmapped_series.csv", class = "opec_alias_error")
if (anyDuplicated(corpus[c("report_month", "source_type", "series_id", "date")])) {
  rlang::abort("Duplicate canonical OPEC observations", class = "opec_duplicate_error")
}
# Reconcile complete country rows against their printed OPEC total, allowing rounding.
# Country membership follows each report's row order, not today's OPEC membership.
reconciliation <- corpus %>% filter(.data$source_type != "price") %>%
  group_by(.data$report_month, .data$source_type, .data$date) %>%
  mutate(row_order = row_number(),
         total_order = if (any(.data$label == "Total OPEC")) which(.data$label == "Total OPEC") else NA_integer_) %>%
  filter(.data$row_order <= .data$total_order) %>%
  summarise(total = .data$value[.data$label == "Total OPEC"],
            country_sum = sum(.data$value[.data$entity_type == "country"]),
            countries = sum(.data$entity_type == "country"), .groups = "drop") %>%
  mutate(difference = .data$total - .data$country_sum,
         tolerance = pmax(1, (.data$countries + 1) / 2),
         status = case_when(is.na(.data$difference) ~ "incomplete_published_values",
                            abs(.data$difference) <= .data$tolerance ~ "ok", TRUE ~ "error"))
write_csv(reconciliation, "data/interim/opec_corpus_reconciliation.csv")
if (any(reconciliation$status == "error")) {
  rlang::abort("OPEC country rows do not reconcile with the published total", class = "opec_reconciliation_error")
}
write_csv(corpus, "data/processed/oil_13_momr_corpus.csv")

# Preserve published missing values in the newest vintage instead of carrying old values forward.
latest <- corpus %>% arrange(.data$report_month) %>%
  group_by(.data$source_type, .data$series_id, .data$date) %>% slice_tail(n = 1L) %>% ungroup()
write_csv(latest, "data/final/oil_13_momr_latest.csv")
coverage <- latest %>% group_by(.data$source_type, .data$series_id, .data$display_label, .data$unit) %>%
  summarise(first_date = min(.data$date), last_date = max(.data$date), observations = n(),
            reported = sum(.data$value_status == "reported"),
            expected_months = length(seq(min(.data$date), max(.data$date), by = "month")), .groups = "drop")
write_csv(coverage, "data/final/oil_13_momr_coverage.csv")
# Publish exact missing months separately from explicitly unreported numeric cells.
gaps <- latest %>% group_by(.data$source_type, .data$series_id) %>%
  complete(date = seq(min(.data$date), max(.data$date), by = "month")) %>%
  filter(is.na(.data$report_month) | .data$value_status == "not_reported") %>%
  mutate(gap_type = if_else(is.na(.data$report_month), "no_observation", "reported_missing")) %>% ungroup()
write_csv(gaps, "data/final/oil_13_momr_gaps.csv")
write_csv(audit, "data/final/oil_13_momr_report_audit.csv")
message(sprintf("Published %s observations; %s reports have documented retrieval gaps.",
                nrow(latest), n_distinct(audit$report_month[audit$status == "unavailable_pdf"])))
