# Download OPEC MOMR PDFs from the verified registry and record every outcome.

library(tidyverse)

source("R/source_helpers.R")
source("R/opec_helpers.R")
source("R/opec_download_helpers.R")

ensure_project_dirs()

start_date <- as.Date(Sys.getenv("OPEC_START_DATE", "2001-01-01"))
end_date <- as.Date(Sys.getenv("OPEC_END_DATE", format(Sys.Date(), "%Y-%m-01")))
max_attempts <- 3L
timeout_seconds <- 120L
refresh_pdfs <- Sys.getenv("OPEC_REFRESH_PDFS", "false")
if (!refresh_pdfs %in% c("true", "false")) {
  rlang::abort("OPEC_REFRESH_PDFS must be true or false", class = "opec_configuration_error")
}

if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
  stop("Set OPEC_START_DATE and OPEC_END_DATE to a valid inclusive date range.", call. = FALSE)
}

reports <- readr::read_csv(
  "registry/opec_momr_reports.csv",
  col_types = readr::cols(
    report_month = readr::col_date(),
    observation_month = readr::col_date(),
    appendix_url = readr::col_character(),
    pdf_url = readr::col_character(),
    pdf_source = readr::col_character(),
    page_price = readr::col_integer(),
    page_production = readr::col_integer(),
    status = readr::col_character(),
    notes = readr::col_character()
  )
) %>%
  dplyr::filter(
    .data$report_month >= start_date,
    .data$report_month <= end_date,
    .data$status %in% c("active", "candidate", "unavailable"),
    !is.na(.data$pdf_url),
    nzchar(.data$pdf_url)
  ) %>%
  dplyr::arrange(.data$report_month)

if (nrow(reports) == 0L) {
  stop("No eligible OPEC MOMR reports were found for the requested date range.", call. = FALSE)
}

# Check the live latest-report endpoint alongside the registry's archive URLs.
# Its delivery URL is eligible only for the publication month named on that page.
latest <- if (source_acquisition_mode() == "update") {
  tryCatch(discover_opec_latest_report(timeout_seconds = 20), error = identity)
} else tibble(report_month = as.Date(character()), pdf_url = character())
if (inherits(latest, "error")) {
  warning(paste("OPEC latest-report discovery failed:", conditionMessage(latest)), call. = FALSE)
  readr::write_csv(tibble(status = "error", details = conditionMessage(latest)), "data/raw/opec_latest_discovery.csv")
  latest <- tibble(report_month = as.Date(character()), pdf_url = character())
} else {
  readr::write_csv(latest %>% mutate(status = "ok"), "data/raw/opec_latest_discovery.csv")
}

download_log <- purrr::pmap_dfr(reports, function(...) {
  report <- tibble::as_tibble(list(...))
  report_month <- report$report_month[[1]]
  pdf_path <- file.path(opec_raw_dir(report_month), "momr.pdf")
  receipt_path <- file.path(opec_raw_dir(report_month), "download_receipt.csv")
  receipt <- if (file.exists(receipt_path)) readr::read_csv(receipt_path, show_col_types = FALSE) else NULL

  # A nonempty HTML error page is not a cached report; validate the PDF container.
  cache_valid <- FALSE
  if (file.exists(pdf_path)) {
    cache_check <- tryCatch({
      if (!identical(rawToChar(readBin(pdf_path, "raw", n = 5L)), "%PDF-")) {
        stop("Cached response is not a PDF", call. = FALSE)
      }
      pdftools::pdf_info(pdf_path)$pages > 4L
    }, error = identity)
    cache_valid <- refresh_pdfs == "false" && isTRUE(cache_check) && (is.null(receipt) ||
      (identical(receipt$pdf_url[[1]], report$pdf_url[[1]]) &&
       identical(receipt$pdf_md5[[1]], unname(tools::md5sum(pdf_path)))))
  }
  if (cache_valid) {
    if (is.null(receipt)) readr::write_csv(tibble(
      pdf_url = report$pdf_url[[1]], pdf_md5 = unname(tools::md5sum(pdf_path)),
      recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), validation = "existing_pdf_validated"
    ), receipt_path)
    return(tibble::tibble(
      report_month = report_month,
      observation_month = report$observation_month[[1]],
      pdf_url = report$pdf_url[[1]],
      pdf_path = pdf_path,
      status = "cached",
      error_message = NA_character_,
      recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  }

  tryCatch(
    {
      # Try the registered archive and the matching live issue in a fixed order.
      # Keep URL-specific failures visible if neither source can provide the issue.
      staged_path <- paste0(pdf_path, ".staged.pdf")
      # A manually supplied issue can retain a rolling source URL in its provenance.
      # Use dated archive candidates for that row; gate the rolling URL by the live month.
      archive_urls <- report$pdf_url[[1]]
      if (stringr::str_detect(archive_urls, "momr\\.opec\\.org/pdf-download/")) {
        month_id <- as.integer(format(report_month, "%m"))
        slugs <- unique(tolower(c(month.name[month_id], month.abb[month_id])))
        archive_urls <- sprintf("https://www.opec.org/assets/assetdb/momr-%s-%s.pdf",
                                 slugs, format(report_month, "%Y"))
      }
      urls <- unique(c(archive_urls, latest$pdf_url[latest$report_month == report_month]))
      downloaded <- download_opec_report_pdf(urls = urls, file_path = staged_path,
        report_month = report_month, max_attempts = max_attempts, timeout_seconds = timeout_seconds)
      if (!file.copy(staged_path, pdf_path, overwrite = TRUE)) {
        stop("Could not archive validated PDF", call. = FALSE)
      }
      unlink(staged_path)
      readr::write_csv(tibble(
        pdf_url = report$pdf_url[[1]], download_url = downloaded$download_url,
        attempt_errors = downloaded$attempt_errors,
        pdf_md5 = unname(tools::md5sum(pdf_path)),
        recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), validation = "download_validated"
      ), receipt_path)

      tibble::tibble(
        report_month = report_month,
        observation_month = report$observation_month[[1]],
        pdf_url = downloaded$download_url,
        pdf_path = pdf_path,
        status = "downloaded",
        error_message = NA_character_,
        recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    },
    error = function(error) {
      tibble::tibble(
        report_month = report_month,
        observation_month = report$observation_month[[1]],
        pdf_url = report$pdf_url[[1]],
        pdf_path = pdf_path,
        status = if_else(report$status[[1]] == "unavailable", "unavailable", "error"),
        error_message = conditionMessage(error),
        recorded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    }
  )
})

log_path <- "data/raw/oil_09_opec_momr_download_log.csv"
readr::write_csv(download_log, log_path)

if (any(download_log$status == "error")) {
  stop(sprintf("One or more OPEC downloads failed; inspect %s.", log_path), call. = FALSE)
}
message(sprintf("OPEC downloads: %s cached, %s downloaded, %s documented retrieval gaps.",
                sum(download_log$status == "cached"), sum(download_log$status == "downloaded"),
                sum(download_log$status == "unavailable")))
