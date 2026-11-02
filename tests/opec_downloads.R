# Offline checks protect dated archives from a changing latest-report endpoint.
library(tidyverse)
source("R/source_helpers.R")
source("R/opec_download_helpers.R")

# Resolve relative delivery links and publication months without machine locale assumptions.
latest_page <- xml2::read_html(paste0(
  '<h1>Download Monthly Oil Market Report - October 2026</h1>',
  '<a href="res/pdf_delivery_momr.php?secToken2=1">click here</a>'
))
latest <- opec_latest_report_from_html(latest_page, "https://momr.opec.org/pdf-download/")
stopifnot(identical(latest$report_month, as.Date("2026-10-01")),
          identical(latest$pdf_url, "https://momr.opec.org/pdf-download/res/pdf_delivery_momr.php?secToken2=1"))

# Fail explicitly when the source layout no longer identifies exactly one report link.
missing_link <- tryCatch(
  opec_latest_report_from_html(xml2::read_html('<h1>Download Monthly Oil Market Report - October 2026</h1>'),
                               "https://momr.opec.org/pdf-download/"),
  opec_latest_schema_error = identity
)
stopifnot(inherits(missing_link, "opec_latest_schema_error"))

# A real multi-page PDF must name the requested issue on its cover.
pdf_path <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_path)
for (page in seq_len(5L)) {
  graphics::plot.new()
  graphics::text(0.5, 0.5, if (page == 1L) "13 October 2026" else "Synthetic MOMR fixture")
}
grDevices::dev.off()
stopifnot(isTRUE(validate_opec_report_pdf(pdf_path, as.Date("2026-10-01"))))
wrong_month <- tryCatch(validate_opec_report_pdf(pdf_path, as.Date("2026-09-01")),
                         opec_pdf_month_error = identity)
stopifnot(inherits(wrong_month, "opec_pdf_month_error"))

# Public delivery endpoints sometimes return an HTML error body instead of a report.
html_path <- tempfile(fileext = ".pdf")
writeLines("<html>Access denied</html>", html_path)
html_error <- tryCatch(validate_opec_report_pdf(html_path, as.Date("2026-10-01")),
                        opec_pdf_validation_error = identity)
stopifnot(inherits(html_error, "opec_pdf_validation_error"))

# Replay a failed archive response followed by a valid latest-report PDF, entirely offline.
file_urls <- paste0("file://", if (.Platform$OS.type == "windows") "/" else "",
                    normalizePath(c(html_path, pdf_path), winslash = "/", mustWork = TRUE))
staged_path <- tempfile(fileext = ".pdf")
downloaded <- download_opec_report_pdf(urls = file_urls, file_path = staged_path,
  report_month = as.Date("2026-10-01"), max_attempts = 1L, timeout_seconds = 5L)
stopifnot(identical(downloaded$download_url, file_urls[[2]]),
          nzchar(downloaded$attempt_errors),
          identical(unname(tools::md5sum(staged_path)), unname(tools::md5sum(pdf_path))))
# Rejected candidates must never leave a staged replacement behind.
rejected <- tryCatch(download_opec_report_pdf(urls = file_urls, file_path = staged_path,
  report_month = as.Date("2026-09-01"), max_attempts = 1L, timeout_seconds = 5L),
  opec_download_error = identity)
stopifnot(inherits(rejected, "opec_download_error"), !file.exists(staged_path))
unlink(pdf_path)
unlink(html_path)
message("OPEC latest-report discovery and PDF month-validation checks passed.")
