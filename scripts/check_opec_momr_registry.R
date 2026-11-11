# Check that the OPEC MOMR registry has one usable PDF URL per report month.

library(tidyverse)

reports <- readr::read_csv(
  "registry/opec_momr_reports.csv",
  col_types = readr::cols(
    report_month = readr::col_date(),
    pdf_url = readr::col_character(),
    status = readr::col_character(),
    .default = readr::col_skip()
  )
)

duplicate_months <- reports %>%
  dplyr::count(.data$report_month, name = "n_reports") %>%
  dplyr::filter(.data$n_reports != 1L)

ineligible_reports <- reports %>%
  dplyr::filter(
    .data$status %in% c("active", "candidate"),
    (is.na(.data$pdf_url) | !nzchar(.data$pdf_url))
  )

if (nrow(duplicate_months) > 0L) {
  stop("The OPEC MOMR registry has duplicate report months.", call. = FALSE)
}
if (nrow(ineligible_reports) > 0L) {
  stop("An eligible OPEC MOMR registry row has no PDF URL.", call. = FALSE)
}

message(sprintf("OPEC MOMR registry check passed: %s reports.", nrow(reports)))
