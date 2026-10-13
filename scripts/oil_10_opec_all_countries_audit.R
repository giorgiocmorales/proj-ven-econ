# Audit the generic all-country MOMR extractor on locally cached reports.

library(tidyverse)
library(pdftools)

source("R/opec_helpers.R")
source("R/opec_historical_helpers.R")

cached_reports <- tibble::tibble(
  report_date = as.Date(c("2025-04-01", "2025-05-01")),
  pdf_path = c(
    "data/raw/opec/2025-05/momr.pdf",
    "data/raw/opec/2025-06/momr.pdf"
  )
) %>%
  dplyr::filter(file.exists(.data$pdf_path))

if (nrow(cached_reports) == 0L) {
  stop("No cached OPEC MOMR PDFs are available for the all-country audit.", call. = FALSE)
}

audit_results <- purrr::pmap_dfr(cached_reports, function(report_date, pdf_path) {
  purrr::map_dfr(c("secondary", "direct"), function(source_type) {
    production_data <- extract_opec_production(
      pdf_path = pdf_path,
      report_date = report_date,
      source_type = source_type,
      y_tolerance = 2,
      country_max_x = 170,
      monthly_min_x = 365,
      monthly_max_x = 500
    )

    audit_opec_production(
      production_data,
      expected_observations = 3L,
      require_total_opec = source_type == "secondary"
    ) %>%
      dplyr::mutate(
        report_date = report_date,
        source_type = source_type,
        pdf_path = pdf_path
      )
  })
})

readr::write_csv(audit_results, "data/raw/oil_10_opec_all_countries_audit.csv")
