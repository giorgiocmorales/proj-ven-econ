# Process cached OPEC MOMR PDFs into an all-country production dataset.

library(tidyverse)
library(pdftools)

source("R/source_helpers.R")
source("R/opec_helpers.R")
source("R/opec_historical_helpers.R")

ensure_project_dirs()

start_date <- as.Date(Sys.getenv("OPEC_START_DATE", "2001-01-01"))
end_date <- as.Date(Sys.getenv("OPEC_END_DATE", format(Sys.Date(), "%Y-%m-01")))

if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
  stop("Set OPEC_START_DATE and OPEC_END_DATE to a valid inclusive date range.", call. = FALSE)
}

reports <- readr::read_csv(
  "registry/opec_momr_reports.csv",
  col_types = readr::cols(
    report_month = readr::col_date(),
    observation_month = readr::col_date(),
    pdf_url = readr::col_character(),
    status = readr::col_character(),
    .default = readr::col_skip()
  )
) %>%
  dplyr::filter(
    .data$report_month >= start_date,
    .data$report_month <= end_date,
    .data$status %in% c("active", "candidate")
  ) %>%
  dplyr::mutate(pdf_path = file.path("data", "raw", "opec", format(.data$report_month, "%Y-%m"), "momr.pdf")) %>%
  dplyr::filter(file.exists(.data$pdf_path)) %>%
  dplyr::arrange(.data$report_month)

if (nrow(reports) == 0L) {
  stop("No cached OPEC MOMR PDFs were found for the requested date range.", call. = FALSE)
}

layouts <- readr::read_csv(
  "registry/opec_momr_layouts.csv",
  col_types = readr::cols(
    layout_id = readr::col_character(),
    first_report_month = readr::col_date(),
    last_report_month = readr::col_date(),
    production_layout = readr::col_character(),
    direct_available = readr::col_logical(),
    y_tolerance = readr::col_double(),
    country_max_x = readr::col_double(),
    value_match_tolerance = readr::col_double(),
    .default = readr::col_skip()
  )
)

results <- purrr::pmap(reports, function(...) {
  report <- tibble::as_tibble(list(...))
  layout <- layouts %>%
    dplyr::filter(
      report$report_month[[1]] >= .data$first_report_month,
      report$report_month[[1]] <= .data$last_report_month
    )

  if (nrow(layout) != 1L) {
    stop(sprintf("Expected one OPEC layout rule for report month %s.", report$report_month[[1]]), call. = FALSE)
  }

  source_types <- if (layout$direct_available[[1]]) c("secondary", "direct") else "secondary"

  purrr::map(source_types, function(source_type) {
    tryCatch(
      {
        if (layout$production_layout[[1]] == "doc_secondary_and_direct") {
          production_data <- extract_opec_production(
            pdf_path = report$pdf_path[[1]],
            report_date = report$observation_month[[1]],
            source_type = source_type,
            y_tolerance = layout$y_tolerance[[1]],
            country_max_x = layout$country_max_x[[1]],
            monthly_min_x = 365,
            monthly_max_x = 500
          )
          expected_observations <- 3L
        } else {
          production_data <- extract_opec_legacy_production(
            pdf_path = report$pdf_path[[1]],
            report_date = report$observation_month[[1]],
            source_type = source_type,
            y_tolerance = layout$y_tolerance[[1]],
            country_max_x = layout$country_max_x[[1]],
            value_match_tolerance = layout$value_match_tolerance[[1]]
          )
          expected_observations <- 1L
        }
        audit_opec_production(
          production_data,
          expected_observations = expected_observations,
          require_total_opec = source_type == "secondary"
        )
        production_data <- production_data %>%
          dplyr::mutate(report_month = report$report_month[[1]])

        list(data = production_data, error = tibble::tibble())
      },
      error = function(error) {
        list(
          data = tibble::tibble(),
          error = tibble::tibble(
            report_month = report$report_month[[1]],
            observation_month = report$observation_month[[1]],
            source_type = source_type,
            pdf_url = report$pdf_url[[1]],
            pdf_path = report$pdf_path[[1]],
            error_message = conditionMessage(error)
          )
        )
      }
    )
  })
})

production_data <- results %>%
  purrr::flatten() %>%
  purrr::map_dfr("data")
production_errors <- results %>%
  purrr::flatten() %>%
  purrr::map_dfr("error")

if (nrow(production_errors) > 0L) {
  write_raw_dataset(production_errors, "oil_10_opec_all_countries_errors")
  stop("One or more cached OPEC PDFs could not be processed; inspect the error dataset.", call. = FALSE)
}

processed_path <- file.path("data", "processed", "oil_10_opec_all_countries.csv")
existing_data <- if (file.exists(processed_path)) {
  readr::read_csv(processed_path, show_col_types = FALSE)
} else {
  tibble::tibble()
}

if (
  nrow(existing_data) > 0L &&
    (!"report_month" %in% names(existing_data) || any(is.na(existing_data$report_month)))
) {
  message("Rebuilding legacy all-country OPEC output without report vintages.")
  existing_data <- tibble::tibble()
}

if (nrow(production_data) > 0L) {
  replacement_months <- unique(production_data$report_month)
  updated_data <- if (nrow(existing_data) == 0L) {
    production_data
  } else {
    dplyr::bind_rows(
      existing_data %>% dplyr::filter(!.data$report_month %in% replacement_months),
      production_data
    )
  }

  write_processed_dataset(updated_data, "oil_10_opec_all_countries")

  updated_data %>%
    dplyr::arrange(.data$source_type, .data$country, .data$date, .data$report_month) %>%
    dplyr::group_by(.data$source_type, .data$country, .data$date) %>%
    dplyr::slice_tail(n = 1L) %>%
    dplyr::ungroup() %>%
    write_final_dataset("oil_10_opec_all_countries_latest")
}
