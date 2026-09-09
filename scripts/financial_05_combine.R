# Combine BCV financial-sector extracts into one deduplicated, comparable database.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")

dataset_id <- "financial_series"
interim_ids <- c(
  "financial_01_balances_bcv",
  "financial_02_reserve_coefficients_bcv",
  "financial_03_reserve_positions_bcv"
)
interim_paths <- file.path("data", "interim", paste0(interim_ids, ".csv"))
missing_paths <- interim_paths[!file.exists(interim_paths)]

if (length(missing_paths) > 0) {
  stop(
    sprintf("Missing financial interim datasets: %s", paste(missing_paths, collapse = ", ")),
    call. = FALSE
  )
}

financial_series_raw <- purrr::map_dfr(interim_paths, function(path) {
  readr::read_csv(path, show_col_types = FALSE)
})

key_columns <- c(
  "dataset_family",
  "frequency",
  "institution_id",
  "variable_id",
  "target_currency",
  "date"
)

financial_series <- financial_series_raw %>%
  dplyr::mutate(
    value = .data$value_raw * .data$conversion_factor,
    dataset_id = dataset_id
  ) %>%
  dplyr::arrange(
    dplyr::across(dplyr::all_of(key_columns)),
    dplyr::desc(.data$source_priority)
  ) %>%
  dplyr::distinct(dplyr::across(dplyr::all_of(key_columns)), .keep_all = TRUE) %>%
  dplyr::select(
    .data$dataset_id,
    .data$dataset_family,
    .data$date,
    .data$frequency,
    .data$institution_id,
    .data$variable_id,
    .data$value,
    currency = .data$target_currency,
    unit = .data$target_unit,
    .data$value_raw,
    .data$currency_raw,
    .data$unit_raw,
    .data$conversion_factor,
    .data$series_label_raw,
    .data$provisional,
    .data$source_id,
    .data$source_url,
    .data$source_file,
    .data$source_vintage,
    .data$source_priority,
    .data$sheet_name,
    .data$row_id,
    .data$column_id,
    .data$downloaded_at
  ) %>%
  dplyr::arrange(
    .data$dataset_family,
    .data$frequency,
    .data$institution_id,
    .data$variable_id,
    .data$currency,
    .data$date
  )

duplicate_keys <- financial_series %>%
  dplyr::count(
    .data$dataset_family,
    .data$frequency,
    .data$institution_id,
    .data$variable_id,
    .data$currency,
    .data$date
  ) %>%
  dplyr::filter(.data$n > 1)

if (nrow(duplicate_keys) > 0) {
  stop(sprintf("Financial final database contains %s duplicated keys.", nrow(duplicate_keys)), call. = FALSE)
}

if (any(is.na(financial_series$value))) {
  stop("Financial final database contains missing normalized values.", call. = FALSE)
}

write_final_dataset(financial_series, dataset_id)
