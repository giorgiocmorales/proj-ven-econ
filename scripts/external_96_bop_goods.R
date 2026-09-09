# Build report-ready BCV goods exports and imports from annual and quarterly BOP extracts.

library(tidyverse)

source("R/source_helpers.R")
ensure_project_dirs()

series_registry <- readr::read_csv(
  "registry/external_bop_goods_series.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    frequency = readr::col_character(),
    row_id = readr::col_double(),
    series_label = readr::col_character(),
    variable_id = readr::col_character(),
    display_label = readr::col_character(),
    source_sign = readr::col_double()
  )
)

required_registry_columns <- c(
  "frequency", "row_id", "series_label", "variable_id", "display_label", "source_sign"
)
missing_registry_columns <- setdiff(required_registry_columns, names(series_registry))

if (length(missing_registry_columns) > 0) {
  stop(
    sprintf(
      "Goods-trade registry is missing columns: %s",
      paste(missing_registry_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

annual_bop <- readr::read_csv(
  "data/processed/external_05_bop_a_bcv.csv",
  show_col_types = FALSE
)
quarterly_bop <- readr::read_csv(
  "data/processed/external_06_bop_q_bcv.csv",
  show_col_types = FALSE
)

bop_data <- dplyr::bind_rows(annual_bop, quarterly_bop)
mapped_bop <- bop_data %>%
  dplyr::inner_join(
    series_registry,
    by = c("frequency", "row_id"),
    suffix = c("_source", "_registry")
  )

expected_series <- series_registry %>%
  dplyr::distinct(.data$frequency, .data$row_id)
observed_series <- mapped_bop %>%
  dplyr::distinct(.data$frequency, .data$row_id)
missing_series <- dplyr::anti_join(
  expected_series,
  observed_series,
  by = c("frequency", "row_id")
)

if (nrow(missing_series) > 0) {
  stop(
    sprintf(
      "BCV goods-trade extract is missing mapped series: %s",
      paste(paste(missing_series$frequency, missing_series$row_id, sep = "/"), collapse = ", ")
    ),
    call. = FALSE
  )
}

label_mismatches <- mapped_bop %>%
  dplyr::filter(.data$series_label_source != .data$series_label_registry)

if (nrow(label_mismatches) > 0) {
  stop("BCV goods-trade series labels do not match the approved registry", call. = FALSE)
}

invalid_source_signs <- mapped_bop %>%
  dplyr::filter(
    (.data$source_sign == 1 & .data$value < 0) |
      (.data$source_sign == -1 & .data$value > 0)
  )

if (nrow(invalid_source_signs) > 0) {
  stop("BCV goods-trade source values have an unexpected sign", call. = FALSE)
}

external_bop_goods <- mapped_bop %>%
  dplyr::transmute(
    date = .data$date,
    frequency = .data$frequency,
    variable_id = .data$variable_id,
    display_label = .data$display_label,
    value_source = .data$value,
    value = .data$value * .data$source_sign,
    unit = .data$unit,
    provisional = .data$provisional,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    series_label = .data$series_label_source,
    source_row_id = .data$row_id,
    downloaded_at = .data$downloaded_at
  ) %>%
  dplyr::arrange(.data$frequency, .data$variable_id, .data$date)

duplicate_keys <- external_bop_goods %>%
  dplyr::count(.data$frequency, .data$variable_id, .data$date) %>%
  dplyr::filter(.data$n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("Report-ready BCV goods-trade output contains duplicate keys", call. = FALSE)
}

write_final_dataset(external_bop_goods, "external_bop_goods")
