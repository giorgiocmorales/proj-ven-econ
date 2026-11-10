# Build monthly nominal and CPI-adjusted deposits from harmonized BCV series.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

# Define the auditable inputs and fail explicitly when either is unavailable.
dataset_id <- "monetary_05_deposits_real_bcv"
liquidity_path <- "data/final/monetary_02_liq_bcv.csv"
cpi_path <- "data/final/prices_01_cpi_bcv.csv"
grouped_cpi_path <- "data/final/prices_03_cpi_groups_bcv.csv"
overlap_path <- "data/interim/prices_03_headline_overlap_check.csv"
input_paths <- c(liquidity_path, cpi_path, grouped_cpi_path, overlap_path)
missing_paths <- input_paths[!file.exists(input_paths)]

if (length(missing_paths) > 0) {
  stop(sprintf("Missing deposit deflator inputs: %s", paste(missing_paths, collapse = ", ")), call. = FALSE)
}

# Reconstruct total deposits from mutually exclusive liquidity components.
deposits_monthly <- readr::read_csv(liquidity_path, show_col_types = FALSE) %>%
  dplyr::filter(.data$variable_id %in% c(
    "monedas_billetes",
    "depositos_vista",
    "depositos_ahorro_transferibles",
    "cuasidinero",
    "liquidez_monetaria"
  )) %>%
  dplyr::select("date", "variable_id", "value", "provisional") %>%
  tidyr::pivot_wider(names_from = "variable_id", values_from = "value") %>%
  dplyr::arrange(.data$date) %>%
  dplyr::mutate(
    deposits_nominal_current_ves = .data$liquidez_monetaria - .data$monedas_billetes,
    deposits_components_current_ves = .data$depositos_vista +
      .data$depositos_ahorro_transferibles + .data$cuasidinero,
    deposits_reconciliation_error = .data$deposits_nominal_current_ves -
      .data$deposits_components_current_ves,
    deposits_reconciliation_relative = .data$deposits_reconciliation_error /
      .data$liquidez_monetaria,
    month = lubridate::floor_date(.data$date, unit = "month")
  ) %>%
  dplyr::group_by(.data$month) %>%
  dplyr::slice_max(order_by = .data$date, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

if (max(abs(deposits_monthly$deposits_reconciliation_relative), na.rm = TRUE) > 1e-4) {
  stop("Deposit components do not reconcile to monetary liquidity less currency.", call. = FALSE)
}

# Extend the original headline index with the grouped release after verifying the overlap.
legacy_cpi <- readr::read_csv(cpi_path, show_col_types = FALSE)
grouped_cpi <- readr::read_csv(grouped_cpi_path, show_col_types = FALSE) %>%
  dplyr::filter(.data$series_id == "headline")
overlap <- readr::read_csv(overlap_path, show_col_types = FALSE)
if (nrow(overlap) == 0L || anyNA(overlap$agrees) || !all(overlap$agrees) ||
    !identical(unique(c(legacy_cpi$unit, grouped_cpi$unit)), "index_dec_2007_100")) {
  rlang::abort("BCV headline CPI releases do not agree on their overlapping base",
    class = "deposit_cpi_overlap_error")
}
cpi <- dplyr::bind_rows(legacy_cpi, grouped_cpi %>%
    dplyr::filter(.data$date > max(legacy_cpi$date))) %>%
  dplyr::select(date, cpi_index, cpi_provisional = provisional, source_dataset_cpi = dataset_id) %>%
  dplyr::arrange(.data$date)

if (anyDuplicated(cpi$date) > 0 || any(cpi$cpi_index <= 0, na.rm = TRUE)) {
  stop("CPI input contains duplicate dates or non-positive values.", call. = FALSE)
}

cpi_reference <- cpi %>%
  dplyr::filter(!is.na(.data$cpi_index)) %>%
  dplyr::slice_max(order_by = .data$date, n = 1, with_ties = FALSE)

deposits_final <- deposits_monthly %>%
  dplyr::left_join(cpi, by = c("month" = "date")) %>%
  dplyr::mutate(
    deposits_real_latest_cpi_ves = .data$deposits_nominal_current_ves *
      cpi_reference$cpi_index[[1]] / .data$cpi_index,
    dataset_id = dataset_id,
    source_id = "bcv",
    date = .data$month,
    frequency = "monthly",
    currency = "VES_current",
    nominal_unit = "current_VES",
    real_unit = paste0("constant_VES_at_", format(cpi_reference$date[[1]], "%Y_%m"), "_prices"),
    cpi_reference_date = cpi_reference$date[[1]],
    cpi_reference_value = cpi_reference$cpi_index[[1]],
    provisional = .data$provisional | dplyr::coalesce(.data$cpi_provisional, FALSE),
    source_dataset_liquidity = "monetary_02_liq_bcv"
  ) %>%
  dplyr::select(dplyr::all_of(c(
    "dataset_id", "source_id", "date", "frequency",
    "deposits_nominal_current_ves", "deposits_real_latest_cpi_ves",
    "cpi_index", "cpi_reference_date", "cpi_reference_value", "currency",
    "nominal_unit", "real_unit", "depositos_vista",
    "depositos_ahorro_transferibles", "cuasidinero", "monedas_billetes",
    "liquidez_monetaria", "deposits_components_current_ves",
    "deposits_reconciliation_error", "deposits_reconciliation_relative", "provisional",
    "source_dataset_liquidity", "source_dataset_cpi"
  ))) %>%
  dplyr::arrange(.data$date)

# Validate keys and economically meaningful values before publishing the series.
if (anyDuplicated(deposits_final$date) > 0) {
  stop("Monthly deposit dataset contains duplicate dates.", call. = FALSE)
}

if (any(deposits_final$deposits_nominal_current_ves <= 0, na.rm = TRUE) ||
    any(deposits_final$deposits_real_latest_cpi_ves <= 0, na.rm = TRUE)) {
  stop("Deposit dataset contains non-positive nominal or real values.", call. = FALSE)
}

real_deposits <- deposits_final %>%
  dplyr::filter(!is.na(.data$deposits_real_latest_cpi_ves))

if (nrow(real_deposits) < 150 || max(real_deposits$date) != cpi_reference$date[[1]]) {
  stop("Real deposit series has insufficient coverage or does not reach the CPI reference date.", call. = FALSE)
}

# Nominal-only months have no deflator source, while every real value must retain one.
if (anyNA(real_deposits$source_dataset_cpi)) {
  stop("Real deposit values are missing CPI source attribution.", call. = FALSE)
}

write_final_dataset(deposits_final, dataset_id)
