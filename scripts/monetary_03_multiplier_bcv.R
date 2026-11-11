# Build the weekly observed monetary multiplier from harmonized BCV aggregates.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "monetary_03_multiplier_bcv"
base_path <- "data/final/monetary_01_mb_bcv.csv"
liquidity_path <- "data/final/monetary_02_liq_bcv.csv"

missing_paths <- c(base_path, liquidity_path)[!file.exists(c(base_path, liquidity_path))]

if (length(missing_paths) > 0) {
  stop(sprintf("Missing harmonized monetary inputs: %s", paste(missing_paths, collapse = ", ")), call. = FALSE)
}

monetary_base <- readr::read_csv(base_path, show_col_types = FALSE) %>%
  dplyr::select(
    "date",
    monetary_base = "value",
    monetary_base_raw = "value_raw",
    base_provisional = "provisional",
    base_source_unit = "source_unit"
  )

liquidity <- readr::read_csv(liquidity_path, show_col_types = FALSE) %>%
  dplyr::filter(.data$variable_id == "liquidez_monetaria") %>%
  dplyr::select(
    "date",
    liquidity = "value",
    liquidity_raw = "value_raw",
    liquidity_provisional = "provisional",
    liquidity_source_unit = "source_unit"
  )

multiplier <- liquidity %>%
  dplyr::inner_join(monetary_base, by = "date") %>%
  dplyr::arrange(.data$date) %>%
  dplyr::mutate(
    value = .data$liquidity / .data$monetary_base,
    liquidity_log_change = 100 * (log(.data$liquidity) - log(dplyr::lag(.data$liquidity))),
    base_log_change = 100 * (log(.data$monetary_base) - log(dplyr::lag(.data$monetary_base))),
    multiplier_log_change = 100 * (log(.data$value) - log(dplyr::lag(.data$value))),
    decomposition_residual = .data$multiplier_log_change -
      (.data$liquidity_log_change - .data$base_log_change),
    provisional = .data$liquidity_provisional | .data$base_provisional,
    dataset_id = dataset_id,
    variable_id = "observed_monetary_multiplier",
    source_id = "bcv",
    frequency = "weekly",
    unit = "ratio",
    numerator_variable_id = "liquidez_monetaria",
    denominator_variable_id = "monetary_base",
    source_dataset_liquidity = "monetary_02_liq_bcv",
    source_dataset_base = "monetary_01_mb_bcv"
  ) %>%
  dplyr::select(dplyr::all_of(c(
    "dataset_id", "source_id", "date", "frequency", "variable_id", "value",
    "unit", "liquidity", "monetary_base", "liquidity_raw", "monetary_base_raw",
    "liquidity_source_unit", "base_source_unit", "liquidity_log_change",
    "base_log_change", "multiplier_log_change", "decomposition_residual",
    "provisional", "numerator_variable_id", "denominator_variable_id",
    "source_dataset_liquidity", "source_dataset_base"
  )))

if (nrow(multiplier) == 0) {
  stop("Monetary multiplier join returned no observations.", call. = FALSE)
}

if (anyDuplicated(multiplier$date) > 0) {
  stop("Monetary multiplier contains duplicate dates.", call. = FALSE)
}

if (any(is.na(multiplier$value)) || any(multiplier$value <= 0)) {
  stop("Monetary multiplier contains missing or non-positive values.", call. = FALSE)
}

residual_values <- multiplier$decomposition_residual[!is.na(multiplier$decomposition_residual)]

if (length(residual_values) == 0 || max(abs(residual_values)) > 1e-8) {
  stop("Weekly monetary multiplier decomposition does not reconcile.", call. = FALSE)
}

write_final_dataset(multiplier, dataset_id)
