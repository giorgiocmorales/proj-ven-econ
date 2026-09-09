# Summarize annual changes in the observed monetary multiplier and its two drivers.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "monetary_04_multiplier_annual_bcv"
multiplier_path <- "data/final/monetary_03_multiplier_bcv.csv"

if (!file.exists(multiplier_path)) {
  stop(sprintf("Missing monetary multiplier input: %s", multiplier_path), call. = FALSE)
}

annual_decomposition <- readr::read_csv(multiplier_path, show_col_types = FALSE) %>%
  dplyr::arrange(.data$date) %>%
  dplyr::mutate(year = lubridate::year(.data$date)) %>%
  dplyr::group_by(.data$year) %>%
  dplyr::summarise(
    period_start = dplyr::first(.data$date),
    period_end = dplyr::last(.data$date),
    observations = dplyr::n(),
    multiplier_start = dplyr::first(.data$value),
    multiplier_end = dplyr::last(.data$value),
    liquidity_start = dplyr::first(.data$liquidity),
    liquidity_end = dplyr::last(.data$liquidity),
    monetary_base_start = dplyr::first(.data$monetary_base),
    monetary_base_end = dplyr::last(.data$monetary_base),
    liquidity_contribution = 100 * log(.data$liquidity_end / .data$liquidity_start),
    base_contribution = -100 * log(.data$monetary_base_end / .data$monetary_base_start),
    multiplier_log_change = 100 * log(.data$multiplier_end / .data$multiplier_start),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    decomposition_residual = .data$multiplier_log_change -
      (.data$liquidity_contribution + .data$base_contribution),
    dataset_id = dataset_id,
    source_id = "bcv",
    frequency = "annual",
    unit = "log_percentage_points",
    provisional = .data$year == max(.data$year)
  ) %>%
  dplyr::select(dplyr::all_of(c(
    "dataset_id", "source_id", "year", "period_start", "period_end", "frequency",
    "observations", "multiplier_start", "multiplier_end", "liquidity_start",
    "liquidity_end", "monetary_base_start", "monetary_base_end",
    "liquidity_contribution", "base_contribution", "multiplier_log_change",
    "decomposition_residual", "unit", "provisional"
  )))

if (max(abs(annual_decomposition$decomposition_residual)) > 1e-8) {
  stop("Annual monetary multiplier decomposition does not reconcile.", call. = FALSE)
}

write_final_dataset(annual_decomposition, dataset_id)
