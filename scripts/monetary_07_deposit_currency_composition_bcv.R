# Estimate the currency composition of banking deposits from harmonized BCV series.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

# Define the auditable inputs and fail explicitly when either is unavailable.
dataset_id <- "monetary_07_deposit_currency_composition_bcv"
liquidity_path <- "data/final/monetary_02_liq_bcv.csv"
financial_path <- "data/final/financial_series.csv"
input_paths <- c(liquidity_path, financial_path)
missing_paths <- input_paths[!file.exists(input_paths)]

if (length(missing_paths) > 0) {
  stop(
    sprintf("Missing deposit-composition inputs: %s", paste(missing_paths, collapse = ", ")),
    call. = FALSE
  )
}

# Reconstruct deposits in bolivars from the monetary-liquidity components.
liquidity_deposits <- readr::read_csv(liquidity_path, show_col_types = FALSE) %>%
  dplyr::filter(.data$variable_id %in% c(
    "depositos_vista",
    "depositos_ahorro_transferibles"
  )) %>%
  dplyr::select("date", "variable_id", "value", "provisional") %>%
  tidyr::pivot_wider(
    names_from = "variable_id",
    values_from = c("value", "provisional")
  ) %>%
  dplyr::transmute(
    date = .data$date,
    liquidity_demand_deposits_current_ves = .data$value_depositos_vista,
    liquidity_savings_deposits_current_ves = dplyr::coalesce(
      .data$value_depositos_ahorro_transferibles,
      0
    ),
    deposits_in_ves_current_ves = .data$value_depositos_vista + dplyr::coalesce(
      .data$value_depositos_ahorro_transferibles,
      0
    ),
    liquidity_provisional = dplyr::coalesce(.data$provisional_depositos_vista, FALSE) |
      dplyr::coalesce(.data$provisional_depositos_ahorro_transferibles, FALSE)
  )

# Sum demand and savings deposits reported by the comparable banking groups.
banking_deposits <- readr::read_csv(financial_path, show_col_types = FALSE) %>%
  dplyr::filter(
    .data$dataset_family == "balances",
    .data$institution_id %in% c(
      "universal_banks",
      "commercial_banks",
      "microfinance_banks"
    ),
    .data$variable_id %in% c("demand_deposits", "savings_deposits")
  ) %>%
  dplyr::group_by(.data$date, .data$variable_id) %>%
  dplyr::summarise(
    value = sum(.data$value),
    provisional = any(.data$provisional),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = "variable_id",
    values_from = c("value", "provisional")
  ) %>%
  dplyr::transmute(
    date = .data$date,
    banking_demand_deposits_current_ves = .data$value_demand_deposits,
    banking_savings_deposits_current_ves = .data$value_savings_deposits,
    banking_deposits_current_ves = .data$value_demand_deposits + .data$value_savings_deposits,
    banking_provisional = dplyr::coalesce(.data$provisional_demand_deposits, FALSE) |
      dplyr::coalesce(.data$provisional_savings_deposits, FALSE)
  )

# Treat the excess of banking deposits over liquidity deposits as foreign-currency deposits.
deposit_composition <- banking_deposits %>%
  dplyr::inner_join(liquidity_deposits, by = "date") %>%
  dplyr::filter(.data$date >= as.Date("2018-01-01")) %>%
  dplyr::mutate(
    deposits_in_foreign_currency_est_current_ves = .data$banking_deposits_current_ves -
      .data$deposits_in_ves_current_ves,
    deposits_in_ves_share = .data$deposits_in_ves_current_ves /
      .data$banking_deposits_current_ves,
    deposits_in_foreign_currency_est_share =
      .data$deposits_in_foreign_currency_est_current_ves /
      .data$banking_deposits_current_ves,
    reconciliation_error = .data$banking_deposits_current_ves -
      .data$deposits_in_ves_current_ves -
      .data$deposits_in_foreign_currency_est_current_ves,
    dataset_id = dataset_id,
    source_id = "bcv",
    frequency = "weekly",
    currency = "VES_current",
    unit = "current_VES",
    provisional = .data$banking_provisional | .data$liquidity_provisional,
    method = "banking_demand_and_savings_less_liquidity_demand_and_transferable_savings",
    source_dataset_liquidity = "monetary_02_liq_bcv",
    source_dataset_financial = "financial_series"
  ) %>%
  dplyr::select(dplyr::all_of(c(
    "dataset_id", "source_id", "date", "frequency",
    "banking_deposits_current_ves", "deposits_in_ves_current_ves",
    "deposits_in_foreign_currency_est_current_ves", "deposits_in_ves_share",
    "deposits_in_foreign_currency_est_share",
    "banking_demand_deposits_current_ves", "banking_savings_deposits_current_ves",
    "liquidity_demand_deposits_current_ves", "liquidity_savings_deposits_current_ves",
    "reconciliation_error", "currency", "unit", "provisional", "method",
    "source_dataset_liquidity", "source_dataset_financial"
  ))) %>%
  dplyr::arrange(.data$date)

# Validate the residual estimate and its exact accounting reconciliation.
if (anyDuplicated(deposit_composition$date) > 0) {
  stop("Deposit-composition dataset contains duplicate dates.", call. = FALSE)
}

if (any(deposit_composition$deposits_in_foreign_currency_est_current_ves < 0, na.rm = TRUE)) {
  stop("Foreign-currency deposit estimate is negative within the analytical period.", call. = FALSE)
}

if (max(abs(deposit_composition$reconciliation_error), na.rm = TRUE) > 1e-6) {
  stop("Deposit currency components do not reconcile to banking deposits.", call. = FALSE)
}

if (nrow(deposit_composition) < 400) {
  stop("Deposit-composition dataset has insufficient weekly coverage.", call. = FALSE)
}

write_final_dataset(deposit_composition, dataset_id)
