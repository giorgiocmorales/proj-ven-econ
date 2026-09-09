# Decompose annual multiplier changes into liquidity and monetary-base components.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

# Define the harmonized inputs used by the exact additive decomposition.
dataset_id <- "monetary_06_multiplier_detailed_annual_bcv"
base_interim_path <- "data/interim/monetary_01_mb_bcv.csv"
base_final_path <- "data/final/monetary_01_mb_bcv.csv"
liquidity_path <- "data/final/monetary_02_liq_bcv.csv"
input_paths <- c(base_interim_path, base_final_path, liquidity_path)
missing_paths <- input_paths[!file.exists(input_paths)]

if (length(missing_paths) > 0) {
  stop(sprintf("Missing detailed decomposition inputs: %s", paste(missing_paths, collapse = ", ")), call. = FALSE)
}

# Extract top-level sources of the monetary base from the modern BCV balance.
base_source_components <- readr::read_csv(base_interim_path, show_col_types = FALSE) %>%
  dplyr::mutate(
    component_id = dplyr::case_when(
      .data$series_label_raw == "1.- Activos Externos Netos" ~
        "base_external_assets",
      stringr::str_detect(.data$series_label_raw, "^2\\.1\\.-") ~
        "base_public_sector",
      stringr::str_detect(.data$series_label_raw, "^2\\.2\\.-") ~
        "base_financial_sector",
      stringr::str_detect(.data$series_label_raw, "^2\\.3\\.-") ~
        "base_private_sector",
      stringr::str_detect(.data$series_label_raw, "^2\\.4\\.-") ~
        "base_unclassified_assets",
      stringr::str_detect(.data$series_label_raw, "^2\\.5\\.-") ~
        "base_equity",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(.data$component_id)) %>%
  dplyr::mutate(
    conversion_factor = dplyr::case_when(
      stringr::str_trim(.data$sheet_name) == "II Sem 2010" ~ 100e-6,
      .data$date <= as.Date("2018-12-31") ~ 1e-6,
      .data$date < as.Date("2021-10-01") ~ 1,
      TRUE ~ 1e3
    ),
    value = .data$value_raw * .data$conversion_factor
  ) %>%
  dplyr::select("date", "component_id", "value") %>%
  tidyr::pivot_wider(names_from = "component_id", values_from = "value")

# Combine the two residual internal positions into one readable category.
base_components <- readr::read_csv(base_final_path, show_col_types = FALSE) %>%
  dplyr::select("date", monetary_base = "value") %>%
  dplyr::left_join(base_source_components, by = "date") %>%
  dplyr::mutate(
    base_other_internal = .data$base_unclassified_assets + .data$base_equity
  )

# Aggregate liquidity into two named deposit categories and an exact residual.
liquidity_components <- readr::read_csv(liquidity_path, show_col_types = FALSE) %>%
  dplyr::filter(
    .data$variable_id %in% c(
      "depositos_vista",
      "depositos_ahorro_transferibles",
      "monedas_billetes",
      "cuasidinero",
      "liquidez_monetaria"
    )
  ) %>%
  dplyr::select("date", "variable_id", "value") %>%
  tidyr::pivot_wider(names_from = "variable_id", values_from = "value") %>%
  dplyr::mutate(
    liquidity_other = .data$monedas_billetes + .data$cuasidinero,
    liquidity_reconciliation_error = .data$liquidez_monetaria -
      .data$depositos_vista - .data$depositos_ahorro_transferibles -
      .data$liquidity_other,
    liquidity_reconciliation_relative = .data$liquidity_reconciliation_error /
      .data$liquidez_monetaria
  )

if (max(abs(liquidity_components$liquidity_reconciliation_relative), na.rm = TRUE) > 1e-4) {
  stop("Detailed liquidity components do not reconcile to total liquidity.", call. = FALSE)
}

# Select the first and final weekly observation of each calendar year.
annual_endpoints <- liquidity_components %>%
  dplyr::inner_join(base_components, by = "date") %>%
  dplyr::mutate(year = lubridate::year(.data$date)) %>%
  dplyr::filter(.data$year >= 2001) %>%
  dplyr::group_by(.data$year) %>%
  dplyr::summarise(
    period_start = dplyr::first(.data$date),
    period_end = dplyr::last(.data$date),
    liquidity_start = dplyr::first(.data$liquidez_monetaria),
    liquidity_end = dplyr::last(.data$liquidez_monetaria),
    base_start = dplyr::first(.data$monetary_base),
    base_end = dplyr::last(.data$monetary_base),
    liquidity_demand_start = dplyr::first(.data$depositos_vista),
    liquidity_demand_end = dplyr::last(.data$depositos_vista),
    liquidity_savings_start = dplyr::first(.data$depositos_ahorro_transferibles),
    liquidity_savings_end = dplyr::last(.data$depositos_ahorro_transferibles),
    liquidity_other_start = dplyr::first(.data$liquidity_other),
    liquidity_other_end = dplyr::last(.data$liquidity_other),
    base_external_assets_start = dplyr::first(.data$base_external_assets),
    base_external_assets_end = dplyr::last(.data$base_external_assets),
    base_public_sector_start = dplyr::first(.data$base_public_sector),
    base_public_sector_end = dplyr::last(.data$base_public_sector),
    base_financial_sector_start = dplyr::first(.data$base_financial_sector),
    base_financial_sector_end = dplyr::last(.data$base_financial_sector),
    base_private_sector_start = dplyr::first(.data$base_private_sector),
    base_private_sector_end = dplyr::last(.data$base_private_sector),
    base_other_internal_start = dplyr::first(.data$base_other_internal),
    base_other_internal_end = dplyr::last(.data$base_other_internal),
    liquidity_detail_available = all(!is.na(c(
      dplyr::first(.data$depositos_vista),
      dplyr::last(.data$depositos_vista),
      dplyr::first(.data$depositos_ahorro_transferibles),
      dplyr::last(.data$depositos_ahorro_transferibles),
      dplyr::first(.data$liquidity_other),
      dplyr::last(.data$liquidity_other)
    ))),
    base_detail_available = all(!is.na(c(
      dplyr::first(.data$base_external_assets),
      dplyr::last(.data$base_external_assets),
      dplyr::first(.data$base_public_sector),
      dplyr::last(.data$base_public_sector),
      dplyr::first(.data$base_financial_sector),
      dplyr::last(.data$base_financial_sector),
      dplyr::first(.data$base_private_sector),
      dplyr::last(.data$base_private_sector),
      dplyr::first(.data$base_other_internal),
      dplyr::last(.data$base_other_internal)
    ))),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    liquidity_log_mean = dplyr::if_else(
      abs(.data$liquidity_end - .data$liquidity_start) < 1e-12,
      .data$liquidity_start,
      (.data$liquidity_end - .data$liquidity_start) /
        (log(.data$liquidity_end) - log(.data$liquidity_start))
    ),
    base_log_mean = dplyr::if_else(
      abs(.data$base_end - .data$base_start) < 1e-12,
      .data$base_start,
      (.data$base_end - .data$base_start) /
        (log(.data$base_end) - log(.data$base_start))
    ),
    multiplier_log_change = 100 * (
      log(.data$liquidity_end / .data$liquidity_start) -
        log(.data$base_end / .data$base_start)
    )
  )

# Allocate aggregate log changes exactly using log-mean-scaled component changes.
detailed_decomposition <- annual_endpoints %>%
  dplyr::transmute(
    year = .data$year,
    period_start = .data$period_start,
    period_end = .data$period_end,
    multiplier_log_change = .data$multiplier_log_change,
    liquidity_detail_available = .data$liquidity_detail_available,
    base_detail_available = .data$base_detail_available,
    liquidity_total = 100 *
      (.data$liquidity_end - .data$liquidity_start) / .data$liquidity_log_mean,
    liquidity_demand_deposits = 100 *
      (.data$liquidity_demand_end - .data$liquidity_demand_start) /
      .data$liquidity_log_mean,
    liquidity_transferable_savings = 100 *
      (.data$liquidity_savings_end - .data$liquidity_savings_start) /
      .data$liquidity_log_mean,
    liquidity_other = 100 *
      (.data$liquidity_other_end - .data$liquidity_other_start) /
      .data$liquidity_log_mean,
    base_external_assets = -100 *
      (.data$base_external_assets_end - .data$base_external_assets_start) /
      .data$base_log_mean,
    base_public_sector = -100 *
      (.data$base_public_sector_end - .data$base_public_sector_start) /
      .data$base_log_mean,
    base_financial_sector = -100 *
      (.data$base_financial_sector_end - .data$base_financial_sector_start) /
      .data$base_log_mean,
    base_private_sector = -100 *
      (.data$base_private_sector_end - .data$base_private_sector_start) /
      .data$base_log_mean,
    base_other_internal = -100 *
      (.data$base_other_internal_end - .data$base_other_internal_start) /
      .data$base_log_mean,
    base_total = -100 *
      (.data$base_end - .data$base_start) / .data$base_log_mean
  ) %>%
  tidyr::pivot_longer(
    cols = -c(
      "year", "period_start", "period_end", "multiplier_log_change",
      "liquidity_detail_available", "base_detail_available"
    ),
    names_to = "component_id",
    values_to = "contribution"
  ) %>%
  dplyr::mutate(
    side = dplyr::if_else(
      stringr::str_starts(.data$component_id, "liquidity_"),
      "liquidity",
      "monetary_base"
    ),
    keep_component = dplyr::case_when(
      .data$side == "liquidity" & .data$liquidity_detail_available ~
        .data$component_id != "liquidity_total",
      .data$side == "liquidity" & !.data$liquidity_detail_available ~
        .data$component_id == "liquidity_total",
      .data$side == "monetary_base" & .data$base_detail_available ~
        .data$component_id != "base_total",
      .data$side == "monetary_base" & !.data$base_detail_available ~
        .data$component_id == "base_total",
      TRUE ~ FALSE
    ),
    dataset_id = dataset_id,
    source_id = "bcv",
    frequency = "annual",
    unit = "log_percentage_points",
    provisional = .data$year == max(.data$year)
  ) %>%
  dplyr::filter(.data$keep_component) %>%
  dplyr::select(dplyr::all_of(c(
    "dataset_id", "source_id", "year", "period_start", "period_end",
    "frequency", "side", "component_id", "contribution",
    "multiplier_log_change", "unit", "provisional"
  )))

# Confirm that the selected components reproduce the total multiplier movement.
reconciliation <- detailed_decomposition %>%
  dplyr::group_by(.data$year) %>%
  dplyr::summarise(
    component_sum = sum(.data$contribution),
    multiplier_log_change = dplyr::first(.data$multiplier_log_change),
    residual = .data$multiplier_log_change - .data$component_sum,
    .groups = "drop"
  )

if (max(abs(reconciliation$residual), na.rm = TRUE) > 1e-8) {
  stop("Detailed multiplier decomposition does not reconcile.", call. = FALSE)
}

write_final_dataset(detailed_decomposition, dataset_id)
