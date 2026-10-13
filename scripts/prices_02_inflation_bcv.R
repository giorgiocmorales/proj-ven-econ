# Derive report-ready monthly, annual, and calendar-year inflation from the BCV CPI index.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

cpi_source <- readr::read_csv(
  "data/processed/prices_01_cpi_bcv.csv",
  show_col_types = FALSE
) %>%
  arrange(.data$date)

duplicate_dates <- cpi_source %>%
  count(.data$date) %>%
  filter(.data$n > 1)

if (nrow(duplicate_dates) > 0) {
  stop("BCV CPI source contains duplicate observation dates", call. = FALSE)
}

if (any(is.na(cpi_source$cpi_index))) {
  stop("BCV CPI source contains missing index observations", call. = FALSE)
}

cpi_calculations <- cpi_source %>%
  mutate(
    inflation_mom_calculated = 100 * (.data$cpi_index / lag(.data$cpi_index) - 1),
    inflation_yoy_calculated = 100 * (.data$cpi_index / lag(.data$cpi_index, 12) - 1),
    inflation_calendar_year_calculated = if_else(
      lubridate::month(.data$date) == 12,
      .data$inflation_yoy_calculated,
      NA_real_
    )
  )

published_mom_mismatches <- cpi_calculations %>%
  filter(
    !is.na(.data$mom_var_pct),
    !is.na(.data$inflation_mom_calculated),
    abs(.data$mom_var_pct - .data$inflation_mom_calculated) > 0.2
  )

if (nrow(published_mom_mismatches) > 0) {
  stop(
    "BCV published monthly CPI changes do not reconcile with the CPI index within 0.2 percentage points",
    call. = FALSE
  )
}

inflation_bcv <- bind_rows(
  cpi_calculations %>%
    transmute(
      date = .data$date,
      frequency = "monthly",
      variable_id = "cpi_index",
      display_label = "Consumer price index",
      value = .data$cpi_index,
      unit = "index_dec_2007_100",
      provisional = .data$provisional,
      source_id = .data$source_id,
      dataset_id = "prices_02_inflation_bcv",
      source_dataset_id = .data$dataset_id,
      source_url = .data$source_url,
      transformation = "identity",
      downloaded_at = .data$downloaded_at
    ),
  cpi_calculations %>%
    filter(!is.na(.data$inflation_mom_calculated)) %>%
    transmute(
      date = .data$date,
      frequency = "monthly",
      variable_id = "inflation_mom",
      display_label = "Monthly inflation",
      value = .data$inflation_mom_calculated,
      unit = "percent_mom",
      provisional = .data$provisional,
      source_id = .data$source_id,
      dataset_id = "prices_02_inflation_bcv",
      source_dataset_id = .data$dataset_id,
      source_url = .data$source_url,
      transformation = "percent_change_from_cpi_index",
      downloaded_at = .data$downloaded_at
    ),
  cpi_calculations %>%
    filter(!is.na(.data$inflation_yoy_calculated)) %>%
    transmute(
      date = .data$date,
      frequency = "monthly",
      variable_id = "inflation_yoy",
      display_label = "Annual inflation",
      value = .data$inflation_yoy_calculated,
      unit = "percent_yoy",
      provisional = .data$provisional,
      source_id = .data$source_id,
      dataset_id = "prices_02_inflation_bcv",
      source_dataset_id = .data$dataset_id,
      source_url = .data$source_url,
      transformation = "percent_change_from_cpi_index_lag_12",
      downloaded_at = .data$downloaded_at
    ),
  cpi_calculations %>%
    filter(!is.na(.data$inflation_calendar_year_calculated)) %>%
    transmute(
      date = .data$date,
      frequency = "annual",
      variable_id = "inflation_calendar_year",
      display_label = "Calendar-year inflation",
      value = .data$inflation_calendar_year_calculated,
      unit = "percent_yoy",
      provisional = .data$provisional,
      source_id = .data$source_id,
      dataset_id = "prices_02_inflation_bcv",
      source_dataset_id = .data$dataset_id,
      source_url = .data$source_url,
      transformation = "december_to_december_percent_change",
      downloaded_at = .data$downloaded_at
    )
) %>%
  arrange(.data$variable_id, .data$date)

duplicate_keys <- inflation_bcv %>%
  count(.data$variable_id, .data$date) %>%
  filter(.data$n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("Derived BCV inflation output contains duplicate variable/date keys", call. = FALSE)
}

write_final_dataset(inflation_bcv, "prices_02_inflation_bcv")
