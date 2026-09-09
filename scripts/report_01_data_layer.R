# Assemble validated source extracts into a stable, report-ready long data layer.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

report_registry <- readr::read_csv(
  "registry/report_series.csv",
  show_col_types = FALSE
)

required_registry_columns <- c(
  "variable_id", "section", "display_label", "source_key", "source_file",
  "source_series", "source_frequency", "frequency", "unit", "observation_role",
  "transformation", "source_priority"
)

missing_registry_columns <- setdiff(required_registry_columns, names(report_registry))

if (length(missing_registry_columns) > 0) {
  stop(
    sprintf(
      "Report series registry is missing columns: %s",
      paste(missing_registry_columns, collapse = ", ")
    ),
    call. = FALSE
  )
}

duplicate_registry_rows <- report_registry %>%
  count(.data$source_key, .data$source_series, .data$source_frequency) %>%
  filter(.data$n > 1)

if (nrow(duplicate_registry_rows) > 0) {
  stop("Report series registry contains duplicate source mappings", call. = FALSE)
}

gdp_bcv <- readr::read_csv(
  "data/processed/real_01_gdp_a_a_bcv.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "gdp_bcv",
    source_series = .data$series_label,
    source_frequency = .data$frequency,
    date = .data$date,
    value = .data$value,
    provisional = .data$provisional,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    downloaded_at = as.character(.data$downloaded_at)
  )

gdp_imf <- readr::read_csv(
  "data/processed/real_05_gdp_imf.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "gdp_imf",
    source_series = .data$target_variable,
    source_frequency = "annual",
    date = .data$date,
    value = .data$OBS_VALUE,
    provisional = FALSE,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    downloaded_at = as.character(.data$downloaded_at)
  )

inflation_bcv <- readr::read_csv(
  "data/final/prices_02_inflation_bcv.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "cpi",
    source_series = .data$variable_id,
    source_frequency = .data$frequency,
    date = .data$date,
    value = .data$value,
    provisional = .data$provisional,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    downloaded_at = as.character(.data$downloaded_at)
  )

fx_bcv <- readr::read_csv(
  "data/final/external_fx.csv",
  show_col_types = FALSE
) %>%
  pivot_longer(
    cols = c("rate_indx_adj", "rate_yad"),
    names_to = "source_series",
    values_to = "value"
  ) %>%
  filter(!is.na(.data$value)) %>%
  transmute(
    source_key = "fx",
    source_series = .data$source_series,
    source_frequency = "daily",
    date = .data$date,
    value = .data$value,
    provisional = FALSE,
    source_id = if_else(.data$source_series == "rate_indx_adj", "bcv", "yadio"),
    dataset_id = "external_fx",
    source_url = NA_character_,
    downloaded_at = NA_character_
  )

oil_production <- readr::read_csv(
  "data/final/oil_01_prod_opec_latest.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "oil_production",
    source_series = .data$source_type,
    source_frequency = .data$period_type,
    date = .data$date,
    value = .data$value,
    provisional = FALSE,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    downloaded_at = as.character(.data$downloaded_at)
  )

oil_price <- readr::read_csv(
  "data/final/oil_02_price_opec_latest.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "oil_price",
    source_series = .data$variable_id,
    source_frequency = .data$period_type,
    date = .data$date,
    value = .data$value,
    provisional = FALSE,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    downloaded_at = as.character(.data$downloaded_at)
  )

bop_goods <- readr::read_csv(
  "data/final/external_bop_goods.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "bop_goods",
    source_series = .data$variable_id,
    source_frequency = .data$frequency,
    date = .data$date,
    value = .data$value,
    provisional = .data$provisional,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = NA_character_,
    downloaded_at = as.character(.data$downloaded_at)
  )

fiscal_balance <- readr::read_csv(
  "data/processed/fiscal_02_bal_imf.csv",
  show_col_types = FALSE
) %>%
  transmute(
    source_key = "fiscal_balance",
    source_series = .data$target_variable,
    source_frequency = "annual",
    date = .data$date,
    value = .data$OBS_VALUE,
    provisional = FALSE,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    downloaded_at = as.character(.data$downloaded_at)
  )

source_data <- bind_rows(
  gdp_bcv,
  gdp_imf,
  inflation_bcv,
  fx_bcv,
  oil_production,
  oil_price,
  bop_goods,
  fiscal_balance
)

report_series <- source_data %>%
  inner_join(
    report_registry,
    by = c("source_key", "source_series", "source_frequency")
  ) %>%
  transmute(
    variable_id = .data$variable_id,
    section = .data$section,
    display_label = .data$display_label,
    date = .data$date,
    frequency = .data$frequency,
    value = .data$value,
    unit = .data$unit,
    observation_role = .data$observation_role,
    transformation = .data$transformation,
    source_priority = .data$source_priority,
    provisional = .data$provisional,
    source_id = .data$source_id,
    dataset_id = .data$dataset_id,
    source_url = .data$source_url,
    source_series = .data$source_series,
    source_frequency = .data$source_frequency,
    downloaded_at = .data$downloaded_at
  ) %>%
  arrange(.data$section, .data$variable_id, .data$date)

expected_mappings <- report_registry %>%
  distinct(.data$variable_id)

observed_mappings <- report_series %>%
  distinct(.data$variable_id)

missing_mappings <- anti_join(expected_mappings, observed_mappings, by = "variable_id")

if (nrow(missing_mappings) > 0) {
  stop(
    sprintf(
      "Report data layer is missing mapped variables: %s",
      paste(missing_mappings$variable_id, collapse = ", ")
    ),
    call. = FALSE
  )
}

duplicate_keys <- report_series %>%
  count(.data$variable_id, .data$date, .data$observation_role) %>%
  filter(.data$n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("Report data layer contains duplicate variable/date/role keys", call. = FALSE)
}

if (any(is.na(report_series$value))) {
  stop("Report data layer contains missing values", call. = FALSE)
}

write_final_dataset(report_series, "report_series")
