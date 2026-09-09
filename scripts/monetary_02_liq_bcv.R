# Extract and harmonize weekly BCV monetary liquidity and its components.

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "monetary_02_liq_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_monetario/liquidez_monetaria_semanal1.xls"
raw_path <- file.path("data", "raw", "bcv_liq.xls")
downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

download_latest_file(
  url = source_url,
  dest_path = raw_path,
  max_attempts = 3,
  timeout_seconds = 120
)

currency_regimes <- readr::read_csv(
  "registry/venezuela_currency_scales.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    currency_code = readr::col_character(),
    effective_start = readr::col_date(),
    effective_end = readr::col_date(),
    zeros_removed_at_start = readr::col_integer(),
    cumulative_zeros_to_current = readr::col_integer(),
    current_ves_per_currency_unit = readr::col_double(),
    notes = readr::col_character()
  )
)

liquidity_sheets <- bcv_excel_sheets(raw_path) %>%
  purrr::discard(~ stringr::str_detect(.x, stringr::regex("module", ignore_case = TRUE)))

liquidity_interim <- liquidity_sheets %>%
  purrr::map_dfr(function(sheet_name) {
    sheet <- read_bcv_sheet(
      file_path = raw_path,
      sheet = sheet_name,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ) %>%
      tibble::as_tibble(.name_repair = "minimal")

    if (ncol(sheet) < 8 || nrow(sheet) < 7) {
      stop(sprintf("Unexpected liquidity sheet structure: %s", sheet_name), call. = FALSE)
    }

    names(sheet) <- sprintf("col_%02d", seq_len(ncol(sheet)))
    source_unit_label <- clean_bcv_text(sheet$col_01[[2]])

    sheet %>%
      dplyr::transmute(
        row_id = dplyr::row_number(),
        date_cell_raw = .data$col_01,
        date = parse_bcv_date(.data$date_cell_raw),
        provisional = is_bcv_provisional(.data$date_cell_raw),
        monedas_billetes = .data$col_02,
        depositos_vista = .data$col_03,
        depositos_ahorro_transferibles = .data$col_04,
        dinero = .data$col_05,
        cuasidinero = .data$col_06,
        liquidez_monetaria = .data$col_07,
        variacion_pct = .data$col_08
      ) %>%
      dplyr::filter(!is.na(.data$date)) %>%
      tidyr::pivot_longer(
        cols = -c("row_id", "date_cell_raw", "date", "provisional"),
        names_to = "variable_id",
        values_to = "value_cell_raw"
      ) %>%
      dplyr::mutate(
        value_raw = clean_bcv_numeric(.data$value_cell_raw),
        dataset_id = dataset_id,
        source_id = source_id,
        source_url = source_url,
        source_file = raw_path,
        sheet_name = sheet_name,
        source_unit_label = source_unit_label,
        frequency = "weekly",
        downloaded_at = downloaded_at
      ) %>%
      dplyr::filter(!is.na(.data$value_raw)) %>%
      dplyr::select(dplyr::all_of(c(
        "dataset_id", "source_id", "date", "frequency", "variable_id",
        "value_cell_raw", "value_raw", "provisional", "sheet_name", "row_id",
        "date_cell_raw", "source_unit_label", "source_url", "source_file",
        "downloaded_at"
      )))
  })

if (nrow(liquidity_interim) == 0) {
  stop("BCV liquidity extraction returned no observations.", call. = FALSE)
}

write_interim_dataset(liquidity_interim, dataset_id)

liquidity_final <- liquidity_interim %>%
  dplyr::mutate(
    source_currency = dplyr::case_when(
      .data$variable_id == "variacion_pct" ~ "not_applicable",
      .data$date < as.Date("2021-10-01") ~ "VES_2018",
      TRUE ~ "VES_current"
    ),
    source_unit = dplyr::case_when(
      .data$variable_id == "variacion_pct" ~ "percent",
      .data$date <= as.Date("2018-12-31") ~ "VES_2018",
      .data$date < as.Date("2021-10-01") ~ "million_VES_2018",
      TRUE ~ "thousand_current_VES"
    ),
    unit_multiplier = dplyr::case_when(
      .data$variable_id == "variacion_pct" ~ 1,
      .data$date <= as.Date("2018-12-31") ~ 1,
      .data$date < as.Date("2021-10-01") ~ 1e6,
      TRUE ~ 1e3
    ),
    currency_conversion_factor = dplyr::case_when(
      .data$variable_id == "variacion_pct" ~ 1,
      .data$date < as.Date("2021-10-01") ~ 1e-6,
      TRUE ~ 1
    ),
    conversion_factor = .data$unit_multiplier * .data$currency_conversion_factor,
    value = .data$value_raw * .data$conversion_factor,
    currency = dplyr::if_else(.data$variable_id == "variacion_pct", "not_applicable", "VES_current"),
    unit = dplyr::if_else(.data$variable_id == "variacion_pct", "percent", "current_VES")
  ) %>%
  tidyr::crossing(currency_regimes) %>%
  dplyr::filter(
    .data$date >= .data$effective_start,
    is.na(.data$effective_end) | .data$date <= .data$effective_end
  ) %>%
  dplyr::rename(
    observation_currency_regime = currency_code,
    observation_currency_to_current_factor = current_ves_per_currency_unit
  ) %>%
  dplyr::select(dplyr::all_of(c(
    "dataset_id", "source_id", "date", "frequency", "variable_id", "value",
    "currency", "unit", "value_cell_raw", "value_raw", "source_currency",
    "source_unit", "unit_multiplier", "currency_conversion_factor",
    "conversion_factor", "observation_currency_regime",
    "observation_currency_to_current_factor", "provisional", "sheet_name",
    "row_id", "date_cell_raw", "source_unit_label", "source_url", "source_file",
    "downloaded_at"
  ))) %>%
  dplyr::arrange(.data$variable_id, .data$date)

duplicate_keys <- liquidity_final %>%
  dplyr::count(.data$variable_id, .data$date) %>%
  dplyr::filter(.data$n > 1)

if (nrow(duplicate_keys) > 0) {
  stop("Final liquidity dataset contains duplicate variable-date keys.", call. = FALSE)
}

if (any(is.na(liquidity_final$value))) {
  stop("Final liquidity dataset contains missing values.", call. = FALSE)
}

write_final_dataset(liquidity_final, dataset_id)
