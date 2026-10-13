# Extract and harmonize the weekly BCV monetary base.

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "monetary_01_mb_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_monetario/base_monetaria_semanal.xls"
raw_path <- file.path("data", "raw", "bcv_mb.xls")
offline <- identical(Sys.getenv("BCV_MONETARY_OFFLINE"), "true")
# An offline rebuild retains the raw workbook's retrieval time instead of inventing one.
downloaded_at <- format(if (offline) file.info(raw_path)$mtime else Sys.time(), "%Y-%m-%d %H:%M:%S")

if (!offline) download_latest_file(
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

monetary_base_interim <- bcv_excel_sheets(raw_path) %>%
  purrr::map_dfr(function(sheet_name) {
    sheet <- read_bcv_sheet(
      file_path = raw_path,
      sheet = sheet_name,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    ) %>%
      tibble::as_tibble(.name_repair = "minimal")

    names(sheet) <- sprintf("col_%02d", seq_len(ncol(sheet)))
    sheet <- sheet %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    if (nrow(sheet) < 8 || ncol(sheet) < 2) {
      stop(sprintf("Unexpected monetary-base sheet structure: %s", sheet_name), call. = FALSE)
    }

    date_map <- tibble::tibble(
      column_name = names(sheet)[-1],
      column_id = seq.int(2L, ncol(sheet)),
      date_cell_raw = unlist(sheet[6, -1], use.names = FALSE)
    ) %>%
      dplyr::mutate(
        date = parse_bcv_date(.data$date_cell_raw),
        provisional = is_bcv_provisional(.data$date_cell_raw)
      ) %>%
      dplyr::filter(!is.na(.data$date))

    if (nrow(date_map) == 0) {
      stop(sprintf("No weekly dates found in monetary-base sheet: %s", sheet_name), call. = FALSE)
    }

    source_unit_label <- clean_bcv_text(sheet$col_01[[2]])

    sheet %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        series_label_raw = clean_bcv_text(.data$col_01)
      ) %>%
      dplyr::filter(.data$row_id > 6, !is.na(.data$series_label_raw)) %>%
      dplyr::select("row_id", "series_label_raw", dplyr::all_of(date_map$column_name)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(date_map$column_name),
        names_to = "column_name",
        values_to = "value_cell_raw"
      ) %>%
      dplyr::left_join(date_map, by = "column_name") %>%
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
      dplyr::filter(!is.na(.data$date), !is.na(.data$value_raw)) %>%
      dplyr::select(dplyr::all_of(c(
        "dataset_id", "source_id", "date", "frequency", "series_label_raw",
        "value_cell_raw", "value_raw", "provisional", "sheet_name", "row_id",
        "column_id", "date_cell_raw", "source_unit_label", "source_url",
        "source_file", "downloaded_at"
      )))
  })

if (nrow(monetary_base_interim) == 0) {
  stop("BCV monetary-base extraction returned no observations.", call. = FALSE)
}

write_interim_dataset(monetary_base_interim, dataset_id)

source_totals <- monetary_base_interim %>%
  dplyr::filter(.data$series_label_raw %in% c("FUENTES", "FUENTES: 1/")) %>%
  dplyr::select("date", sources_total_raw = "value_raw")

# The workbook re-expresses 2001-2018 in individual 2018 VES, 2019-Sep 2021
# in millions of 2018 VES, and Oct 2021 onward in thousands of current VES.
monetary_base_final <- monetary_base_interim %>%
  dplyr::filter(.data$series_label_raw == "USOS") %>%
  dplyr::left_join(source_totals, by = "date") %>%
  dplyr::mutate(
    source_currency = dplyr::case_when(
      .data$date < as.Date("2021-10-01") ~ "VES_2018",
      TRUE ~ "VES_current"
    ),
    source_unit = dplyr::case_when(
      .data$date <= as.Date("2018-12-31") ~ "VES_2018",
      .data$date < as.Date("2021-10-01") ~ "million_VES_2018",
      TRUE ~ "thousand_current_VES"
    ),
    unit_multiplier = dplyr::case_when(
      .data$date <= as.Date("2018-12-31") ~ 1,
      .data$date < as.Date("2021-10-01") ~ 1e6,
      TRUE ~ 1e3
    ),
    currency_conversion_factor = dplyr::case_when(
      .data$date < as.Date("2021-10-01") ~ 1e-6,
      TRUE ~ 1
    ),
    # The second-half 2010 worksheet is published at 1/100 of the adjacent
    # worksheets despite carrying the same unit label; retain raw values and
    # expose the continuity correction separately for audit.
    source_scale_correction = dplyr::if_else(
      stringr::str_trim(.data$sheet_name) == "II Sem 2010",
      100,
      1
    ),
    conversion_factor = .data$unit_multiplier * .data$currency_conversion_factor *
      .data$source_scale_correction,
    value = .data$value_raw * .data$conversion_factor,
    sources_total_value = .data$sources_total_raw * .data$conversion_factor,
    sources_uses_relative_difference = abs(.data$value_raw - .data$sources_total_raw) /
      pmax(abs(.data$value_raw), 1),
    balance_identity_flag = dplyr::case_when(
      is.na(.data$sources_total_raw) ~ "not_available",
      .data$sources_uses_relative_difference <= 1e-4 ~ "pass",
      TRUE ~ "review"
    ),
    variable_id = "monetary_base",
    currency = "VES_current",
    unit = "current_VES"
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
    "source_scale_correction", "conversion_factor", "observation_currency_regime",
    "observation_currency_to_current_factor", "sources_total_raw",
    "sources_total_value", "sources_uses_relative_difference",
    "balance_identity_flag", "provisional", "sheet_name", "row_id", "column_id",
    "date_cell_raw", "source_unit_label", "source_url", "source_file",
    "downloaded_at"
  ))) %>%
  dplyr::arrange(.data$date)

duplicate_dates <- monetary_base_final %>%
  dplyr::count(.data$date) %>%
  dplyr::filter(.data$n > 1)

if (nrow(duplicate_dates) > 0) {
  stop("Final monetary-base series contains duplicate dates.", call. = FALSE)
}

if (any(is.na(monetary_base_final$value)) || any(monetary_base_final$value < 0)) {
  stop("Final monetary-base series contains missing or negative values.", call. = FALSE)
}

write_final_dataset(monetary_base_final, dataset_id)
