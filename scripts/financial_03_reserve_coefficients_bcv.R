# Extract monthly BCV statutory reserve coefficients by institution group.

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
source("R/financial_helpers.R")

dataset_id <- "financial_02_reserve_coefficients_bcv"

source_row <- readr::read_csv(
  "registry/financial_bcv_sources.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(.data$dataset_family == "reserve_coefficients")

if (nrow(source_row) != 1) {
  stop(sprintf("Expected one reserve-coefficient source; found %s", nrow(source_row)), call. = FALSE)
}

raw_file <- source_row$raw_file[[1]]

if (!file.exists(raw_file)) {
  stop(sprintf("Missing raw financial workbook: %s", raw_file), call. = FALSE)
}

coefficients <- purrr::map_dfr(bcv_excel_sheets(raw_file), function(sheet_name) {
  sheet <- read_bcv_sheet(
    file_path = raw_file,
    sheet = sheet_name,
    col_names = FALSE,
    .name_repair = "minimal"
  ) %>%
    tibble::as_tibble(.name_repair = "minimal")

  names(sheet) <- sprintf("col_%02d", seq_len(ncol(sheet)))
  sheet <- sheet %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  is_historical <- stringr::str_detect(
    financial_ascii(sheet_name),
    "historico"
  )

  column_specs <- if (is_historical) {
    tibble::tribble(
      ~column_id, ~institution_id, ~series_label_raw,
      2L, "commercial_and_universal_banks", "ordinary",
      3L, "commercial_and_universal_banks", "marginal",
      4L, "commercial_and_universal_banks", "special",
      6L, "savings_and_loan_entities", "ordinary",
      7L, "savings_and_loan_entities", "marginal",
      8L, "savings_and_loan_entities", "special",
      10L, "other_financial_institutions", "ordinary",
      11L, "other_financial_institutions", "marginal",
      12L, "other_financial_institutions", "special"
    )
  } else {
    tibble::tribble(
      ~column_id, ~institution_id, ~series_label_raw,
      2L, "commercial_and_universal_banks", "ordinary",
      3L, "commercial_and_universal_banks", "marginal",
      4L, "commercial_and_universal_banks", "special",
      6L, "other_financial_institutions", "ordinary",
      7L, "other_financial_institutions", "marginal",
      8L, "other_financial_institutions", "special"
    )
  }

  available_specs <- column_specs %>%
    dplyr::filter(.data$column_id <= ncol(sheet)) %>%
    dplyr::mutate(column_name = names(sheet)[.data$column_id])

  observations <- sheet %>%
    dplyr::mutate(
      row_id = dplyr::row_number(),
      date = financial_parse_month_rows(.data$col_01)
    ) %>%
    dplyr::filter(!is.na(.data$date)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(available_specs$column_name),
      names_to = "column_name",
      values_to = "value_cell_raw"
    ) %>%
    dplyr::left_join(available_specs, by = "column_name") %>%
    dplyr::mutate(
      value_raw = clean_bcv_numeric(.data$value_cell_raw),
      column_id = as.integer(.data$column_id),
      sheet_name = sheet_name,
      currency_group = "not_applicable",
      frequency = "monthly",
      provisional = FALSE
    ) %>%
    dplyr::filter(!is.na(.data$value_raw))

  sheet_source_row <- source_row
  sheet_source_row$priority <- source_row$priority + dplyr::if_else(is_historical, 0, 1)
  sheet_source_row$vintage_label <- sheet_name

  financial_attach_metadata(
    observations = observations,
    source_row = sheet_source_row,
    dataset_id = dataset_id
  )
})

if (nrow(coefficients) == 0) {
  stop("BCV reserve-coefficient extraction returned no observations.", call. = FALSE)
}

coefficients_interim <- coefficients %>%
  dplyr::select(
    .data$dataset_id,
    .data$dataset_family,
    .data$source_id,
    .data$date,
    .data$frequency,
    .data$institution_id,
    .data$variable_id,
    .data$series_label_raw,
    .data$value_cell_raw,
    .data$value_raw,
    .data$currency_group,
    .data$currency_raw,
    .data$unit_raw,
    .data$conversion_factor,
    target_currency = .data$currency,
    target_unit = .data$unit,
    .data$provisional,
    .data$sheet_name,
    .data$row_id,
    .data$column_id,
    .data$source_url,
    .data$source_file,
    .data$source_vintage,
    .data$source_priority,
    .data$downloaded_at
  )

write_interim_dataset(coefficients_interim, dataset_id)
