# Extract weekly BCV banking balance-sheet series while preserving published values.

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
source("R/financial_helpers.R")

dataset_id <- "financial_01_balances_bcv"

source_registry <- readr::read_csv(
  "registry/financial_bcv_sources.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(.data$dataset_family == "balances")

balances <- purrr::pmap_dfr(source_registry, function(...) {
  source_row <- tibble::as_tibble(list(...))
  raw_file <- source_row$raw_file[[1]]

  if (!file.exists(raw_file)) {
    stop(sprintf("Missing raw financial workbook: %s", raw_file), call. = FALSE)
  }

  purrr::map_dfr(bcv_excel_sheets(raw_file), function(sheet_name) {
    institution_id <- financial_institution_id(sheet_name)

    if (is.na(institution_id)) {
      return(tibble::tibble())
    }

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

    header_row <- which(apply(sheet, 1, function(row) {
      any(stringr::str_detect(
        dplyr::coalesce(as.character(row), ""),
        stringr::regex("semana al", ignore_case = TRUE)
      ))
    }))

    if (length(header_row) != 1) {
      stop(
        sprintf("Expected one weekly header in %s / %s; found %s", basename(raw_file), sheet_name, length(header_row)),
        call. = FALSE
      )
    }

    series_map <- tibble::tibble(
      column_name = names(sheet),
      column_id = seq_len(ncol(sheet)),
      series_label_raw = clean_bcv_text(unlist(sheet[header_row, ], use.names = FALSE))
    ) %>%
      dplyr::filter(.data$column_id > 1, !is.na(.data$series_label_raw))

    observations <- sheet %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        date_raw = .data$col_01,
        date = parse_bcv_date(.data$date_raw),
        provisional = is_bcv_provisional(.data$date_raw)
      ) %>%
      dplyr::filter(!is.na(.data$date)) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(series_map$column_name),
        names_to = "column_name",
        values_to = "value_cell_raw"
      ) %>%
      dplyr::left_join(series_map, by = "column_name") %>%
      dplyr::mutate(
        value_raw = clean_bcv_numeric(.data$value_cell_raw),
        sheet_name = sheet_name,
        institution_id = institution_id,
        currency_group = "national",
        frequency = source_row$frequency[[1]]
      ) %>%
      dplyr::filter(!is.na(.data$value_raw))

    financial_attach_metadata(
      observations = observations,
      source_row = source_row,
      dataset_id = dataset_id
    )
  })
})

if (nrow(balances) == 0) {
  stop("BCV balance extraction returned no observations.", call. = FALSE)
}

balances_interim <- balances %>%
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

write_interim_dataset(balances_interim, dataset_id)
