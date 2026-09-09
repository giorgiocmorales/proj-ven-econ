# Extract weekly and monthly BCV reserve-position series with source-unit metadata.

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
source("R/financial_helpers.R")

dataset_id <- "financial_03_reserve_positions_bcv"

source_registry <- readr::read_csv(
  "registry/financial_bcv_sources.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(.data$dataset_family == "reserve_positions")

# Extract a weekly reserve-position source using its stable BCV column layout.
extract_weekly_source <- function(source_row, dataset_id) {
  raw_file <- source_row$raw_file[[1]]
  parser_profile <- source_row$parser_profile[[1]]

  if (!file.exists(raw_file)) {
    stop(sprintf("Missing raw financial workbook: %s", raw_file), call. = FALSE)
  }

  column_specs <- tibble::tribble(
    ~column_id, ~currency_group, ~series_label_raw,
    2L, "national", "required reserves",
    3L, "national", "current account balance",
    4L, "national", "excess reserves"
  )

  if (parser_profile == "reserve_positions_weekly_dual") {
    column_specs <- dplyr::bind_rows(
      column_specs,
      tibble::tribble(
        ~column_id, ~currency_group, ~series_label_raw,
        6L, "foreign", "required reserves",
        7L, "foreign", "current account balance",
        8L, "foreign", "excess reserves"
      )
    )
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

    available_specs <- column_specs %>%
      dplyr::filter(.data$column_id <= ncol(sheet)) %>%
      dplyr::mutate(column_name = names(sheet)[.data$column_id])

    observations <- sheet %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        date_raw = .data$col_01,
        date = parse_bcv_date(.data$date_raw),
        provisional = is_bcv_provisional(.data$date_raw)
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
        sheet_name = sheet_name,
        institution_id = institution_id,
        frequency = "weekly"
      ) %>%
      dplyr::filter(!is.na(.data$value_raw))

    financial_attach_metadata(
      observations = observations,
      source_row = source_row,
      dataset_id = dataset_id
    )
  })
}

# Identify the column containing Spanish month names in a monthly BCV sheet.
find_month_column <- function(sheet, raw_file, sheet_name) {
  month_keys <- c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "setiembre", "octubre",
    "noviembre", "diciembre"
  )
  month_counts <- purrr::map_int(sheet, function(column) {
    sum(financial_ascii(column) %in% month_keys, na.rm = TRUE)
  })

  if (max(month_counts) == 0) {
    stop(
      sprintf("Could not identify a monthly date column in %s / %s", basename(raw_file), sheet_name),
      call. = FALSE
    )
  }

  which.max(month_counts)
}

# Return source-column mappings for each historical monthly layout.
monthly_column_specs <- function(parser_profile, date_column_id) {
  if (parser_profile == "reserve_positions_monthly_current") {
    return(tibble::tribble(
      ~column_id, ~currency_group, ~series_label_raw,
      date_column_id + 2L, "national", "current account balance",
      date_column_id + 4L, "national", "required reserves",
      date_column_id + 6L, "national", "excess reserves"
    ))
  }

  if (parser_profile == "reserve_positions_monthly_historical") {
    return(tibble::tribble(
      ~column_id, ~currency_group, ~series_label_raw,
      date_column_id + 1L, "national", "ordinary required reserves",
      date_column_id + 2L, "national", "marginal required reserves",
      date_column_id + 3L, "national", "special required reserves",
      date_column_id + 4L, "national", "total",
      date_column_id + 5L, "national", "current account balance",
      date_column_id + 6L, "national", "excess reserves"
    ))
  }

  if (parser_profile == "reserve_positions_monthly_dual") {
    return(tibble::tribble(
      ~column_id, ~currency_group, ~series_label_raw,
      2L, "national", "ordinary required reserves",
      3L, "national", "marginal required reserves",
      4L, "national", "special required reserves",
      5L, "national", "total",
      7L, "national", "current account balance",
      8L, "national", "source column 08",
      9L, "national", "excess reserves",
      11L, "foreign", "required reserves",
      12L, "foreign", "source column 12",
      13L, "foreign", "current account balance",
      15L, "foreign", "excess reserves"
    ))
  }

  stop(sprintf("Unsupported monthly reserve-position parser profile: %s", parser_profile), call. = FALSE)
}

# Extract a monthly reserve-position source with year headings and month rows.
extract_monthly_source <- function(source_row, dataset_id) {
  raw_file <- source_row$raw_file[[1]]
  parser_profile <- source_row$parser_profile[[1]]

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
    date_column_id <- find_month_column(sheet, raw_file, sheet_name)
    date_column_name <- names(sheet)[date_column_id]
    column_specs <- monthly_column_specs(parser_profile, date_column_id)
    available_specs <- column_specs %>%
      dplyr::filter(.data$column_id <= ncol(sheet)) %>%
      dplyr::mutate(column_name = names(sheet)[.data$column_id])

    observations <- sheet %>%
      dplyr::mutate(
        row_id = dplyr::row_number(),
        date = financial_parse_month_rows(.data[[date_column_name]])
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
        sheet_name = sheet_name,
        institution_id = institution_id,
        frequency = "monthly",
        provisional = FALSE
      ) %>%
      dplyr::filter(!is.na(.data$value_raw))

    financial_attach_metadata(
      observations = observations,
      source_row = source_row,
      dataset_id = dataset_id
    )
  })
}

reserve_positions <- purrr::pmap_dfr(source_registry, function(...) {
  source_row <- tibble::as_tibble(list(...))

  if (source_row$frequency[[1]] == "weekly") {
    return(extract_weekly_source(source_row, dataset_id))
  }

  if (source_row$frequency[[1]] == "monthly") {
    return(extract_monthly_source(source_row, dataset_id))
  }

  stop(
    sprintf("Unsupported reserve-position frequency: %s", source_row$frequency[[1]]),
    call. = FALSE
  )
})

if (nrow(reserve_positions) == 0) {
  stop("BCV reserve-position extraction returned no observations.", call. = FALSE)
}

reserve_positions_interim <- reserve_positions %>%
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

write_interim_dataset(reserve_positions_interim, dataset_id)
