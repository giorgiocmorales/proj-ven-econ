# Extract international reserves tables from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "external_07_res_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_externo/2_1_1.xlsx"
raw_path <- file.path("data", "raw", "bcv_reserves.xlsx")

download_latest_file(source_url, raw_path)
downloaded_at <- source_retrieval_time(raw_path, dataset_id)

reserve_sheets <- bcv_excel_sheets(raw_path) %>%
  keep(~ str_detect(.x, "^\\d{4}$"))

reserves_raw <- map_dfr(reserve_sheets, function(sheet_name) {
  sheet <- read_bcv_sheet(raw_path, sheet = sheet_name, col_names = FALSE) %>%
    as_tibble(.name_repair = "unique")

  currency_map <- tibble(
    col_name = names(sheet)[2:min(10, ncol(sheet))],
    currency = rep(c("usd", "eur", "cny"), each = 3, length.out = min(9, ncol(sheet) - 1)),
    component = rep(c("bcv", "fem", "total"), times = 3, length.out = min(9, ncol(sheet) - 1))
  )

  sheet %>%
    mutate(
      row_id = row_number(),
      date = parse_bcv_date(...1),
      provisional = is_bcv_provisional(...1)
    ) %>%
    filter(!is.na(date)) %>%
    # Keep duplicate dates in source order. Row positions shift when BCV adds new days.
    group_by(date) %>%
    mutate(source_date_occurrence = row_number(), source_date_repeated = n() > 1L) %>%
    ungroup() %>%
    select(row_id, date, source_date_occurrence, source_date_repeated,
      provisional, all_of(currency_map$col_name)) %>%
    pivot_longer(
      cols = all_of(currency_map$col_name),
      names_to = "col_name",
      values_to = "value",
      values_transform = list(value = as.character)
    ) %>%
    left_join(currency_map, by = "col_name") %>%
    mutate(
      value = clean_bcv_numeric(value),
      dataset_id = dataset_id,
      source_id = source_id,
      source_url = source_url,
      sheet_name = sheet_name,
      frequency = "daily",
      unit = paste0("million_", currency),
      downloaded_at = downloaded_at
    ) %>%
    filter(!is.na(value))
})

write_final_dataset(reserves_raw, dataset_id)
