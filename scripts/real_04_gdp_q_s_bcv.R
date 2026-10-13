# Extract quarterly GDP sector tables from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_04_gdp_q_s_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/cuentas_macroeconomicas/5_2_1_si_trim.xlsx"
raw_path <- file.path("data", "raw", "bcv_gdp_q_s.xlsx")

downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
download_latest_file(source_url, raw_path)

gdp_sheet <- read_bcv_sheet(raw_path, sheet = "Var_pun%", col_names = FALSE) %>%
  as_tibble(.name_repair = "unique")

series_map <- tibble(
  col_name = names(gdp_sheet)[2:5],
  series_label = c(
    "Total",
    "Sectores Publico",
    "Sectores Privado",
    "Impuestos netos sobre los productos"
  )
)

gdp_q_sector_raw <- gdp_sheet %>%
  mutate(
    row_id = row_number(),
    label = clean_bcv_text(...1),
    year = parse_bcv_year(label),
    quarter = parse_bcv_quarter(label),
    year_provisional = if_else(!is.na(year), is_bcv_provisional(label), NA)
  ) %>%
  tidyr::fill(year, year_provisional) %>%
  filter(!is.na(year), !is.na(quarter)) %>%
  mutate(provisional = coalesce(year_provisional, FALSE)) %>%
  select(row_id, year, quarter, provisional, all_of(series_map$col_name)) %>%
  pivot_longer(
    cols = all_of(series_map$col_name),
    names_to = "col_name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  left_join(series_map, by = "col_name") %>%
  mutate(
    value = clean_bcv_numeric(value),
    date = make_bcv_quarter_date(year, quarter),
    frequency = "quarterly",
    unit = "percent_yoy",
    dataset_id = dataset_id,
    source_id = source_id,
    source_url = source_url,
    sheet_name = "Var_pun%",
    downloaded_at = downloaded_at
  ) %>%
  filter(!is.na(value))

write_processed_dataset(gdp_q_sector_raw, dataset_id)
