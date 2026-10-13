# Extract annual GDP sector tables from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_02_gdp_a_s_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/cuentas_macroeconomicas/5_2_1_si_anual.xlsx"
raw_path <- file.path("data", "raw", "bcv_gdp_a_s.xlsx")

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

gdp_a_sector_raw <- gdp_sheet %>%
  mutate(
    row_id = row_number(),
    year = parse_bcv_year(...1),
    provisional = is_bcv_provisional(...1)
  ) %>%
  filter(!is.na(year)) %>%
  select(row_id, year, provisional, all_of(series_map$col_name)) %>%
  pivot_longer(
    cols = all_of(series_map$col_name),
    names_to = "col_name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  left_join(series_map, by = "col_name") %>%
  mutate(
    value = clean_bcv_numeric(value),
    date = as.Date(sprintf("%s-01-01", year)),
    frequency = "annual",
    unit = "percent_yoy",
    dataset_id = dataset_id,
    source_id = source_id,
    source_url = source_url,
    sheet_name = "Var_pun%",
    downloaded_at = downloaded_at
  ) %>%
  filter(!is.na(value))

write_processed_dataset(gdp_a_sector_raw, dataset_id)
