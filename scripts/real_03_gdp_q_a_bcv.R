# Extract quarterly GDP activity tables from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_03_gdp_q_a_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/cuentas_macroeconomicas/5_2_4_ae_trim.xlsx"
raw_path <- file.path("data", "raw", "bcv_gdp_q_a.xlsx")

downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
download_latest_file(source_url, raw_path)

gdp_sheet <- read_bcv_sheet(raw_path, sheet = "Var_pun%", col_names = FALSE) %>%
  as_tibble(.name_repair = "unique")

quarter_map <- tibble(
  col_name = names(gdp_sheet)[-1],
  year = unlist(gdp_sheet[6, -1], use.names = FALSE),
  quarter_label = unlist(gdp_sheet[7, -1], use.names = FALSE)
) %>%
  mutate(
    provisional = is_bcv_provisional(year) | is_bcv_provisional(quarter_label),
    year = parse_bcv_year(year),
    quarter = parse_bcv_quarter(quarter_label)
  ) %>%
  tidyr::fill(year, provisional) %>%
  filter(!is.na(year), !is.na(quarter)) %>%
  mutate(date = make_bcv_quarter_date(year, quarter))

gdp_q_activity_raw <- gdp_sheet %>%
  mutate(
    row_id = row_number(),
    series_label = clean_bcv_text(...1)
  ) %>%
  filter(row_id > 7, !is.na(series_label)) %>%
  select(row_id, series_label, all_of(quarter_map$col_name)) %>%
  pivot_longer(
    cols = all_of(quarter_map$col_name),
    names_to = "col_name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  left_join(quarter_map, by = "col_name") %>%
  mutate(
    value = clean_bcv_numeric(value),
    frequency = "quarterly",
    unit = "percent_yoy",
    dataset_id = dataset_id,
    source_id = source_id,
    source_url = source_url,
    sheet_name = "Var_pun%",
    downloaded_at = downloaded_at
  ) %>%
  filter(!is.na(value))

write_processed_dataset(gdp_q_activity_raw, dataset_id)
