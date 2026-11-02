# Extract CPI level data from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/precios_consumidor/4_5_7_0.xls"
raw_path <- file.path("data", "raw", "bcv_cpi.xls")
dataset_id <- "prices_01_cpi_bcv"

download_latest_file(source_url, raw_path)
downloaded_at <- source_retrieval_time(raw_path, dataset_id)

cpi_sheet <- read_bcv_sheet(raw_path, sheet = "Base Diciembre 2007", col_names = FALSE) %>%
  as_tibble(.name_repair = "unique")

cpi_raw <- cpi_sheet %>%
  transmute(
    row_id = row_number(),
    label = clean_bcv_text(...1),
    year_marker = if_else(!is.na(parse_bcv_year(label)), label, NA_character_),
    cpi_index = clean_bcv_numeric(...2),
    mom_var_pct = clean_bcv_numeric(...3)
  ) %>%
  tidyr::fill(year_marker) %>%
  mutate(
    date = parse_bcv_month_date(label, year_marker),
    provisional = str_detect(coalesce(year_marker, ""), "\\*"),
    dataset_id = dataset_id,
    source_id = source_id,
    source_url = source_url,
    sheet_name = "Base Diciembre 2007",
    frequency = "monthly",
    unit = "index_dec_2007_100",
    downloaded_at = downloaded_at
  ) %>%
  filter(!is.na(date)) %>%
  select(
    dataset_id, source_id, source_url, sheet_name, frequency, date,
    row_id, label, cpi_index, mom_var_pct, provisional, unit, downloaded_at
  )

write_final_dataset(cpi_raw, dataset_id)
