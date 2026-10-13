# Extract annual balance of payments tables from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "external_05_bop_a_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_externo/2_4_1_a.xls"
raw_path <- file.path("data", "raw", "bcv_bop_a.xls")

downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
download_latest_file(source_url, raw_path)

bop_sheet <- read_bcv_sheet(raw_path, sheet = "Anual", col_names = FALSE) %>%
  as_tibble(.name_repair = "unique")

year_map <- bop_sheet[7, -1] %>%
  unlist(use.names = TRUE) %>%
  enframe(name = "col_name", value = "year") %>%
  mutate(
    provisional = is_bcv_provisional(year),
    year = parse_bcv_year(year)
  ) %>%
  filter(!is.na(year))

bop_a_raw <- bop_sheet %>%
  mutate(
    row_id = row_number(),
    series_label = clean_bcv_text(...1)
  ) %>%
  filter(row_id > 7, !is.na(series_label)) %>%
  select(row_id, series_label, all_of(year_map$col_name)) %>%
  pivot_longer(
    cols = all_of(year_map$col_name),
    names_to = "col_name",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  left_join(year_map, by = "col_name") %>%
  mutate(
    value = clean_bcv_numeric(value),
    date = as.Date(sprintf("%s-01-01", year)),
    frequency = "annual",
    unit = "usd_million",
    series_path = paste(row_id, series_label, sep = " | "),
    dataset_id = dataset_id,
    source_id = source_id,
    source_url = source_url,
    sheet_name = "Anual",
    downloaded_at = downloaded_at
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    year = as.integer(year)
  )

write_processed_dataset(bop_a_raw, dataset_id)
