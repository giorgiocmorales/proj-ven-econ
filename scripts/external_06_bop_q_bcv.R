# Extract quarterly balance of payments tables from BCV

library(tidyverse)
library(readxl)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "external_06_bop_q_bcv"
source_id <- "bcv"
source_url <- "https://bcv.org.ve/sites/default/files/indicadores_sector_externo/2_4_1_t.xls"
raw_path <- file.path("data", "raw", "bcv_bop_q.xls")

downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
download_latest_file(source_url, raw_path)

bop_sheet <- read_bcv_sheet(raw_path, sheet = "Trimestral", col_names = FALSE) %>%
  as_tibble(.name_repair = "unique")

quarter_map <- tibble(
  col_name = names(bop_sheet)[-1],
  year = unlist(bop_sheet[7, -1], use.names = FALSE),
  quarter_label = unlist(bop_sheet[8, -1], use.names = FALSE)
) %>%
  mutate(
    provisional = is_bcv_provisional(year) | is_bcv_provisional(quarter_label),
    year = parse_bcv_year(year),
    quarter = parse_bcv_quarter(quarter_label)
  ) %>%
  tidyr::fill(year, provisional) %>%
  filter(!is.na(year), !is.na(quarter)) %>%
  mutate(date = make_bcv_quarter_date(year, quarter))

bop_q_raw <- bop_sheet %>%
  mutate(
    row_id = row_number(),
    series_label = clean_bcv_text(...1)
  ) %>%
  filter(row_id > 8, !is.na(series_label)) %>%
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
    unit = "usd_million",
    series_path = paste(row_id, series_label, sep = " | "),
    dataset_id = dataset_id,
    source_id = source_id,
    source_url = source_url,
    sheet_name = "Trimestral",
    downloaded_at = downloaded_at
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter)
  )

write_processed_dataset(bop_q_raw, dataset_id)
