# Import historical BCV GDP levels, retaining each price-base block separately.
library(tidyverse)
library(readxl)

rm(list = ls())
source("R/source_helpers.R")
ensure_project_dirs()

# Archive the official workbook using the project's existing download pipeline.
dataset_id <- "real_09_gdp_historical_bcv"
source_url <- "https://bcv.org.ve/sites/default/files/cuentas_macroeconomicas/7_1_14_anual.xls"
raw_path <- "data/raw/bcv_gdp_historical.xls"
download_latest_file(source_url, raw_path, max_attempts = 3, timeout_seconds = 120)
downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Base-year headings precede observations; retain overlap years under both bases.
gdp_historical <- read_bcv_sheet(raw_path, sheet = "7_1_14", col_names = FALSE) %>%
  transmute(
    row_id = row_number(), year_label = clean_bcv_text(...1),
    base_year = if_else(str_detect(coalesce(.data$year_label, ""), "precios de"),
                        parse_bcv_year(.data$year_label), NA_integer_),
    value = clean_bcv_numeric(...3), current_price_value = clean_bcv_numeric(...5)
  ) %>%
  fill(base_year, .direction = "down") %>%
  filter(str_detect(coalesce(.data$year_label, ""), "^\\d{4}")) %>%
  mutate(year = parse_bcv_year(.data$year_label),
         provisional = is_bcv_provisional(.data$year_label))

# Check positive constant-price levels and unique observations before calculating growth.
if (anyNA(gdp_historical$base_year) || any(!is.finite(gdp_historical$value)) ||
    any(gdp_historical$value <= 0) ||
    anyDuplicated(gdp_historical[c("base_year", "year")]) > 0 ||
    !setequal(gdp_historical$year, 1950:2017)) {
  rlang::abort("Invalid historical BCV GDP levels or year coverage", class = "gdp_history_data_error")
}

# Reject missing years inside a base rather than silently skipping a growth estimate.
base_coverage <- gdp_historical %>%
  group_by(.data$base_year) %>%
  summarise(complete = n() == max(.data$year) - min(.data$year) + 1L, .groups = "drop")
if (!all(base_coverage$complete)) {
  rlang::abort("Historical BCV GDP contains gaps within a price base", class = "gdp_history_data_error")
}

# Growth is undefined at the first observation of each base, never across a rebase.
gdp_historical <- gdp_historical %>%
  arrange(.data$base_year, .data$year) %>%
  group_by(.data$base_year) %>%
  mutate(growth_percent = if_else(
    .data$year - lag(.data$year) == 1L,
    100 * (.data$value / lag(.data$value) - 1), NA_real_
  )) %>%
  ungroup() %>%
  mutate(
    date = as.Date(sprintf("%s-01-01", .data$year)), frequency = "annual",
    unit = if_else(.data$base_year == 1997L, "VES_2018_constant_1997", "million_VEB_constant_base_year"),
    dataset_id = dataset_id, source_id = "bcv", source_url = source_url,
    sheet_name = "7_1_14", downloaded_at = downloaded_at
  )
write_processed_dataset(gdp_historical, dataset_id)
