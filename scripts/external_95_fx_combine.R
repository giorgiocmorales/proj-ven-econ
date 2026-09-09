# Consolidate FX data

#Load packages ------------
library(tidyverse)
library(lubridate)

source("R/source_helpers.R")
ensure_project_dirs()

# Clean up
rm(list = ls())

resolve_input_path <- function(primary_path, legacy_paths = character()) {
  candidates <- c(primary_path, legacy_paths)
  match <- candidates[file.exists(candidates)][1]

  if (is.na(match)) {
    stop(sprintf("Missing required input file: %s", primary_path))
  }

  match
}

add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% names(df)) {
      df[[column]] <- NA
    }
  }

  df
}

# Load data ----------
bcv_fx_smc <- read_csv(resolve_input_path(
  "data/processed/external_01_fx_smc_bcv.csv",
  c("data/processed/external_01_bcv_fx_smc.csv", "data/processed/fx_01_bcv_smc.csv")
),
                       col_types = cols(
                         fecha_valor = col_date(),
                         currency = col_character(),
                         bid = col_double(),
                         ask = col_double(),
                         database_id = col_character()
                         ))

bcv_fx_indx <- read_csv(resolve_input_path(
  "data/processed/external_02_fx_ind_bcv.csv",
  c("data/processed/external_02_bcv_fx_indx.csv", "data/processed/fx_02_bcv_indx.csv")
),
                        col_types = cols(
                          fecha = col_date(),
                          tasa = col_double()
                        ))

ves_fx_yad <- read_csv(resolve_input_path(
  "data/processed/external_04_fx_yadio.csv",
  c("data/processed/external_03_fx_yadio.csv", "data/processed/external_03_yadio_fx.csv", "data/processed/fx_03_yadio.csv")
),
                       col_types = cols(
                         date = col_date(),
                         rate = col_double(),
                         implicit_rate = col_double(),
                         avg24h = col_double(),
                         usdbtc = col_double()
                       ))

# Clean data ------------
bcv_fx_smc <- bcv_fx_smc %>%
  add_missing_columns(c("currency", "ask", "usd_ask")) %>%
  mutate(
    currency = coalesce(currency, "USD"),
    rate_smc = coalesce(ask, usd_ask)
  ) %>%
  filter(currency == "USD") %>%
  rename(date = fecha_valor) %>%
  select(date, rate_smc) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE)

bcv_fx_indx <- bcv_fx_indx %>%
  rename(date = fecha, rate_indx = tasa) %>%
  select(date, rate_indx) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE)

ves_fx_yad <- ves_fx_yad %>%
  rename(date = date, rate_yad = implicit_rate) %>%
  select(date, rate_yad) %>%
  filter(!is.na(date)) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE)

# Set combination date range -------------
min_date <- min(bcv_fx_smc$date, bcv_fx_indx$date, ves_fx_yad$date, na.rm = TRUE)
max_date <- max(bcv_fx_smc$date, bcv_fx_indx$date, ves_fx_yad$date, na.rm = TRUE)

full_date_range <- tibble(date = seq.Date(from = min_date, to = max_date, by = "day"))

# Combine data -------------
fx_combined <- full_join(full_date_range, bcv_fx_smc, by = "date") %>%
  full_join(bcv_fx_indx, by = "date") %>%
  full_join(ves_fx_yad, by = "date")

# Fill NAs down -------------
fx_combined <- fx_combined %>%
  arrange(date) %>%
  fill(rate_smc, rate_indx, rate_yad) %>%
  # Do not extend a stopped source to another source's latest observation date.
  mutate(
    rate_smc = if_else(date > max(bcv_fx_smc$date), NA_real_, rate_smc),
    rate_indx = if_else(date > max(bcv_fx_indx$date), NA_real_, rate_indx),
    rate_yad = if_else(date > max(ves_fx_yad$date), NA_real_, rate_yad)
  )

# Correct for currency reconversions -------------
fx_combined <- fx_combined %>%
  mutate(rate_smc_adj = rate_smc, 
         rate_smc_adj = if_else(date <= as.Date("2021-10-03"), rate_smc_adj / 1000000, rate_smc_adj)) %>%
  mutate(rate_indx_adj = rate_indx,
         rate_indx_adj = if_else(date <= as.Date("2021-10-03"), rate_indx_adj / 1000000, rate_indx_adj)) %>%
  select(date, rate_smc, rate_smc_adj, rate_indx, rate_indx_adj, rate_yad)

# Save ----------------
output_path <- "data/final/external_fx.csv"

# A failed write must stop downstream reporting from reading an older file.
write_csv(fx_combined, output_path)

# Clean up
rm(
  bcv_fx_indx, bcv_fx_smc, ves_fx_yad, full_date_range, fx_combined,
  output_path, resolve_input_path, add_missing_columns
)

