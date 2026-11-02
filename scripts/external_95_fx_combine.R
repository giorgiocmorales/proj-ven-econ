# Consolidate FX data

#Load packages ------------
library(tidyverse)
library(lubridate)

source("R/source_helpers.R")
ensure_project_dirs()

# Clean up
rm(list = ls())

add_missing_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% names(df)) {
      df[[column]] <- NA
    }
  }

  df
}

# Load data ----------
source("R/fx_intervention_helpers.R")

bcv_fx_smc <- read_csv("data/final/external_01_fx_smc_bcv.csv",
                       col_types = cols(
                         fecha_valor = col_date(),
                         currency = col_character(),
                         bid = col_double(),
                         ask = col_double(),
                         database_id = col_character()
                         ))

bcv_fx_indx <- read_csv("data/final/external_02_fx_ind_bcv.csv",
                        col_types = cols(
                          fecha = col_date(),
                          tasa = col_double()
                        ))

ves_fx_yad <- read_csv("data/final/external_04_fx_yadio.csv",
                       col_types = cols(
                         date = col_date(),
                         rate = col_double(),
                         implicit_rate = col_double(),
                         avg24h = col_double(),
                         usdbtc = col_double()
                       ))

# Convert EUR intervention quotes before restricting the reference dataset to USD.
bcv_fx_interventions <- read_csv("data/final/external_03_fx_int_bcv.csv",
  col_types = cols(date = col_date(), intervention_id = col_character(), rate = col_double(), currency = col_character()))
intervention_conversion <- convert_bcv_intervention(bcv_fx_smc, bcv_fx_interventions)
write_csv(intervention_conversion$observations, "data/interim/external_95_fx_intervention_conversion.csv")

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
min_date <- min(bcv_fx_smc$date, bcv_fx_indx$date, ves_fx_yad$date, intervention_conversion$daily$date, na.rm = TRUE)
max_date <- max(bcv_fx_smc$date, bcv_fx_indx$date, ves_fx_yad$date, intervention_conversion$daily$date, na.rm = TRUE)

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

# Add the observed intervention series after filling other sources, preserving its gaps.
fx_combined <- fx_combined %>%
  left_join(intervention_conversion$daily, by = "date", relationship = "one-to-one")

# Save ----------------
output_path <- "data/final/external_fx.csv"

# A failed write must stop downstream reporting from reading an older file.
write_csv(fx_combined, output_path)

# Clean up
rm(
  bcv_fx_indx, bcv_fx_smc, ves_fx_yad, full_date_range, fx_combined,
  output_path, add_missing_columns,
  bcv_fx_interventions, intervention_conversion
)

