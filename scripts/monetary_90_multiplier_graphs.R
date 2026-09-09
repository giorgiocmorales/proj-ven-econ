# Generate the report and exploratory figures for the monetary multiplier analysis.

library(tidyverse)
library(slider)
library(egg)
library(grid)
library(ragg)

rm(list = ls())

# Use an explicit UTF-8 locale so Spanish labels render consistently on Windows.
locale_result <- Sys.setlocale("LC_CTYPE", "English_United States.utf8")

if (is.na(locale_result)) {
  stop("Unable to set the UTF-8 locale required for chart labels.", call. = FALSE)
}

source("R/source_helpers.R")
source("R/chart_theme.R")
source("R/monetary_multiplier_figures.R")
ensure_project_dirs()
ven_set_theme()

# Load the finalized analytical datasets used by the six figures.
multiplier <- readr::read_csv("data/final/monetary_03_multiplier_bcv.csv", show_col_types = FALSE)
annual_decomposition <- readr::read_csv("data/final/monetary_06_multiplier_detailed_annual_bcv.csv", show_col_types = FALSE)
deposits <- readr::read_csv("data/final/monetary_05_deposits_real_bcv.csv", show_col_types = FALSE)
liquidity <- readr::read_csv("data/final/monetary_02_liq_bcv.csv", show_col_types = FALSE)
deposit_composition <- readr::read_csv(
  "data/final/monetary_07_deposit_currency_composition_bcv.csv",
  show_col_types = FALSE
)
official_fx <- readr::read_csv("data/final/external_fx.csv", show_col_types = FALSE)
reserve_rates <- readr::read_csv("data/final/financial_series.csv", show_col_types = FALSE) %>%
  dplyr::filter(.data$dataset_family == "reserve_coefficients")

# Use the final observation available to each source in its caption.
multiplier_updated <- format(max(multiplier$date, na.rm = TRUE), "%d-%m-%Y")
reserve_updated <- format(max(reserve_rates$date, na.rm = TRUE), "%d-%m-%Y")
deposits_updated <- format(max(deposits$cpi_reference_date, na.rm = TRUE), "%d-%m-%Y")
liquidity_updated <- format(max(liquidity$date, na.rm = TRUE), "%d-%m-%Y")
liquidity_panel_updated <- format(
  max(liquidity$date[liquidity$date <= as.Date("2025-12-31")], na.rm = TRUE),
  "%d-%m-%Y"
)
deposit_composition_updated <- format(
  max(deposit_composition$date[deposit_composition$date <= as.Date("2025-12-31")], na.rm = TRUE),
  "%d-%m-%Y"
)

# Build every figure from the reusable chart functions.
multiplier_figure <- build_multiplier_figure(multiplier, multiplier_updated)
reserve_rates_figure <- build_reserve_rates_figure(reserve_rates, reserve_updated)
deposits_figure <- build_deposits_figure(deposits, deposits_updated)
aggregate_decomposition_figure <- build_aggregate_decomposition_figure(
  annual_decomposition,
  multiplier_updated
)
detailed_decomposition_figure <- build_decomposition_figure(
  annual_decomposition,
  multiplier_updated
)
liquidity_composition_figure <- build_liquidity_composition_figure(liquidity, liquidity_updated)
deposit_currency_composition_figure <- build_deposit_currency_composition_figure(
  deposit_composition,
  deposit_composition_updated
)
deposit_currency_composition_panel_figure <- build_deposit_currency_composition_panel_figure(
  deposit_composition,
  official_fx,
  deposit_composition_updated
)
deposit_summary_panel_figure <- build_liquidity_deposit_summary_panel_figure(
  liquidity,
  deposits,
  liquidity_panel_updated,
  deposits_updated
)

# Export JPEG assets with the same dimensions and rendering as the FX report charts.
ven_save_plot(
  "outputs/figures/monetary_multiplier_1.jpeg",
  multiplier_figure,
  device = ragg::agg_jpeg
)
ven_save_plot(
  "outputs/figures/monetary_multiplier_2_encaje.jpeg",
  reserve_rates_figure,
  device = ragg::agg_jpeg
)
ven_save_plot(
  "outputs/figures/monetary_multiplier_3_deposits.jpeg",
  deposits_figure,
  device = ragg::agg_jpeg
)
ven_save_plot(
  "outputs/figures/monetary_multiplier_4_decomposition.jpeg",
  aggregate_decomposition_figure,
  device = ragg::agg_jpeg
)
ven_save_plot(
  "outputs/figures/monetary_multiplier_4_decomposition_detail.jpeg",
  detailed_decomposition_figure,
  device = ragg::agg_jpeg
)
ven_save_plot(
  "outputs/figures/monetary_multiplier_5_composition.jpeg",
  liquidity_composition_figure,
  device = ragg::agg_jpeg
)
ven_save_plot(
  "outputs/figures/monetary_multiplier_6_deposit_currency_composition.jpeg",
  deposit_currency_composition_figure,
  device = ragg::agg_jpeg
)
# Export the combined grob at the same size and resolution as the report figures.
ragg::agg_jpeg(
  filename = "outputs/figures/monetary_multiplier_7_deposit_currency_usd_panel.jpeg",
  width = ven_chart_specs$width_in,
  height = ven_chart_specs$height_in,
  units = "in",
  res = ven_chart_specs$dpi,
  background = "white"
)
grid::grid.newpage()
grid::grid.draw(deposit_currency_composition_panel_figure)
grDevices::dev.off()

# Export liquidity composition and real deposits with separate legends.
ragg::agg_jpeg(
  filename = "outputs/figures/monetary_multiplier_8_deposit_summary_panel.jpeg",
  width = ven_chart_specs$width_in,
  height = ven_chart_specs$height_in,
  units = "in",
  res = ven_chart_specs$dpi,
  background = "white"
)
grid::grid.newpage()
grid::grid.draw(deposit_summary_panel_figure)
grDevices::dev.off()
