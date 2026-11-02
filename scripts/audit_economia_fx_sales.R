# Compare Economia_A_C monthly FX-sale columns with independently published BCV totals.
# This terminal audit never changes, shifts, fills, or links the report's monthly series.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()

# Read the two existing dollar-series columns at their stored values and original cells.
workbook <- "data/raw/Economia_A_C.xlsx"
if (!file.exists(workbook)) {
  rlang::abort("Economia_A_C source workbook is missing", class = "economia_fx_source_error")
}
source_md5 <- unname(tools::md5sum(workbook))
sheet <- "4 - Venta divisas"
raw <- readxl::read_excel(workbook, sheet = sheet, col_names = FALSE,
  col_types = "text", .name_repair = "minimal")
if (!identical(as.character(unlist(raw[1, c(1, 6, 7)], use.names = FALSE)),
    c("Fecha", "Total d\u00f3lares", "BCV"))) {
  rlang::abort("Economia FX-sale headers changed", class = "economia_fx_schema_error")
}
monthly <- tibble(source_row = seq_len(nrow(raw))[-1],
  date_original = as.Date(readr::parse_double(raw[[1]][-1]), origin = "1899-12-30"),
  total_dollars = readr::parse_double(raw[[6]][-1]),
  bcv_labelled = readr::parse_double(raw[[7]][-1])) %>%
  mutate(date = floor_date(.data$date_original, "month")) %>%
  pivot_longer(c(total_dollars, bcv_labelled), names_to = "workbook_series", values_to = "workbook_value") %>%
  mutate(source_column = if_else(.data$workbook_series == "total_dollars", "F", "G"),
    source_cell = paste0(.data$source_column, .data$source_row),
    source_header = if_else(.data$workbook_series == "total_dollars", "Total d\u00f3lares", "BCV"),
    workbook_rounding_increment = if_else(.data$workbook_series == "total_dollars", 0.1, 1),
    source_file = workbook, source_sheet = sheet, source_md5,
    comparison_unit = "million_USD",
    unit_note = "Compared on the million-USD scale; the workbook column headers do not explicitly state a unit multiplier.")
if (anyNA(monthly$date) || anyDuplicated(monthly[c("date", "workbook_series")]) > 0L ||
    any(!is.na(monthly$workbook_value) & (!is.finite(monthly$workbook_value) | monthly$workbook_value < 0))) {
  rlang::abort("Economia FX-sale dates, values or keys are invalid", class = "economia_fx_data_error")
}

# Compare only like reporting windows; missing months never count as zero or a complete year.
benchmarks <- readr::read_csv("data/final/external_11_fx_sales_bcv_aggregates.csv", show_col_types = FALSE) %>%
  select(period_label, period_start, period_end, frequency, bcv_total = value,
    bcv_rounding_increment = rounding_increment, bcv_source_url = source_url)
comparison <- benchmarks %>% cross_join(monthly) %>%
  filter(.data$date >= .data$period_start, .data$date <= .data$period_end) %>%
  group_by(.data$period_label, .data$period_start, .data$period_end, .data$frequency,
    .data$bcv_total, .data$bcv_rounding_increment, .data$bcv_source_url,
    .data$workbook_series, .data$source_header, .data$source_column,
    .data$workbook_rounding_increment) %>%
  summarise(reported_months = sum(!is.na(.data$workbook_value)),
    available_month_sum = if (any(!is.na(.data$workbook_value))) sum(.data$workbook_value, na.rm = TRUE) else NA_real_,
    .groups = "drop") %>%
  mutate(expected_months = (year(.data$period_end) - year(.data$period_start)) * 12L +
      month(.data$period_end) - month(.data$period_start) + 1L,
    complete_window = .data$reported_months == .data$expected_months,
    workbook_total = if_else(.data$complete_window, .data$available_month_sum, NA_real_),
    difference_million_usd = .data$workbook_total - .data$bcv_total,
    difference_percent = 100 * .data$difference_million_usd / .data$bcv_total,
    maximum_printed_rounding = (.data$expected_months * .data$workbook_rounding_increment +
      .data$bcv_rounding_increment) / 2,
    status = case_when(!.data$complete_window ~ "incomplete_monthly_coverage",
      abs(.data$difference_million_usd) <= .data$bcv_rounding_increment / 2 + 1e-8 ~ "matches_bcv_printed_total",
      abs(.data$difference_million_usd) <= .data$maximum_printed_rounding + 1e-8 ~ "within_possible_printed_rounding",
      TRUE ~ "does_not_match"),
    comparison_unit = "million_USD", source_file = workbook, source_sheet = sheet, source_md5)

# Check monthly levels at the original date and at adjacent dates without changing any date.
official_monthly <- readr::read_csv("data/final/external_11_fx_sales_bcv_monthly.csv", show_col_types = FALSE) %>%
  select(date, official_value = value)
monthly_comparison <- monthly %>% filter(!is.na(.data$workbook_value)) %>%
  left_join(official_monthly, by = "date") %>%
  mutate(difference_million_usd = .data$workbook_value - .data$official_value,
    next_month = .data$date %m+% months(1), prior_month = .data$date %m-% months(1)) %>%
  left_join(official_monthly %>% rename(next_month = date, official_next_month = official_value), by = "next_month") %>%
  left_join(official_monthly %>% rename(prior_month = date, official_prior_month = official_value), by = "prior_month") %>%
  mutate(matches_same_month = !is.na(.data$official_value) & abs(.data$difference_million_usd) < 1e-8,
    matches_next_month = !is.na(.data$official_next_month) & abs(.data$workbook_value - .data$official_next_month) < 1e-8,
    matches_prior_month = !is.na(.data$official_prior_month) & abs(.data$workbook_value - .data$official_prior_month) < 1e-8)

# Save source-cell evidence and terminal audit results; leave both input sources untouched.
readr::write_csv(monthly, "data/interim/economia_a_c_fx_sales_monthly_cells.csv")
readr::write_csv(comparison, "data/interim/economia_a_c_fx_sales_aggregate_comparison.csv")
readr::write_csv(monthly_comparison, "data/interim/economia_a_c_fx_sales_monthly_comparison.csv")
if (!identical(source_md5, unname(tools::md5sum(workbook)))) {
  rlang::abort("Economia source workbook changed during the audit", class = "economia_fx_source_error")
}
print(comparison %>% filter(.data$complete_window) %>%
  select(period_label, source_column, workbook_total, bcv_total, difference_million_usd,
    difference_percent, status), n = Inf, width = Inf)
print(monthly_comparison %>% filter(.data$workbook_series == "bcv_labelled",
    .data$date >= as.Date("2025-09-01"), .data$matches_next_month) %>%
  select(date, source_cell, workbook_value, official_value, next_month, official_next_month), n = Inf)
