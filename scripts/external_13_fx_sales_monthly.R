# Allocate BCV annual sales with Ecoanalitica monthly shares only where no canonical months exist.
# Published BCV months and the supplied daily-workbook extension retain their original values.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()

# Load the canonical monthly evidence before determining which annual periods need allocation.
baseline <- readr::read_csv("data/final/external_11_fx_sales_bcv_monthly.csv", show_col_types = FALSE)
extension <- readr::read_csv("data/final/external_12_fx_interventions_monthly.csv", show_col_types = FALSE)
benchmarks <- readr::read_csv("data/final/external_11_fx_sales_bcv_aggregates.csv", show_col_types = FALSE) %>%
  filter(.data$frequency == "annual") %>%
  transmute(year = year(.data$period_start), benchmark_value = value,
    benchmark_period = period_label, benchmark_source_url = source_url)
canonical <- bind_rows(baseline %>% transmute(date, period_end, value, unit, source_id, source_url,
    source_measure = measure, method = extraction_method, source_note = scope_note,
    coverage = "published_monthly_total", quality_flag = aggregation_check),
  extension %>% transmute(date, period_end, value, unit, source_id, source_url = NA_character_,
    source_measure = measure, method, source_note, coverage,
    quality_flag = if_else(.data$coverage == "recorded_event_sheets_only",
      "event_ledger_with_header_date_conflict", "workbook_summary_verified"))) %>%
  mutate(is_estimated = FALSE)
if (anyDuplicated(canonical$date) > 0L || anyNA(canonical$date) || anyNA(canonical$value) ||
    any(!is.finite(canonical$value) | canonical$value < 0) || any(canonical$unit != "million_USD") ||
    anyDuplicated(benchmarks$year) > 0L || anyNA(benchmarks$benchmark_value) ||
    any(!is.finite(benchmarks$benchmark_value) | benchmarks$benchmark_value <= 0)) {
  rlang::abort("Canonical monthly sales or annual benchmarks are invalid", class = "fx_sales_observation_error")
}
allocation_years <- benchmarks %>%
  anti_join(canonical %>% distinct(year = year(.data$date)), by = "year")

# Read the existing total-dollar column without recomputing currencies or using the BCV-labelled column.
# Ecoanalitica attribution is user-confirmed; the column's unit scale cancels when calculating shares.
workbook <- "data/raw/Economia_A_C.xlsx"
sheet <- "4 - Venta divisas"
if (!file.exists(workbook)) {
  rlang::abort("Ecoanalitica monthly weighting workbook is missing", class = "economia_fx_source_error")
}
source_md5 <- unname(tools::md5sum(workbook))
raw <- readxl::read_excel(workbook, sheet = sheet, col_names = FALSE,
  col_types = "text", .name_repair = "minimal")
if (!identical(as.character(unlist(raw[1, c(1, 6)], use.names = FALSE)),
    c("Fecha", "Total d\u00f3lares"))) {
  rlang::abort("Ecoanalitica monthly weighting headers changed", class = "economia_fx_schema_error")
}
raw_dates <- readr::parse_double(raw[[1]][-1])
raw_values <- readr::parse_double(raw[[6]][-1])
readr::stop_for_problems(raw_dates)
readr::stop_for_problems(raw_values)
weights <- tibble(source_row = seq_len(nrow(raw))[-1],
  date_original = as.Date(raw_dates, origin = "1899-12-30"),
  weight_source_value = raw_values) %>%
  mutate(date = floor_date(.data$date_original, "month"), year = year(.data$date)) %>%
  inner_join(allocation_years, by = "year")
if (anyNA(weights$date) || anyNA(weights$weight_source_value) ||
    anyDuplicated(weights$date) > 0L ||
    any(!is.finite(weights$weight_source_value) | weights$weight_source_value < 0) ||
    nrow(weights) != 12L * nrow(allocation_years) ||
    any((weights %>% count(.data$year))$n != 12L)) {
  rlang::abort("Allocation requires twelve nonmissing Ecoanalitica observations per year",
    class = "economia_fx_coverage_error")
}

# Use full stored precision for the annual denominators, monthly shares and allocated amounts.
weights <- weights %>% group_by(.data$year) %>%
  mutate(weight_source_annual_total = sum(.data$weight_source_value)) %>% ungroup()
if (any(weights$weight_source_annual_total <= 0)) {
  rlang::abort("Ecoanalitica annual weighting totals must be positive", class = "economia_fx_data_error")
}
weights <- weights %>% mutate(monthly_share = .data$weight_source_value / .data$weight_source_annual_total,
  monthly_share_percent = 100 * .data$monthly_share,
  allocated_value = .data$benchmark_value * .data$monthly_share,
  allocated_value_usd = .data$allocated_value * 1e6,
  allocation_unit = "million_USD", weight_source = "Ecoanalitica",
  weight_source_attribution = "user_confirmed", weight_source_file = workbook,
  weight_source_sheet = sheet, weight_source_cell = paste0("F", .data$source_row),
  weight_source_md5 = source_md5,
  weight_source_unit_note = "Total-dollar column on the million-USD comparison scale; shares are invariant to the common unit multiplier.") %>%
  arrange(.data$date)
checks <- weights %>% group_by(.data$year, .data$benchmark_value, .data$weight_source_annual_total) %>%
  summarise(months = n(), share_sum = sum(.data$monthly_share),
    allocated_total = sum(.data$allocated_value), .groups = "drop") %>%
  mutate(difference_million_usd = .data$allocated_total - .data$benchmark_value,
    status = "allocated_to_bcv_annual_total_by_construction")
if (any(abs(checks$share_sum - 1) > 1e-12) || any(abs(checks$difference_million_usd) > 1e-8)) {
  rlang::abort("Allocated monthly sales do not reconcile to BCV annual totals", class = "fx_sales_reconciliation_error")
}

# Keep estimated observations and their weighting evidence explicit beside canonical observations.
allocated <- weights %>% transmute(date, period_end = ceiling_date(.data$date, "month") - days(1),
  value = allocated_value, unit = allocation_unit, source_id = "bcv_annual_ecoanalitica_weights",
  source_url = benchmark_source_url, source_measure = "smc_fx_sales_total",
  method = "bcv_annual_total_times_ecoanalitica_monthly_share",
  source_note = "Estimated monthly distribution: BCV annual total multiplied by Ecoanalitica's share for that month; preserves source zero values.",
  coverage = "annual_benchmark_allocated_to_months", quality_flag = "estimated_monthly_allocation",
  is_estimated = TRUE, benchmark_period, benchmark_value, weight_source_value,
  weight_source_annual_total, monthly_share, monthly_share_percent, weight_source,
  weight_source_attribution, weight_source_file, weight_source_sheet, weight_source_cell, weight_source_md5)
combined <- bind_rows(allocated, canonical) %>% arrange(.data$date)
expected_dates <- seq.Date(as.Date("2019-01-01"), max(canonical$date), by = "month")
if (nrow(combined) != length(expected_dates) || anyDuplicated(combined$date) > 0L ||
    any(combined$date != expected_dates) || anyNA(combined$value) ||
    any(!is.finite(combined$value) | combined$value < 0) ||
    !identical(source_md5, unname(tools::md5sum(workbook)))) {
  rlang::abort("Combined monthly FX sales have invalid coverage, values or changed source evidence",
    class = "fx_sales_coverage_error")
}

# Export the reviewable monthly series and the separate allocation evidence and annual controls.
readr::write_csv(weights, "data/interim/external_13_fx_sales_monthly_weights.csv")
readr::write_csv(checks, "data/interim/external_13_fx_sales_annual_allocation.csv")
readr::write_csv(combined, "data/final/external_13_fx_sales_monthly.csv")
print(checks, n = Inf, width = Inf)
print(combined %>% count(.data$source_id, .data$is_estimated), n = Inf)
