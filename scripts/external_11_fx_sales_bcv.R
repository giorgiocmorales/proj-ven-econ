# Rebuild published SMC FX sales from the reviewed May 5, 2026 BCV graphics.
# Monthly labels remain at their printed precision; annual/YTD totals stay separate.
# Preserve the 2026 coverage changes and source disagreement without adjusting values.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()

# Verify the preserved user images, original publication, and reviewed transcription.
raw_dir <- "data/raw/bcv_smc_sales"
required_files <- file.path(raw_dir, c("snapshot.json", "source_manifest.csv", "reviewed_observations.csv"))
if (!all(file.exists(required_files))) {
  rlang::abort("SMC FX-sales source snapshot is incomplete", class = "fx_sales_evidence_error")
}
snapshot <- jsonlite::fromJSON(required_files[1])
manifest <- readr::read_csv(required_files[2], show_col_types = FALSE)
source_paths <- file.path(raw_dir, manifest$file_name)
if (!all(file.exists(source_paths)) || anyNA(manifest$md5) ||
    !identical(unname(tools::md5sum(source_paths)), manifest$md5)) {
  rlang::abort("SMC FX-sales source files differ from their manifest", class = "fx_sales_evidence_error")
}

# Parse explicit reporting windows and reconcile each transcription to its printed label.
observations <- readr::read_csv(required_files[3], show_col_types = FALSE,
  col_types = cols(period_start = col_date(), period_end = col_date(), raw_value = col_character()))
readr::stop_for_problems(observations)
required_columns <- c("period_label", "period_start", "period_end", "frequency", "raw_value",
                      "value", "rounding_increment", "source_asset")
if (!all(required_columns %in% names(observations))) {
  rlang::abort("SMC FX-sales ledger lacks required fields", class = "fx_sales_schema_error")
}
printed_values <- readr::parse_number(observations$raw_value,
  locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
readr::stop_for_problems(printed_values)
if (nrow(observations) != 49L || anyNA(observations) || any(!is.finite(observations$value)) ||
    any(observations$value <= 0) || any(observations$rounding_increment <= 0) ||
    any(abs(printed_values - observations$value) > 1e-8) ||
    any(observations$period_start > observations$period_end) ||
    any(observations$period_end > as.Date(snapshot$as_of)) ||
    anyDuplicated(observations[c("frequency", "period_start", "period_end")]) > 0L ||
    any(!observations$frequency %in% c("monthly", "annual", "year_to_date")) ||
    any(!file.path(raw_dir, observations$source_asset) %in% source_paths)) {
  rlang::abort("SMC FX-sales ledger has invalid values, windows, keys or evidence", class = "fx_sales_observation_error")
}

# Keep provenance, scope changes, and numeric precision beside each reported observation.
observations <- observations %>%
  mutate(date = .data$period_start, unit = snapshot$unit, measure = snapshot$measure,
    source_id = snapshot$source_id, dataset_id = snapshot$dataset_id,
    publication_date = as.Date(snapshot$publication_date), as_of = as.Date(snapshot$as_of),
    source_url = snapshot$source_url, source_pdf_url = snapshot$source_pdf_url,
    extraction_method = snapshot$extraction_method,
    scope_note = snapshot$source_note_original,
    coverage_regime = case_when(
      .data$period_end < as.Date(snapshot$auctions_included_since) ~ "original_smc_coverage",
      .data$period_end < as.Date(snapshot$direct_sales_included_since) ~ "includes_auctions_from_2026_01_20",
      TRUE ~ "includes_auctions_from_2026_01_20_and_direct_sales_from_2026_03_31")) %>%
  arrange(.data$frequency, .data$period_start, .data$period_end)
monthly <- observations %>% filter(.data$frequency == "monthly")
aggregates <- observations %>% filter(.data$frequency != "monthly")
expected_months <- seq.Date(as.Date("2023-01-01"), as.Date("2026-04-01"), by = "month")
if (!identical(monthly$date, expected_months) || nrow(aggregates) != 9L ||
    any(monthly$period_end != ceiling_date(monthly$date, "month") - days(1))) {
  rlang::abort("SMC FX-sales snapshot has unexpected monthly or aggregate coverage", class = "fx_sales_coverage_error")
}

# Compare independently printed aggregates with rounded monthly labels; retain discrepancies.
reconciliation <- aggregates %>% filter(.data$period_start >= min(monthly$date)) %>%
  select(period_label, period_start, period_end, published_total = value,
    aggregate_increment = rounding_increment) %>%
  cross_join(monthly %>% transmute(month_date = .data$date, month_end = .data$period_end,
    monthly_value = .data$value, monthly_increment = .data$rounding_increment)) %>%
  filter(.data$month_date >= .data$period_start, .data$month_end <= .data$period_end) %>%
  group_by(.data$period_label, .data$period_start, .data$period_end,
    .data$published_total, .data$aggregate_increment) %>%
  summarise(months = n(), monthly_sum = sum(.data$monthly_value),
    rounding_tolerance = sum(.data$monthly_increment) / 2 + first(.data$aggregate_increment) / 2,
    .groups = "drop") %>%
  mutate(difference_million_usd = .data$monthly_sum - .data$published_total,
    status = if_else(abs(.data$difference_million_usd) <= .data$rounding_tolerance + 1e-8,
      "within_printed_rounding", "published_source_discrepancy"),
    unit = snapshot$unit, source_url = snapshot$source_url)
expected_counts <- with(reconciliation,
  (year(period_end) - year(period_start)) * 12L + month(period_end) - month(period_start) + 1L)
if (nrow(reconciliation) != 5L || any(reconciliation$months != expected_counts)) {
  rlang::abort("SMC FX-sales reconciliation lacks complete period coverage", class = "fx_sales_reconciliation_error")
}

# Tag the unresolved source disagreement while preserving every published number.
discrepancies <- reconciliation %>% filter(.data$status == "published_source_discrepancy")
conflicting_keys <- observations %>%
  cross_join(discrepancies %>% select(conflict_start = period_start, conflict_end = period_end)) %>%
  filter(.data$period_start <= .data$conflict_end, .data$period_end >= .data$conflict_start) %>%
  distinct(.data$frequency, .data$period_start, .data$period_end) %>%
  mutate(source_conflict = TRUE)
observations <- observations %>%
  left_join(conflicting_keys, by = c("frequency", "period_start", "period_end")) %>%
  mutate(aggregation_check = case_when(
  replace_na(.data$source_conflict, FALSE) ~ "published_source_discrepancy",
  .data$period_start >= min(monthly$date) ~ "within_printed_rounding",
  TRUE ~ "monthly_comparison_unavailable")) %>% select(-source_conflict)
readr::write_csv(observations, "data/final/external_11_fx_sales_bcv.csv")
observations %>% filter(.data$frequency == "monthly") %>%
  readr::write_csv("data/final/external_11_fx_sales_bcv_monthly.csv")
observations %>% filter(.data$frequency != "monthly") %>%
  readr::write_csv("data/final/external_11_fx_sales_bcv_aggregates.csv")
readr::write_csv(reconciliation, "data/interim/external_11_fx_sales_bcv_reconciliation.csv")
print(reconciliation %>% select(period_label, monthly_sum, published_total,
  difference_million_usd, status), n = Inf)
if (nrow(discrepancies) > 0L) {
  rlang::warn("Published 2026 January-April monthly labels sum to 3993 million USD; the aggregate table reports 3987.1. Both are retained without adjustment.",
    class = "fx_sales_published_source_discrepancy")
}
