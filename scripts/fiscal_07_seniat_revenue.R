# Build a monthly SENIAT research series from the archived, manually reviewed evidence.
# Keep reported aggregates, alternative transcriptions and published dollars separate.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()
dataset_id <- "fiscal_07_seniat_revenue"
raw_dir <- "data/raw/seniat_revenue/research_2026-10-23"

# Verify the immutable research vintage before reading any observations.
manifest <- readr::read_csv(file.path(raw_dir, "manifest.csv"),
  col_types = cols(file = col_character(), md5 = col_character()), show_col_types = FALSE)
paths <- file.path(raw_dir, manifest$file)
if (!all(file.exists(paths)) || anyDuplicated(manifest$file) ||
    !all(c("reviewed_observations.csv", "reviewed_primary_monthly.csv", "snapshot.json") %in% manifest$file)) {
  rlang::abort("SENIAT research files or manifest entries are missing", class = "seniat_source_error")
}
if (any(unname(tools::md5sum(paths)) != manifest$md5)) {
  rlang::abort("SENIAT archived research hashes changed", class = "seniat_source_integrity_error")
}
observations <- readr::read_csv(file.path(raw_dir, "reviewed_observations.csv"),
  col_types = cols(.default = col_character(), rounding_increment = col_double(),
    source_rank = col_integer(), eligible = col_logical()), show_col_types = FALSE)
readr::stop_for_problems(observations)
observations <- observations %>% mutate(
  period_start = readr::parse_date(.data$period_start, format = "%Y-%m-%d"),
  period_end = readr::parse_date(.data$period_end, format = "%Y-%m-%d"),
  publication_date = readr::parse_date(.data$publication_date, format = "%Y-%m-%d"),
  reported_amount = readr::parse_double(.data$reported_value))
if (anyNA(observations[c("observation_id", "period_start", "period_end", "reported_amount",
    "currency_regime", "rounding_increment", "source_rank", "source_url", "eligible")]) ||
    anyDuplicated(observations$observation_id) ||
    any(!observations$frequency %in% c("monthly", "annual", "year_to_date")) ||
    any(!observations$currency_regime %in% c("VES_2018", "VES_2021", "USD")) ||
    any(!is.finite(observations$reported_amount) | observations$reported_amount < 0) ||
    any(!is.finite(observations$rounding_increment) | observations$rounding_increment <= 0) ||
    any(!observations$source_rank %in% 1:5) ||
    any(!grepl("^https?://", observations$source_url)) ||
    any(observations$period_start != floor_date(observations$period_start, "month")) ||
    any(observations$period_end != ceiling_date(observations$period_end, "month") - days(1)) ||
    any(observations$period_start > observations$period_end) ||
    any(observations$frequency == "monthly" &
      floor_date(observations$period_end, "month") != observations$period_start) ||
    any(observations$frequency != "monthly" & month(observations$period_start) != 1L) ||
    any(observations$frequency == "annual" & month(observations$period_end) != 12L) ||
    any(!is.na(observations$publication_date) & observations$publication_date < observations$period_end)) {
  rlang::abort("Invalid SENIAT observation, period, source or native currency", class = "seniat_observation_error")
}

# Normalize only the monetary expression; never convert dollars or infer a source currency from a year.
observations <- observations %>% mutate(
  monetary_divisor = if_else(.data$currency_regime == "VES_2018", 1e6, 1),
  normalized_amount = .data$reported_amount / .data$monetary_divisor,
  normalized_increment = .data$rounding_increment / .data$monetary_divisor,
  normalized_currency = if_else(.data$currency_regime == "USD", "USD", "VES_2021"),
  research_vintage = "2026-10-23")
eligible <- observations %>% filter(.data$eligible) %>%
  arrange(.data$source_rank, desc(.data$publication_date), .data$observation_id)
selected <- eligible %>% group_by(.data$period_start, .data$period_end,
  .data$frequency, .data$measure, .data$normalized_currency) %>% slice_head(n = 1) %>% ungroup()

# Compare each eligible alternative against the preferred source at the identical grain and currency.
comparisons <- eligible %>% inner_join(selected %>% transmute(period_start, period_end,
    frequency, measure, normalized_currency, selected_id = observation_id,
    selected_amount = normalized_amount, selected_increment = normalized_increment),
  by = c("period_start", "period_end", "frequency", "measure", "normalized_currency")) %>%
  mutate(difference = .data$normalized_amount - .data$selected_amount,
    comparison_allowance = .data$normalized_increment + .data$selected_increment,
    numerical_allowance = .Machine$double.eps * pmax(abs(.data$normalized_amount),
      abs(.data$selected_amount), 1) * 8,
    conflicts_with_selected = abs(.data$difference) > .data$comparison_allowance + .data$numerical_allowance)
flags <- comparisons %>% group_by(.data$period_start, .data$period_end, .data$frequency,
  .data$measure, .data$normalized_currency) %>% summarise(
    candidate_count = n(), source_conflict = any(.data$conflicts_with_selected), .groups = "drop")
selected <- selected %>% left_join(flags,
  by = c("period_start", "period_end", "frequency", "measure", "normalized_currency"))

# Create the full requested monthly calendar and retain missing observations as missing.
calendar <- tibble(date = seq(as.Date("2019-01-01"), as.Date("2026-09-01"), by = "month"))
monthly <- calendar %>% left_join(selected %>%
    filter(.data$frequency == "monthly", .data$measure == "total_collection",
      .data$normalized_currency == "VES_2021"), by = c("date" = "period_start")) %>%
  mutate(period_end = ceiling_date(.data$date, "month") - days(1),
    value = .data$normalized_amount / 1e6, unit = "million_VES_2021",
    source_id = "seniat", is_estimated = FALSE,
    status = case_when(is.na(.data$observation_id) ~ "not_located",
      .data$source_rank <= 2 ~ "original_official_post",
      .data$source_rank <= 4 ~ "reproduced_release", TRUE ~ "secondary_compilation"),
    source_conflict = replace_na(.data$source_conflict, FALSE)) %>%
  select(date, period_end, value, unit, source_id, status, source_conflict, is_estimated,
    observation_id, reported_value, currency_regime, monetary_divisor, normalized_increment,
    publication_date, publisher, source_rank, source_url, evidence_type, source_excerpt,
    candidate_count, quality_note, research_vintage)
if (nrow(monthly) != 93L || anyDuplicated(monthly$date) ||
    any(monthly$value < 0, na.rm = TRUE) || any(!is.finite(monthly$value[!is.na(monthly$value)]))) {
  rlang::abort("Invalid SENIAT monthly calendar", class = "seniat_monthly_coverage_error")
}

# Reconcile separately published aggregates only when every constituent month is available.
# A partial sum is an audit field and is never used to estimate the missing months.
aggregates <- selected %>% filter(.data$frequency != "monthly", .data$measure == "total_collection",
  .data$normalized_currency == "VES_2021")
reconciliation <- aggregates %>% rowwise() %>% mutate(
  expected_months = interval(.data$period_start, .data$period_end + days(1)) %/% months(1),
  observed_months = sum(!is.na(monthly$value[monthly$date >= .data$period_start &
    monthly$date <= .data$period_end])),
  partial_sum_bolivares_2021 = sum(monthly$value[monthly$date >= .data$period_start &
    monthly$date <= .data$period_end], na.rm = TRUE) * 1e6,
  monthly_increment_allowance = sum(monthly$normalized_increment[monthly$date >= .data$period_start &
    monthly$date <= .data$period_end], na.rm = TRUE),
  difference_bolivares_2021 = if_else(.data$observed_months == .data$expected_months,
    .data$partial_sum_bolivares_2021 - .data$normalized_amount, NA_real_),
  reconciliation_status = case_when(.data$observed_months < .data$expected_months ~ "incomplete_monthly_coverage",
    abs(.data$difference_bolivares_2021) <= .data$monthly_increment_allowance +
      .data$normalized_increment + .Machine$double.eps * abs(.data$normalized_amount) * 16 ~
      "within_printed_increment_allowance", TRUE ~ "unresolved_difference")) %>% ungroup()
gaps <- monthly %>% filter(is.na(.data$value)) %>% transmute(date, period_end,
  status = "not_located_in_reviewed_sources", research_vintage = "2026-10-23",
  note = case_when(.data$date == as.Date("2020-04-01") ~
      "Contemporary commentary reports an omission; exact monthly total not recovered.",
    .data$date %in% as.Date(c("2026-04-01", "2026-07-01")) ~
      "Not located in official X searches or reviewed reproductions; not evidence of zero collection.",
    .data$date == as.Date("2023-12-01") ~
      "A secondary candidate remains excluded because its period is not verified.",
    TRUE ~ "Exact monthly total not recovered; no aggregate residual substituted."))
coverage <- monthly %>% group_by(year = year(.data$date)) %>% summarise(
  expected_months = n(), observed_months = sum(!is.na(.data$value)),
  original_post_months = sum(.data$status == "original_official_post"),
  missing_months = sum(is.na(.data$value)), conflict_months = sum(.data$source_conflict), .groups = "drop")

# Persist the research outputs separately from the report's USD revenue and Ecoanalitica layers.
readr::write_csv(observations, paste0("data/interim/", dataset_id, "_observations.csv"), na = "")
readr::write_csv(comparisons %>% filter(.data$observation_id != .data$selected_id),
  paste0("data/interim/", dataset_id, "_alternatives.csv"), na = "")
readr::write_csv(monthly, paste0("data/final/", dataset_id, "_monthly.csv"), na = "")
readr::write_csv(reconciliation, paste0("data/interim/", dataset_id, "_reconciliation.csv"), na = "")
readr::write_csv(gaps, paste0("data/interim/", dataset_id, "_gaps.csv"), na = "")
readr::write_csv(coverage, paste0("data/interim/", dataset_id, "_coverage.csv"), na = "")
readr::write_csv(selected %>% filter(.data$normalized_currency == "USD"),
  paste0("data/interim/", dataset_id, "_published_usd.csv"), na = "")
readr::write_csv(selected %>% filter(.data$measure != "total_collection"),
  paste0("data/interim/", dataset_id, "_components.csv"), na = "")
print(coverage)
print(reconciliation %>% select(period_start, period_end, expected_months,
  observed_months, difference_bolivares_2021, reconciliation_status))
