# Consolidate month-end wage/food levels, keeping original and current-bolivar scales.
# The last listed amount in each month applies; this is not a day-weighted monthly mean.
# Carry absent months forward only through the chronology's final month. An explicitly
# blank source cell remains missing, including the food allowance in May 2025.
library(dplyr)
library(tidyr)
source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_14_minimum_wage_monthly"
observations <- readr::read_csv("data/final/real_13_minimum_wage_wikipedia.csv",
  show_col_types = FALSE)
if (nrow(observations) == 0L || anyNA(observations$date) ||
    anyDuplicated(observations[c("date", "variable_id")]) ||
    !setequal(unique(observations$variable_id), c("minimum_wage", "food_allowance"))) {
  rlang::abort("Invalid wage chronology input", class = "minimum_wage_monthly_input_error")
}

# Rolling joins select source rows, not nonmissing amounts, preserving explicit blanks.
months <- seq.Date(lubridate::floor_date(min(observations$date), "month"),
  lubridate::floor_date(max(observations$date), "month"), by = "month")
monthly <- tidyr::crossing(date = months, variable_id = unique(observations$variable_id)) %>%
  mutate(month_end = lubridate::ceiling_date(date, "month") - lubridate::days(1)) %>%
  left_join(observations %>% rename(source_date = date, source_value_raw = value_raw,
      source_currency = currency_raw, source_conversion_factor = conversion_factor,
      source_dataset_id = dataset_id),
    by = join_by(variable_id, closest(month_end >= source_date)), relationship = "many-to-one")

# Express monthly nominal amounts in the currency active at month end, including
# reconversions between source observations (notably August 2018). Source quotes stay intact.
currency_scales <- readr::read_csv("registry/venezuela_currency_scales.csv", show_col_types = FALSE)
monthly <- monthly %>% crossing(currency_scales) %>%
  filter(month_end >= effective_start, is.na(effective_end) | month_end <= effective_end) %>%
  mutate(dataset_id = dataset_id, frequency = "monthly", currency_raw = currency_code,
    conversion_factor = current_ves_per_currency_unit,
    value_raw = if_else(currency_raw == source_currency, source_value_raw, value / conversion_factor),
    unit_raw = paste0(currency_raw, "_per_month"),
    source_currency_rebased = currency_raw != source_currency,
    carried_forward = lubridate::floor_date(source_date, "month") < date,
    value_status = case_when(is.na(source_value_raw) ~ "source_missing",
      carried_forward ~ "carried_forward", TRUE ~ "source_reported"),
    monthly_method = "last_listed_level_at_month_end",
    source_coverage_end = max(observations$date)) %>%
  select(dataset_id, source_id, date, month_end, frequency, variable_id,
    value_raw, currency_raw, unit_raw, conversion_factor, value, currency, unit,
    value_status, carried_forward, source_currency_rebased, source_date,
    source_value_raw, source_currency, source_conversion_factor, punctuation_normalized,
    monthly_method, source_coverage_end, source_dataset_id, source_table, source_row,
    date_cell_raw, value_cell_raw, date_basis, source_url, revision_url, source_file,
    source_sha256, downloaded_at) %>% arrange(date, variable_id)

# Require complete monthly keys, valid conversion identities and source missingness parity.
if (nrow(monthly) != 2L * length(months) || anyDuplicated(monthly[c("date", "variable_id")]) ||
    anyNA(monthly$source_date) || anyNA(monthly$conversion_factor) ||
    any(is.na(monthly$value) != is.na(monthly$source_value_raw)) ||
    any(!is.na(monthly$value) & (!is.finite(monthly$value) | monthly$value < 0)) ||
    any(abs(monthly$value - monthly$value_raw * monthly$conversion_factor) >
      1e-12 * pmax(1, abs(monthly$value)), na.rm = TRUE)) {
  rlang::abort("Monthly wage coverage or conversion checks failed",
    class = "minimum_wage_monthly_data_error")
}
write_final_dataset(monthly, dataset_id)
message("Consolidated ", length(months), " months: ", min(months), " to ", max(months),
  "; ", sum(is.na(monthly$value)), " missing amounts retained")
