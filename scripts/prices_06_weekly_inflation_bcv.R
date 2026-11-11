# Estimated headline inflation for Saturday-Friday weeks; monthly data are untouched.
# Daily rates are constant within each source month and compound to its published change.
library(tidyverse)
daily_path <- "data/final/prices_05_daily_cpi_bcv.csv"
daily <- readr::read_csv(daily_path, show_col_types = FALSE) %>% arrange(date)
if (!nrow(daily) || anyNA(daily$date) || anyDuplicated(daily$date) ||
    any(!is.finite(daily$daily_log_factor)) ||
    any(as.numeric(diff(daily$date)) != 1)) {
  stop("Weekly inflation requires unique consecutive daily estimates")
}
daily <- daily %>% mutate(
  week_end = floor_date(date + days(2), "week", week_start = 1) + days(4),
  week_start = week_end - days(6))

# Retain each month's contribution to weeks that cross month boundaries.
components <- daily %>% group_by(week_start, week_end, month) %>%
  summarise(days_from_month = n(), days_in_month = first(days_in_month),
    monthly_inflation_pct = first(monthly_inflation_pct),
    daily_inflation_pct = first(daily_inflation_pct),
    component_log_factor = sum(daily_log_factor),
    component_factor = exp(component_log_factor),
    first_covered_date = min(date), last_covered_date = max(date),
    provisional = any(provisional %in% TRUE | previous_provisional %in% TRUE),
    .groups = "drop")
coverage <- components %>% group_by(week_start, week_end) %>%
  summarise(first_covered_date = min(first_covered_date), last_covered_date = max(last_covered_date),
    days_covered = sum(days_from_month), months_in_week = n(),
    weekly_log_factor = sum(component_log_factor),
    provisional = any(provisional),
    .groups = "drop") %>%
  mutate(complete_week = days_covered == 7L & first_covered_date == week_start & last_covered_date == week_end)
weekly <- coverage %>% filter(complete_week) %>%
  transmute(date = week_end, week_start, week_end, days_covered, months_in_week,
    variable_id = "inflation_weekly_estimated", series_id = "headline",
    value = 100 * expm1(weekly_log_factor), unit = "percent_weekly", frequency = "weekly",
    is_estimated = TRUE, provisional, source_id = "bcv",
    source_dataset_id = "prices_05_daily_cpi_bcv", source_file = daily_path,
    dataset_id = "prices_06_weekly_inflation_bcv",
    method = "constant_daily_compounding_by_month; Saturday-Friday; seven_calendar_days")
if (!nrow(weekly) || anyDuplicated(weekly$date) || any(!is.finite(weekly$value)) ||
    any(weekly$value <= -100)) stop("Invalid derived weekly inflation")
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(components, "data/interim/prices_06_weekly_month_components.csv")
readr::write_csv(coverage, "data/interim/prices_06_weekly_coverage.csv")
readr::write_csv(weekly, "data/final/prices_06_weekly_inflation_bcv.csv")
message("Published ", nrow(weekly), " complete estimated weeks; ", sum(!coverage$complete_week),
        " incomplete boundary weeks retained only in the coverage audit.")
