# Offline numerical checks of the saved estimated daily/weekly inflation outputs.
library(tidyverse)
daily <- readr::read_csv("data/final/prices_05_daily_cpi_bcv.csv", show_col_types = FALSE)
weekly <- readr::read_csv("data/final/prices_06_weekly_inflation_bcv.csv", show_col_types = FALSE)
components <- readr::read_csv("data/interim/prices_06_weekly_month_components.csv", show_col_types = FALSE)
coverage <- readr::read_csv("data/interim/prices_06_weekly_coverage.csv", show_col_types = FALSE)
stopifnot(all(wday(weekly$week_start) == 7L), all(wday(weekly$week_end) == 6L),
  all(weekly$days_covered == 7L), all(weekly$is_estimated), !anyDuplicated(weekly$date))
monthly <- daily %>% group_by(month) %>% summarise(n = n(), days = first(days_in_month),
  observed = first(monthly_inflation_pct), calculated = 100 * (prod(1 + daily_inflation_pct / 100) - 1),
  .groups = "drop")
stopifnot(all(monthly$n == monthly$days),
  all(abs(monthly$calculated - monthly$observed) < 1e-8),
  any(monthly$days == 28), any(monthly$days == 29), any(monthly$days == 30), any(monthly$days == 31))
# Independent cross-month formula: product of monthly factors raised to day fractions.
check <- components %>% group_by(week_end) %>% summarise(
  expected = 100 * (prod((1 + monthly_inflation_pct / 100)^(days_from_month / days_in_month)) - 1),
  .groups = "drop") %>% inner_join(weekly, by = c("week_end" = "date"))
stopifnot(nrow(check) == nrow(weekly), any(check$months_in_week == 2L),
  all(abs(check$expected - check$value) < 1e-8),
  !any(coverage$week_end[!coverage$complete_week] %in% weekly$date))
# A full week's compounded rate also equals the end/start daily CPI ratio.
ratios <- weekly %>% left_join(daily %>% select(date, closing_cpi = daily_cpi_estimated), by = "date") %>%
  mutate(opening_date = week_start - days(1)) %>%
  inner_join(daily %>% select(opening_date = date, opening_cpi = daily_cpi_estimated), by = "opening_date")
stopifnot(all(abs(100 * (ratios$closing_cpi / ratios$opening_cpi - 1) - ratios$value) < 1e-8))
cat("Passed: monthly reconstruction, 28/29/30/31-day months, mixed-month weeks, week boundaries and CPI ratios.\n")
