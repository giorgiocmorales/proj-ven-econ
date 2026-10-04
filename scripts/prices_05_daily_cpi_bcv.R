# Estimated daily CPI path: constant compound daily inflation within each month.
# Published monthly indices are treated as month-end anchors for this allocation.
library(tidyverse)
cpi_path <- if (file.exists("data/processed/prices_03_cpi_groups_bcv.csv"))
  "data/processed/prices_03_cpi_groups_bcv.csv" else "data/processed/prices_01_cpi_bcv.csv"
cpi <- readr::read_csv(cpi_path, show_col_types = FALSE)
if ("series_id" %in% names(cpi)) cpi <- cpi %>% filter(series_id == "headline")
cpi <- cpi %>% select(month = date, cpi_index, provisional) %>% arrange(month)
if (anyDuplicated(cpi$month) || anyNA(cpi$month) ||
    any(!is.finite(cpi$cpi_index) | cpi$cpi_index <= 0) ||
    !identical(cpi$month, seq(min(cpi$month), max(cpi$month), by = "month"))) {
  stop("Daily CPI allocation requires positive consecutive monthly indices")
}
daily_cpi <- cpi %>%
  mutate(previous_cpi = lag(cpi_index), previous_provisional = lag(provisional),
    days_in_month = lubridate::days_in_month(month),
    monthly_log_factor = log(cpi_index / previous_cpi),
    daily_log_factor = monthly_log_factor / days_in_month,
    monthly_inflation_pct = 100 * expm1(monthly_log_factor),
    daily_inflation_pct = 100 * expm1(daily_log_factor)) %>%
  filter(!is.na(previous_cpi)) %>%
  tidyr::uncount(days_in_month, .remove = FALSE, .id = "day_in_month") %>%
  mutate(date = month + days(day_in_month - 1L),
    daily_cpi_estimated = previous_cpi * exp(daily_log_factor * day_in_month),
    source_file = cpi_path, source_id = "bcv",
    method = "constant_daily_compounding_with_month_end_anchors") %>%
  select(date, month, day_in_month, days_in_month, daily_cpi_estimated,
    daily_inflation_pct, daily_log_factor, monthly_inflation_pct,
    cpi_index, previous_cpi, provisional, previous_provisional, source_id, source_file, method)
monthly_check <- daily_cpi %>% group_by(month) %>%
  summarise(days = n(), expected_days = first(days_in_month),
    compounded_factor = exp(sum(daily_log_factor)),
    published_factor = first(cpi_index / previous_cpi),
    closing_index = last(daily_cpi_estimated), published_index = first(cpi_index), .groups = "drop")
if (any(monthly_check$days != monthly_check$expected_days) ||
    any(abs(monthly_check$compounded_factor / monthly_check$published_factor - 1) > 1e-10) ||
    any(abs(monthly_check$closing_index / monthly_check$published_index - 1) > 1e-10)) {
  stop("Daily compounding failed to reproduce published monthly CPI")
}
readr::write_csv(daily_cpi, "data/final/prices_05_daily_cpi_bcv.csv")
readr::write_csv(monthly_check, "data/interim/prices_05_daily_cpi_reconciliation.csv")
message("Daily CPI reconciles for ", nrow(monthly_check), " months, including actual month lengths.")
