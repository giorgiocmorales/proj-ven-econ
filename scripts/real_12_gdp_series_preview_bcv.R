# Preview consolidated GDP and reconcile quarter contributions with calendar-year growth.
# BCV's original quarterly value is a YoY rate; GDP index levels are reconstructed.
library(tidyverse)
source("R/source_helpers.R")
ensure_project_dirs()

# Read existing vintages without downloading new files or mixing historical price bases.
quarterly <- read_csv("data/final/real_10_gdp_quarterly_bcv.csv", show_col_types = FALSE) %>%
  filter(family == "activity", series_label == "Consolidado", between(year, 2000L, 2026L)) %>%
  arrange(year, quarter)
original <- read_csv("data/final/real_03_gdp_q_a_bcv.csv", show_col_types = FALSE) %>%
  filter(series_label == "Consolidado") %>%
  select(year, quarter, original_bcv_quarterly_value = value)
annual <- read_csv("data/final/real_10_gdp_annual_bcv.csv", show_col_types = FALSE) %>%
  filter(family == "activity", series_label == "Consolidado") %>%
  select(year, annual_gdp_index = gdp_index, annual_bcv_yoy_percent = published_yoy_percent)
if (!nrow(quarterly) || anyDuplicated(quarterly[c("year", "quarter")]) ||
    anyDuplicated(original[c("year", "quarter")]) || anyDuplicated(annual$year)) {
  rlang::abort("GDP preview requires nonempty, unique source periods", class = "gdp_preview_input_error")
}

# Quarter contribution = prior-year quarter weight * that quarter's YoY rate (pp).
# Cumulative contributions use a full-year denominator; YTD uses matched quarters only.
preview <- quarterly %>% left_join(original, by = c("year", "quarter")) %>%
  left_join(annual, by = "year") %>% group_by(year) %>% mutate(
    quarter_contribution_to_annual_growth_pp = previous_year_annual_weight * reconstructed_yoy_percent,
    cumulative_contribution_pp = cumsum(quarter_contribution_to_annual_growth_pp),
    cumulative_previous_year_weight = cumsum(previous_year_annual_weight),
    ytd_from_contributions_percent = cumulative_contribution_pp / cumulative_previous_year_weight
  ) %>% ungroup()
if (anyNA(preview$original_bcv_quarterly_value) ||
    any(abs(preview$original_bcv_quarterly_value - preview$reconstructed_yoy_percent) > 1e-6) ||
    any(abs(preview$ytd_from_contributions_percent - preview$ytd_yoy_percent) > 1e-6)) {
  rlang::abort("Quarterly rates or YTD contribution identities fail", class = "gdp_preview_reconciliation_error")
}

# At Q4, quarterly indices sum to the annual index and both growth measures match BCV.
checks <- preview %>% group_by(year) %>% summarise(
  n_quarters = n(), index_sum = sum(gdp_index), contribution_sum = sum(quarter_contribution_to_annual_growth_pp),
  q4_ytd = last(ytd_yoy_percent), annual_gdp_index = first(annual_gdp_index),
  annual_bcv_yoy_percent = first(annual_bcv_yoy_percent), .groups = "drop"
) %>% filter(n_quarters == 4L)
if (anyNA(checks) || any(abs(checks$index_sum - checks$annual_gdp_index) > 1e-6) ||
    any(abs(checks$contribution_sum - checks$annual_bcv_yoy_percent) > 1e-6) ||
    any(abs(checks$q4_ytd - checks$annual_bcv_yoy_percent) > 1e-6)) {
  rlang::abort("Quarter totals fail annual BCV reconciliation", class = "gdp_preview_reconciliation_error")
}

# Include every requested quarter explicitly; missing source observations remain NA.
preview <- crossing(year = 2000:2026, quarter = 1:4) %>%
  left_join(preview %>% select(-date), by = c("year", "quarter")) %>% mutate(
    date = lubridate::make_date(year, 3L * quarter - 2L, 1L),
    period = paste0(year, " Q", quarter),
    availability = case_when(
      !is.na(gdp_index) ~ "available",
      year < 2008L ~ "quarterly data unavailable in current extracts",
      TRUE ~ "not in downloaded extract"
    )
  ) %>% select(period, date, availability, year, quarter, original_bcv_quarterly_value,
    gdp_index, reconstructed_yoy_percent, qoq_unadjusted_percent,
    quarter_contribution_to_annual_growth_pp, cumulative_contribution_pp, ytd_yoy_percent,
    rolling_four_quarter_yoy_percent, previous_year_annual_weight,
    annual_bcv_yoy_percent, annual_gdp_index, everything())
readr::write_csv(preview, "outputs/data/real_12_gdp_series_preview_bcv.csv")

# Keep earlier annual observations separate and explicitly label their 1997 price base.
historical <- read_csv("data/final/real_09_gdp_historical_bcv.csv", show_col_types = FALSE) %>%
  filter(base_year == 1997L, between(year, 2000L, 2007L)) %>%
  select(year, historical_annual_yoy_percent = growth_percent, price_base_year = base_year,
         source_url, downloaded_at)
write_csv(historical, "data/interim/real_12_gdp_preview_historical_annual.csv")

# Emit a compact full-coverage table for review; CSV output retains full precision.
display <- preview %>% filter(availability == "available") %>% transmute(
  period, index = gdp_index, bcv_yoy = original_bcv_quarterly_value,
  qoq = qoq_unadjusted_percent, contribution_pp = quarter_contribution_to_annual_growth_pp,
  ytd = ytd_yoy_percent
)
cat(knitr::kable(display, format = "pipe", digits = 2), sep = "\n")
cat("\nHistorical annual growth only (1997 prices):\n")
cat(knitr::kable(historical %>% select(year, historical_annual_yoy_percent),
                 format = "pipe", digits = 2), sep = "\n")
message("Verified ", nrow(checks), " complete calendar years; preview includes all 108 requested quarters.")
