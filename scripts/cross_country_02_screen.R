# Screen annual country histories against the thresholds in the supplied request.
# Run after cross_country_01_download.R; this script uses cached extracts only.
# Missing values and missing calendar years break runs. Thresholds are strict >.
# Report both 10+ years and the literal "more than 10" interpretation (11+).
# Debt stability and sovereign borrowing costs remain undefined in this exercise.

## Inputs and threshold definitions ----
library(tidyverse)
final_dir <- "data/final/cross_country_macro"
historical <- read_csv(file.path(final_dir, "historical_observations.csv"), show_col_types = FALSE)
panel <- read_csv(file.path(final_dir, "country_year_panel.csv"), show_col_types = FALSE)
thresholds <- tribble(
  ~variable, ~condition, ~threshold,
  "gdp_growth", "gdp_growth_gt_8", 8,
  "inflation", "inflation_gt_200", 200,
  "primary_balance_gdp", "primary_surplus_gt_10", 10,
  "debt_gdp", "debt_gdp_gt_240", 240
)
if (anyDuplicated(historical[c("source", "country_code", "variable", "year")])) {
  rlang::abort("Screen input contains duplicate annual observations.", class = "macro_duplicate_error")
}

## Historical samples ----
# Completed years include clearly labelled WEO estimates. The sensitivity sample
# admits only WEO reported actuals and published WB observations for completed years.
# Data360 status labels do not provide WEO-style last-actual-year boundaries.
samples <- bind_rows(
  historical %>% mutate(sample = "completed_years"),
  historical %>% mutate(sample = "reported_actual_sensitivity",
                         value = if_else(source == "IMF_WEO" & period_class != "reported_actual",
                                         NA_real_, value))
)
single_conditions <- samples %>% inner_join(thresholds, by = "variable") %>%
  transmute(sample, source, country_code, country, year, condition, value,
            available = !is.na(value), meets = !is.na(value) & value > threshold,
            includes_estimate = period_class %in% c("completed_year_estimate", "actual_status_unknown"))

# Joint tests use three WEO series in the same year, without mixing government scopes.
joint <- samples %>%
  filter(source == "IMF_WEO", variable %in% c("gdp_growth", "primary_balance_gdp", "debt_gdp")) %>%
  group_by(sample, source, country_code, country, year) %>%
  mutate(includes_estimate = any(period_class %in% c("completed_year_estimate", "actual_status_unknown"))) %>%
  ungroup() %>% select(sample, source, country_code, country, year, includes_estimate, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  transmute(sample, source, country_code, country, year,
            condition = "joint_growth_primary_debt_thresholds", value = NA_real_,
            available = !is.na(gdp_growth) & !is.na(primary_balance_gdp) & !is.na(debt_gdp),
            meets = available & gdp_growth > 8 & primary_balance_gdp > 10 & debt_gdp > 240,
            includes_estimate)
conditions <- bind_rows(single_conditions, joint) %>%
  arrange(sample, source, country_code, condition, year)

## Consecutive runs ----
# A new run starts on a state change or calendar gap. Missing data is its own state.
runs <- conditions %>% group_by(sample, source, country_code, country, condition) %>%
  mutate(state = case_when(!available ~ "missing", meets ~ "meets", TRUE ~ "below_threshold"),
         run_id = cumsum(row_number() == 1L | year != lag(year, default = first(year)) + 1L |
                           state != lag(state, default = first(state)))) %>%
  group_by(sample, source, country_code, country, condition, run_id, state) %>%
  summarise(start_year = min(year), end_year = max(year), consecutive_years = n(),
            min_value = if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
            max_value = if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
            includes_estimate = any(includes_estimate), .groups = "drop")
episodes <- runs %>% filter(state == "meets", consecutive_years >= 10L) %>%
  mutate(qualifies_10_plus = TRUE, qualifies_11_plus = consecutive_years >= 11L,
         episode_id = paste(sample, source, country_code, condition, start_year, sep = ":"))
write_csv(episodes, file.path(final_dir, "qualifying_episodes.csv"))

# Preserve the maximum run even when no country passes the ten-year screen.
max_runs <- runs %>% group_by(sample, source, country_code, country, condition) %>%
  summarise(longest_threshold_run = max(c(0L, consecutive_years[state == "meets"])), .groups = "drop")
write_csv(max_runs, file.path(final_dir, "longest_threshold_runs.csv"))

## Coverage denominators ----
# Count eligible uninterrupted observation windows, independent of threshold success.
coverage_runs <- conditions %>% group_by(sample, source, country_code, country, condition) %>%
  mutate(run_id = cumsum(row_number() == 1L | year != lag(year, default = first(year)) + 1L |
                           available != lag(available, default = first(available)))) %>%
  group_by(sample, source, country_code, country, condition, run_id, available) %>%
  summarise(consecutive_years = n(), .groups = "drop") %>% filter(available)
denominators <- coverage_runs %>% group_by(sample, source, country_code, condition) %>%
  summarise(eligible_windows_10 = sum(pmax(0L, consecutive_years - 9L)),
            eligible_windows_11 = sum(pmax(0L, consecutive_years - 10L)), .groups = "drop")
summary <- max_runs %>% left_join(denominators, by = c("sample", "source", "country_code", "condition")) %>%
  group_by(sample, source, condition) %>%
  summarise(countries_with_10_year_coverage = sum(eligible_windows_10 > 0, na.rm = TRUE),
            countries_with_11_year_coverage = sum(eligible_windows_11 > 0, na.rm = TRUE),
            countries_qualifying_10_plus = sum(longest_threshold_run >= 10),
            countries_qualifying_11_plus = sum(longest_threshold_run >= 11),
            longest_threshold_run = max(longest_threshold_run),
            eligible_windows_10 = sum(eligible_windows_10, na.rm = TRUE),
            eligible_windows_11 = sum(eligible_windows_11, na.rm = TRUE), .groups = "drop")
write_csv(summary, file.path(final_dir, "screen_summary.csv"))

## Episode paths and debt diagnostics ----
# Export all four variables within each episode and quantify debt movements without
# imposing an unrequested stability tolerance or treating overlapping windows as independent.
paths <- episodes %>% select(episode_id, sample, source, country_code, condition, start_year, end_year) %>%
  inner_join(panel, by = c("source", "country_code"), relationship = "many-to-many") %>%
  filter(year >= start_year, year <= end_year) %>% arrange(episode_id, year)
write_csv(paths, file.path(final_dir, "episode_paths.csv"))
debt_diagnostics <- paths %>% filter(condition %in% c("debt_gdp_gt_240", "joint_growth_primary_debt_thresholds")) %>%
  group_by(episode_id, sample, source, country_code, country, condition) %>%
  summarise(start_year = if (n() == 0L) NA_integer_ else min(year),
            end_year = if (n() == 0L) NA_integer_ else max(year),
            debt_start = first(debt_gdp), debt_end = last(debt_gdp),
            debt_change_pp = last(debt_gdp) - first(debt_gdp),
            debt_min = if (n() == 0L) NA_real_ else min(debt_gdp),
            debt_max = if (n() == 0L) NA_real_ else max(debt_gdp),
            largest_annual_change_pp = if (n() < 2L) NA_real_ else
              max(abs(debt_gdp - lag(debt_gdp)), na.rm = TRUE),
            .groups = "drop")
write_csv(debt_diagnostics, file.path(final_dir, "debt_episode_diagnostics.csv"))

# Select the earliest starting source series per country/variable without splicing it.
# This catalog supports a longest-history comparison while keeping source differences visible.
coverage <- read_csv(file.path(final_dir, "coverage_by_country.csv"), show_col_types = FALSE)
selection <- coverage %>% filter(observed_years > 0) %>%
  group_by(country_code, variable) %>%
  arrange(first_year, desc(observed_years), desc(last_year), source, .by_group = TRUE) %>%
  slice_head(n = 1) %>% ungroup()
write_csv(selection, file.path(final_dir, "earliest_history_source_selection.csv"))
print(summary %>% filter(sample == "completed_years"), width = Inf)
message("Wrote threshold screens to ", final_dir)
