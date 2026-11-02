# Summarize country coverage, sign episodes, and strict threshold runs separately.
# Run after cross_country_02_sign_episodes.R; no API refresh is required.
library(tidyverse)
source("R/cross_country_episode_helpers.R")

## Inputs and definitions ----
# Use published completed years and labelled WEO estimates, excluding projections.
output_dir <- "data/interim/cross_country_macro"
observations <- read_csv("data/raw/cross_country_macro/cross_country_inputs.csv", show_col_types = FALSE) %>%
  filter(period_class != "future_or_current_year_projection")
episodes <- read_csv(file.path(output_dir, "sign_episode_summary.csv"), show_col_types = FALSE)
paths <- read_csv(file.path(output_dir, "sign_episode_paths.csv"), show_col_types = FALSE)
thresholds <- tribble(~variable, ~threshold,
                     "gdp_growth", 8, "primary_balance_gdp", 10,
                     "inflation", 200, "debt_gdp", 240)

## Country coverage and sign episode counts ----
# Count only countries with at least one nonmissing historical observation.
coverage <- observations %>% filter(!is.na(value)) %>%
  group_by(source, variable) %>%
  summarise(countries = n_distinct(country_code), observations = n(),
            first_year = min(year), last_year = max(year), .groups = "drop")
sign_counts <- episodes %>% group_by(source, variable) %>%
  summarise(sign_episodes = n(), positive_episodes = sum(episode_sign == "positive"),
            negative_episodes = sum(episode_sign == "negative"), zero_episodes = sum(episode_sign == "zero"),
            .groups = "drop")
overview <- coverage %>% left_join(sign_counts, by = c("source", "variable"))

## Threshold runs within annual histories ----
# Reuse continuity segmentation on the pass/fail state, keeping actual values intact.
# One maximal run counts once; overlapping ten-year windows are not extra episodes.
threshold_paths <- observations %>% inner_join(thresholds, by = "variable") %>%
  mutate(original_value = value, value = case_when(is.na(value) ~ NA_real_,
                                                   value > threshold ~ 1, TRUE ~ -1)) %>%
  build_sign_episode_paths() %>%
  rename(threshold_run_id = episode_id) %>%
  left_join(paths %>% select(source, country_code, indicator_id, year, parent_sign_episode_id = episode_id),
            by = c("source", "country_code", "indicator_id", "year"))
runs <- threshold_paths %>% filter(episode_sign == "positive") %>%
  group_by(source, country_code, country, variable, threshold, threshold_run_id) %>%
  summarise(start_year = first(year), end_year = last(year), number_of_years = n(),
            min_value = min(original_value), max_value = max(original_value),
            parent_sign_episode_id = first(parent_sign_episode_id),
            includes_estimate_or_unknown = any(period_class %in%
              c("completed_year_estimate", "actual_status_unknown")), .groups = "drop")
stopifnot(all(runs$min_value > runs$threshold),
          all(runs$number_of_years == runs$end_year - runs$start_year + 1L))
run_counts <- runs %>% group_by(source, variable) %>%
  summarise(runs_10_plus = sum(number_of_years >= 10), runs_11_plus = sum(number_of_years >= 11),
            countries_10_plus = n_distinct(country_code[number_of_years >= 10]),
            countries_11_plus = n_distinct(country_code[number_of_years >= 11]),
            longest_threshold_run = max(number_of_years), .groups = "drop")

## Coverage denominators and whole sign episode comparisons ----
# Count countries with sufficiently long uninterrupted data, regardless of threshold.
available_runs <- observations %>% mutate(value = if_else(is.na(value), NA_real_, 1)) %>%
  build_sign_episode_paths() %>% filter(!is.na(episode_id)) %>%
  count(source, variable, country_code, episode_id, name = "available_years")
eligible <- available_runs %>% group_by(source, variable) %>%
  summarise(countries_with_10_year_coverage = n_distinct(country_code[available_years >= 10]),
            countries_with_11_year_coverage = n_distinct(country_code[available_years >= 11]), .groups = "drop")
threshold_summary <- coverage %>% inner_join(thresholds, by = "variable") %>%
  left_join(eligible, by = c("source", "variable")) %>%
  left_join(run_counts, by = c("source", "variable")) %>%
  mutate(across(c(runs_10_plus, runs_11_plus, countries_10_plus, countries_11_plus, longest_threshold_run),
                ~ replace_na(.x, 0L)))
whole_episode_counts <- episodes %>% inner_join(thresholds, by = "variable") %>%
  group_by(source, variable) %>%
  summarise(whole_sign_episodes_10_plus = sum(number_of_years >= 10 & min_value > threshold),
            whole_sign_episodes_11_plus = sum(number_of_years >= 11 & min_value > threshold), .groups = "drop")
containing_episode_counts <- runs %>% filter(!is.na(parent_sign_episode_id)) %>%
  group_by(source, variable) %>%
  summarise(sign_episodes_containing_10_year_run = n_distinct(parent_sign_episode_id[number_of_years >= 10]),
            sign_episodes_containing_11_year_run = n_distinct(parent_sign_episode_id[number_of_years >= 11]),
            .groups = "drop")
sign_threshold_counts <- sign_counts %>%
  left_join(whole_episode_counts, by = c("source", "variable")) %>%
  left_join(containing_episode_counts, by = c("source", "variable")) %>%
  mutate(across(starts_with("sign_episodes_containing"), ~ replace_na(.x, 0L)))

## Inspectable results ----
# Persist summaries and underlying runs so country-period membership can be reviewed.
write_csv(overview, file.path(output_dir, "episode_overview.csv"))
write_csv(threshold_summary, file.path(output_dir, "threshold_summary.csv"))
write_csv(runs, file.path(output_dir, "threshold_runs.csv"))
write_csv(sign_threshold_counts, file.path(output_dir, "sign_threshold_counts.csv"))
print(overview, width = Inf)
print(threshold_summary, width = Inf)
print(sign_threshold_counts, width = Inf)
print(runs %>% filter(number_of_years >= 10), n = Inf, width = Inf)
message("Unique countries/economies with historical data: ",
        observations %>% filter(!is.na(value)) %>% pull(country_code) %>% n_distinct())
