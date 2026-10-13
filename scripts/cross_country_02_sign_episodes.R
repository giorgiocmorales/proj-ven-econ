# Build growth and primary-balance sign episodes from raw inputs, without thresholds.
# Run from the repository root after the raw-only download.
# Rscript scripts/cross_country_02_sign_episodes.R
library(tidyverse)
source("R/cross_country_episode_helpers.R")

## Historical inputs ----
# Sign periods describe growth/contraction and surplus/deficit. Inflation and debt
# remain in raw for later threshold analysis. Preserve estimates and exclude projections.
input_path <- "data/raw/cross_country_macro/cross_country_inputs.csv"
if (!file.exists(input_path)) {
  rlang::abort("Download cross_country_inputs.csv before building sign episodes.", class = "macro_cache_error")
}
observations <- read_csv(input_path, show_col_types = FALSE) %>%
  filter(variable %in% c("gdp_growth", "primary_balance_gdp"),
         period_class != "future_or_current_year_projection")
paths <- build_sign_episode_paths(observations)

## Episode summaries ----
# Duration counts qualifying annual observations, with no extra baseline year.
# Values remain in their original units; rates and ratios are not compounded.
episodes <- paths %>%
  filter(!is.na(episode_id)) %>%
  group_by(source, country_code, country, indicator_id, variable, unit,
           government_scope, episode_id, episode_number, episode_sign) %>%
  summarise(start_year = first(year), end_year = last(year), number_of_years = n(),
            start_value = first(value), end_value = last(value),
            min_value = min(value), max_value = max(value), mean_value = mean(value),
            boundary_reason = first(boundary_reason),
            includes_estimate_or_unknown = any(period_class %in%
              c("completed_year_estimate", "actual_status_unknown")), .groups = "drop") %>%
  arrange(source, country_code, variable, start_year)
if (any(episodes$number_of_years != episodes$end_year - episodes$start_year + 1L)) {
  rlang::abort("Sign episodes contain a calendar gap.", class = "macro_value_error")
}

## Interim outputs ----
# Missing rows remain in the path with no episode ID, allowing continuity audits.
output_dir <- "data/interim/cross_country_macro"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(paths, file.path(output_dir, "sign_episode_paths.csv"))
write_csv(episodes, file.path(output_dir, "sign_episode_summary.csv"))
message("Wrote ", nrow(episodes), " historical sign episodes to ", output_dir)
