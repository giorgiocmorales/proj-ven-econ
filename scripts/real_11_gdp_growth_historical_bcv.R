# Consolidate annual real GDP YoY growth for historical reference only.
# Run after the annual activity and historical BCV importers; no levels are spliced.
library(tidyverse)

rm(list = ls())
source("R/source_helpers.R")

# Retain source vintages and price bases so each selected rate remains auditable.
dataset_id <- "real_11_gdp_growth_historical_bcv"
historical_path <- "data/processed/real_09_gdp_historical_bcv.csv"
annual_path <- "data/processed/real_01_gdp_a_a_bcv.csv"
historical <- read_csv(historical_path, show_col_types = FALSE)
annual <- read_csv(annual_path, show_col_types = FALSE) %>%
  filter(.data$series_label == "Consolidado", .data$frequency == "annual",
         .data$unit == "percent_yoy")

# Reject ambiguous source keys and missing recent rates before selecting observations.
if (nrow(annual) == 0L || anyNA(annual$year) ||
    anyDuplicated(annual$year) > 0L || any(!is.finite(annual$value)) ||
    anyNA(historical$year) || anyNA(historical$base_year) ||
    anyDuplicated(historical[c("base_year", "year")]) > 0L) {
  rlang::abort("Invalid BCV GDP source keys or annual rates",
               class = "gdp_history_growth_input_error")
}

# Use within-base growth; a base's first level has no rate and is not a candidate.
# At rebase boundaries the preceding base supplies the valid annual comparison.
historical_growth <- historical %>%
  filter(!is.na(.data$growth_percent)) %>%
  group_by(.data$year) %>%
  slice_max(order_by = .data$base_year, n = 1L, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(year, date, value = .data$growth_percent, base_year, provisional,
            source_id, source_url, sheet_name, downloaded_at,
            source_file = historical_path, method = "within_base_level_growth")
annual_growth <- annual %>%
  transmute(year, date, value, base_year = 1997L, provisional,
            source_id, source_url, sheet_name, downloaded_at,
            source_file = annual_path, method = "published_annual_growth")

# Preserve vintage differences in the overlap; published recent rates take priority.
overlap <- historical_growth %>%
  select(year, historical_growth_percent = value) %>%
  inner_join(annual_growth %>% select(year, annual_growth_percent = value),
             by = "year", relationship = "one-to-one") %>%
  mutate(difference_pp = .data$annual_growth_percent - .data$historical_growth_percent)
gdp_growth <- bind_rows(
  historical_growth %>% filter(.data$year < min(annual_growth$year)),
  annual_growth
) %>%
  arrange(.data$year) %>%
  mutate(dataset_id = dataset_id, series_label = "Consolidado",
         frequency = "annual", unit = "percent_yoy", purpose = "historical_reference")

# Require uninterrupted finite annual rates from the first calculable historical year.
expected_years <- seq.int(min(historical$year) + 1L, max(annual$year))
if (!identical(as.integer(gdp_growth$year), expected_years) ||
    any(!is.finite(gdp_growth$value)) || any(gdp_growth$value <= -100) ||
    anyNA(gdp_growth$date) ||
    any(gdp_growth$date != lubridate::make_date(gdp_growth$year, 1L, 1L))) {
  rlang::abort("Historical GDP growth must contain one valid rate per consecutive year",
               class = "gdp_history_growth_coverage_error")
}

# Export a separate reference series and the overlap audit after all checks pass.
write_interim_dataset(overlap, "real_11_gdp_growth_historical_overlap")
write_final_dataset(gdp_growth, dataset_id)
