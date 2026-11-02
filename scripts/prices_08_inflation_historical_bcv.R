# Derive Caracas inflation and a historical monthly reference with a labelled geography break.
# National report inputs remain separate; never calculate growth across different CPI levels.
library(dplyr)
library(tidyr)
library(readr)
source("R/source_helpers.R")

# Retain the complete Caracas series, including its overlap with national coverage.
caracas <- read_csv("data/final/prices_07_cpi_caracas_bcv.csv", show_col_types = FALSE) %>%
  arrange(date)
if (anyDuplicated(caracas$date) || any(!is.finite(caracas$cpi_index) | caracas$cpi_index <= 0) ||
    !identical(caracas$date, seq.Date(as.Date("1950-01-01"), max(caracas$date), by = "month"))) {
  rlang::abort("Caracas inflation requires positive consecutive monthly CPI levels",
    class = "historical_inflation_input_error")
}
caracas_inflation <- caracas %>% mutate(
  inflation_mom = 100 * (cpi_index / lag(cpi_index) - 1),
  inflation_yoy = 100 * (cpi_index / lag(cpi_index, 12L) - 1)) %>%
  pivot_longer(c(cpi_index, inflation_mom, inflation_yoy),
    names_to = "variable_id", values_to = "value") %>% filter(!is.na(value)) %>%
  mutate(unit = case_when(variable_id == "cpi_index" ~ "index_dec_2007_100",
    variable_id == "inflation_mom" ~ "percent_mom", variable_id == "inflation_yoy" ~ "percent_yoy"),
    series_id = "caracas_headline", source_dataset_id = dataset_id,
    dataset_id = "prices_08_inflation_caracas_bcv",
    transformation = case_when(variable_id == "cpi_index" ~ "identity",
      variable_id == "inflation_mom" ~ "percent_change_from_cpi_index",
      variable_id == "inflation_yoy" ~ "percent_change_from_cpi_index_lag_12")) %>%
  arrange(variable_id, date)

# Prefer the newer national group extract on overlap, after reconciling both vintages.
national_path <- "data/final/prices_02_inflation_bcv.csv"
groups_path <- "data/final/prices_04_inflation_groups_bcv.csv"
national <- read_csv(national_path, show_col_types = FALSE) %>%
  filter(variable_id == "inflation_mom") %>% mutate(source_file = national_path,
    source_md5 = unname(tools::md5sum(national_path)), priority = 1L)
groups <- read_csv(groups_path, show_col_types = FALSE) %>%
  filter(series_id == "headline", variable_id == "inflation_mom") %>%
  mutate(source_file = groups_path, source_md5 = unname(tools::md5sum(groups_path)), priority = 2L)
for (source in list(national, groups)) {
  if (!nrow(source) || anyNA(source$date) || anyDuplicated(source$date) ||
      any(!is.finite(source$value) | source$value <= -100)) {
    rlang::abort("Invalid national monthly inflation source", class = "historical_inflation_input_error")
  }
}
national_overlap <- national %>% select(date, older_value = value) %>%
  inner_join(groups %>% select(date, newer_value = value), by = "date", relationship = "one-to-one") %>%
  mutate(difference_pp = newer_value - older_value)
if (!nrow(national_overlap) || any(abs(national_overlap$difference_pp) > 1e-8)) {
  rlang::abort("National inflation vintages differ; review overlap before building history",
    class = "historical_inflation_vintage_error")
}
national <- bind_rows(national, groups) %>% group_by(date) %>%
  slice_max(priority, n = 1L, with_ties = FALSE) %>% ungroup() %>%
  mutate(source_dataset_id = dataset_id, geography = "venezuela_national", series_id = "headline",
    transformation = "percent_change_from_national_cpi_index", purpose = "historical_reference")

# Select existing monthly rates at the coverage boundary; this is not a homogeneous CPI.
# January 1950 has an index but no preceding index, so monthly inflation starts in February.
boundary <- as.Date("2008-01-01")
reference_columns <- c("date", "variable_id", "value", "unit", "frequency", "series_id", "geography",
  "provisional", "source_id", "source_url", "source_file", "source_md5", "source_dataset_id",
  "transformation", "purpose")
history <- bind_rows(
  caracas_inflation %>% filter(variable_id == "inflation_mom", date < boundary) %>%
    mutate(source_dataset_id = dataset_id) %>% select(all_of(reference_columns)),
  national %>% filter(date >= boundary) %>% select(all_of(reference_columns))) %>%
  arrange(date) %>% mutate(dataset_id = "prices_08_inflation_historical_bcv",
    geography_break = date == boundary,
    coverage_note = if_else(date < boundary, "Caracas metropolitan area",
      "National CPI; geographic coverage changes in January 2008"))
if (anyDuplicated(caracas_inflation[c("variable_id", "date")]) ||
    any(!is.finite(caracas_inflation$value)) || anyDuplicated(history$date) ||
    any(!is.finite(history$value) | history$value <= -100) ||
    !identical(history$date, seq.Date(as.Date("1950-02-01"), max(national$date), by = "month"))) {
  rlang::abort("Historical inflation failed value, unique-key, or monthly coverage checks",
    class = "historical_inflation_output_error")
}

# Keep discrepancies, native Caracas observations, and the labelled reference inspectable.
write_interim_dataset(national_overlap, "prices_08_national_inflation_overlap")
write_final_dataset(caracas_inflation, "prices_08_inflation_caracas_bcv")
write_final_dataset(history, "prices_08_inflation_historical_bcv")
message("Historical monthly reference: ", min(history$date), " to ", max(history$date),
  "; geography changes from Caracas to national CPI at ", boundary)
