# Fetch comparable general-government fiscal components from one IMF WEO snapshot.
library(tidyverse)
library(rsdmx)
source("R/source_helpers.R")
options(timeout = 45)
dataset_id <- "fiscal_05_components_imf"
indicator_map <- tribble(
  ~target_variable, ~flow_id, ~country, ~indicator_code, ~key_extra, ~frequency,
  "general_government_revenue_gdp", "WEO", "VEN", "GGR_NGDP", NA_character_, "A",
  "general_government_expenditure_gdp", "WEO", "VEN", "GGX_NGDP", NA_character_, "A",
  "general_government_balance_gdp", "WEO", "VEN", "GGXCNL_NGDP", NA_character_, "A"
)
result <- fetch_imf_indicator_set(dataset_id, "imf", indicator_map,
  start_year = parse_year_env("START_YEAR", 1999))
if (nrow(result$errors) || !setequal(unique(result$data$INDICATOR), indicator_map$indicator_code)) {
  write_raw_dataset(result$errors, paste0(dataset_id, "_errors"))
  stop("All three IMF fiscal components are required; existing data were preserved")
}
if (anyDuplicated(result$data[c("INDICATOR", "TIME_PERIOD")]) ||
    any(!is.finite(result$data$OBS_VALUE))) stop("Invalid IMF fiscal observations")
write_api_source_result(result, dataset_id)
