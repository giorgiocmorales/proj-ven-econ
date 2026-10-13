# Derive monthly and year-on-year rates from consecutive monthly CPI levels.
library(tidyverse)
source("R/source_helpers.R")
cpi <- readr::read_csv("data/processed/prices_03_cpi_groups_bcv.csv", show_col_types = FALSE)
if (anyDuplicated(cpi[c("series_id", "date")]) || anyNA(cpi$date) ||
    any(!is.finite(cpi$cpi_index) | cpi$cpi_index <= 0)) stop("Invalid CPI input")
for (id in c("headline", "food")) {
  dates <- sort(cpi$date[cpi$series_id == id])
  if (length(dates) < 13L || !identical(dates, seq(min(dates), max(dates), by = "month"))) {
    stop("Inflation calculations require consecutive monthly observations")
  }
}
inflation <- cpi %>% arrange(series_id, date) %>% group_by(series_id) %>%
  mutate(inflation_mom = 100 * (cpi_index / lag(cpi_index) - 1),
         inflation_yoy = 100 * (cpi_index / lag(cpi_index, 12L) - 1)) %>%
  ungroup() %>%
  pivot_longer(c(inflation_mom, inflation_yoy), names_to = "variable_id", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(unit = if_else(variable_id == "inflation_mom", "percent_mom", "percent_yoy"),
         source_dataset_id = dataset_id, dataset_id = "prices_04_inflation_groups_bcv")
if (any(!is.finite(inflation$value))) stop("Nonfinite derived inflation")
write_final_dataset(inflation, "prices_04_inflation_groups_bcv")
