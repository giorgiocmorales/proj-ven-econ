# Publish monthly crude-price views from the validated, revision-aware MOMR corpus.
# Keep the existing Brent variable ID for downstream report compatibility.
library(tidyverse)
source("R/source_helpers.R")
ensure_project_dirs()

oil_prices <- read_csv("data/processed/oil_13_momr_corpus.csv", show_col_types = FALSE,
                       col_types = cols(footnote_marker = col_character()))
readr::stop_for_problems(oil_prices)
oil_prices <- oil_prices %>%
  filter(.data$source_type == "price") %>%
  mutate(dataset_id = "oil_02_price_opec",
         variable_id = if_else(.data$series_id == "brent", "north_sea_dated", .data$series_id),
         period = format(.data$date, "%Y-%m"), period_type = "monthly",
         source_url = "https://www.opec.org/monthly-oil-market-report.html")
if (nrow(oil_prices) == 0L) rlang::abort("No crude prices in the corpus", class = "opec_view_error")
write_processed_dataset(oil_prices, "oil_02_price_opec")

# Preserve the raw printed label alongside the harmonized benchmark identity.
oil_prices %>% arrange(.data$report_month) %>%
  group_by(.data$variable_id, .data$period_type, .data$date) %>%
  slice_tail(n = 1L) %>% ungroup() %>%
  write_final_dataset("oil_02_price_opec_latest")
