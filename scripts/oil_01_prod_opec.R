# Publish Venezuela monthly production from the validated MOMR corpus.
# Run oil_13_process_momr_corpus.R first; retain vintages and source evidence.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()

oil_prod <- read_csv("data/processed/oil_13_momr_corpus.csv", show_col_types = FALSE,
                     col_types = cols(footnote_marker = col_character()))
readr::stop_for_problems(oil_prod)
oil_prod <- oil_prod %>%
  filter(.data$series_id == "venezuela", .data$source_type %in% c("secondary", "direct")) %>%
  mutate(dataset_id = "oil_01_prod_opec", country = .data$display_label,
         source_type = recode(.data$source_type, secondary = "secondary_sources", direct = "direct_communication"),
         period = format(.data$date, "%Y-%m"), period_type = "monthly",
         observation_month = .data$report_month %m-% months(1),
         source_url = "https://www.opec.org/monthly-oil-market-report.html")
if (nrow(oil_prod) == 0L) rlang::abort("No Venezuela production in the corpus", class = "opec_view_error")
write_processed_dataset(oil_prod, "oil_01_prod_opec")

# The newest published cell wins, including an explicitly unreported value.
oil_prod %>% arrange(.data$report_month) %>%
  group_by(.data$source_type, .data$country, .data$period_type, .data$date) %>%
  slice_tail(n = 1L) %>% ungroup() %>%
  write_final_dataset("oil_01_prod_opec_latest")
