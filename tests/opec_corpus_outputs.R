# Integration checks for a built corpus: vintages, source evidence, gaps and report contracts.
library(tidyverse)
source("R/data_contract_helpers.R")

corpus <- read_csv("data/processed/oil_13_momr_corpus.csv", show_col_types = FALSE,
                   col_types = cols(footnote_marker = col_character()))
latest <- read_csv("data/final/oil_13_momr_latest.csv", show_col_types = FALSE,
                   col_types = cols(footnote_marker = col_character()))
readr::stop_for_problems(corpus)
readr::stop_for_problems(latest)
audit <- read_csv("data/final/oil_13_momr_report_audit.csv", show_col_types = FALSE)
stopifnot(!anyDuplicated(corpus[c("report_month", "source_type", "series_id", "date")]),
          !anyDuplicated(latest[c("source_type", "series_id", "date")]),
          all(corpus$date < corpus$report_month),
          all(corpus$value[corpus$source_type != "price"] >= 0, na.rm = TRUE),
          all(is.na(corpus$value) == (corpus$value_status == "not_reported")),
          all(!is.na(corpus$source_pdf_url)), all(nchar(corpus$pdf_md5) == 32L),
          all(audit$status %in% c("ok", "not_published", "unavailable_pdf")))

# Check the vintage selection without discarding missing published cells.
keys <- c("source_type", "series_id", "date")
newest <- corpus %>% group_by(across(all_of(keys))) %>%
  summarise(expected_report = max(.data$report_month), .groups = "drop")
comparison <- latest %>% left_join(newest, by = keys, relationship = "one-to-one")
stopifnot(nrow(comparison) == nrow(newest), all(comparison$report_month == comparison$expected_report))

# Independent transcription from April 2018, page 51: Venezuela March production.
anchors <- corpus %>% filter(.data$report_month == as.Date("2018-04-01"),
                             .data$date == as.Date("2018-03-01"), .data$series_id == "venezuela")
stopifnot(nrow(anchors) == 2L, anchors$value[anchors$source_type == "secondary"] == 1488,
          anchors$value[anchors$source_type == "direct"] == 1509)

# January 2001's printed basket-price headers omit the year and cross a year boundary.
basket <- corpus %>% filter(.data$report_month == as.Date("2001-01-01"), .data$series_id == "orb") %>%
  arrange(.data$date)
stopifnot(identical(basket$date, as.Date(c("2000-11-01", "2000-12-01"))),
          identical(basket$value, c(31.22, 24.13)))

# Footnote-qualified figures retain their published meaning, not an inferred provisional flag.
qualified <- corpus %>% filter(.data$report_month == as.Date("2025-08-01"),
                               .data$date == as.Date("2025-07-01"), .data$source_type == "direct",
                               .data$series_id == "saudi_arabia")
stopifnot(nrow(qualified) == 1L, qualified$value == 9525, qualified$footnote_marker == "**",
          str_detect(qualified$source_table_notes, fixed("supply to the market")),
          str_detect(qualified$source_table_notes, fixed("9,201")))

# Existing report consumers must accept source-declared historical missing cells.
contracts <- read_data_contracts("registry/data_contracts.csv") %>%
  filter(.data$dataset_id %in% c("oil_01_prod_opec_latest", "oil_02_price_opec_latest"))
checks <- map_dfr(seq_len(nrow(contracts)), function(i) validate_dataset_contract(contracts[i, ], Sys.Date()))
print(checks %>% filter(.data$status != "pass"))
stopifnot(all(checks$status == "pass"))
message("OPEC corpus output and report compatibility checks passed.")
