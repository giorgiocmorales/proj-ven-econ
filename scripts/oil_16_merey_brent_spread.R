# Calculate the monthly Merey-minus-Brent differential from existing latest-revision prices.
# USD spread = Merey - Brent; percentage spread = 100 * (Merey - Brent) / Brent.
# Negative values denote a Merey discount. OPEC benchmark labels remain attached to each leg.
library(dplyr)
library(tidyr)
library(readr)
source("R/source_helpers.R")

# Validate source schema before selecting prices; no downloads or source changes are needed.
input_path <- "data/final/oil_02_price_opec_latest.csv"
prices <- read_csv(input_path, show_col_types = FALSE,
  col_types = cols(footnote_marker = col_character()))
stop_for_problems(prices)
required_columns <- c("date", "variable_id", "period_type", "source_type", "unit", "value",
  "source_label", "value_status", "report_month", "source_pdf_url", "pdf_file", "pdf_md5",
  "source_table_page", "footnote_marker", "source_table_notes")
if (!all(required_columns %in% names(prices))) {
  rlang::abort("Oil-price input is missing required price or provenance columns",
    class = "oil_spread_schema_error")
}
prices <- prices %>% filter(period_type == "monthly", variable_id %in% c("merey", "north_sea_dated"))
if (!setequal(prices$variable_id, c("merey", "north_sea_dated")) ||
    !inherits(prices$date, "Date") || anyNA(prices$date) ||
    anyDuplicated(prices[c("variable_id", "date")]) ||
    anyNA(prices$unit) || any(prices$unit != "USD/b") ||
    any(prices$source_type != "price") || any(is.infinite(prices$value)) ||
    any(prices$value[prices$variable_id == "north_sea_dated"] <= 0, na.rm = TRUE)) {
  rlang::abort("Spread inputs require unique monthly USD/b prices and a positive Brent denominator",
    class = "oil_spread_input_error")
}

# Start when both series have begun; preserve internal or trailing unavailable months.
coverage <- prices %>% group_by(variable_id) %>%
  summarise(first_date = min(date), last_date = max(date), .groups = "drop")
start_date <- max(coverage$first_date)
end_date <- max(coverage$last_date)
if (start_date > end_date) {
  rlang::abort("Merey and Brent have no common historical window", class = "oil_spread_coverage_error")
}
spread <- prices %>% filter(date >= start_date) %>%
  transmute(date, leg = if_else(variable_id == "merey", "merey", "brent"),
    price_usd_per_barrel = value, source_label, value_status, report_month,
    source_pdf_url, pdf_file, pdf_md5, source_table_page, footnote_marker, source_table_notes) %>%
  pivot_wider(names_from = leg, names_glue = "{leg}_{.value}",
    values_from = c(price_usd_per_barrel, source_label, value_status, report_month,
      source_pdf_url, pdf_file, pdf_md5, source_table_page, footnote_marker, source_table_notes)) %>%
  complete(date = seq.Date(start_date, end_date, by = "month")) %>% arrange(date) %>%
  mutate(availability = case_when(
      is.na(merey_price_usd_per_barrel) & is.na(brent_price_usd_per_barrel) ~ "both_missing",
      is.na(merey_price_usd_per_barrel) ~ "merey_missing",
      is.na(brent_price_usd_per_barrel) ~ "brent_missing", TRUE ~ "paired"),
    spread_usd_per_barrel = merey_price_usd_per_barrel - brent_price_usd_per_barrel,
    spread_pct_brent = 100 * spread_usd_per_barrel / brent_price_usd_per_barrel,
    same_report_vintage = if_else(availability == "paired", merey_report_month == brent_report_month, NA),
    frequency = "monthly", dataset_id = "oil_16_merey_brent_spread",
    source_dataset_id = "oil_02_price_opec_latest", input_md5 = unname(tools::md5sum(input_path))) %>%
  select(date, merey_price_usd_per_barrel, brent_price_usd_per_barrel,
    spread_usd_per_barrel, spread_pct_brent, availability, same_report_vintage, everything())

# Missing source prices must remain missing spreads; never interpolate or fill a leg.
if (!any(spread$availability == "paired") ||
    any(is.infinite(spread$spread_usd_per_barrel) | is.infinite(spread$spread_pct_brent)) ||
    any(is.na(spread$spread_usd_per_barrel) != (spread$availability != "paired")) ||
    any(is.na(spread$spread_pct_brent) != (spread$availability != "paired"))) {
  rlang::abort("Oil spread values failed paired-price or missing-value checks",
    class = "oil_spread_output_error")
}
write_final_dataset(spread, "oil_16_merey_brent_spread")
message("Calculated ", sum(spread$availability == "paired"), " monthly spreads from ",
  start_date, " to ", end_date, "; retained ", sum(spread$availability != "paired"), " missing months")
print(spread %>% filter(availability == "paired") %>% slice_tail(n = 3L) %>%
  select(date, merey_price_usd_per_barrel, brent_price_usd_per_barrel,
    spread_usd_per_barrel, spread_pct_brent))
