# Check source fidelity, denomination boundaries, historical gaps and canonical precedence.
library(tidyverse)
source("R/source_helpers.R")
source("R/palma_fx_helpers.R")
historical <- read_csv("data/final/external_15_fx_palma.csv", show_col_types = FALSE,
  col_types = cols(value_cell_raw = col_character(), date_cell_raw = col_character()))
stopifnot(all(historical$quality_status == "usable"), !anyNA(historical$date))
# Full source-fidelity checks include retained exclusions without making them model inputs.
historical <- bind_rows(historical,
  read_csv("data/interim/external_15_fx_palma_quarantine.csv", show_col_types = FALSE,
    col_types = cols(value_cell_raw = col_character(), date_cell_raw = col_character())))
daily <- read_csv("data/final/external_16_fx_daily_history.csv", show_col_types = FALSE)
canonical <- read_csv("data/final/external_fx.csv", show_col_types = FALSE)
manifest <- read_csv("data/raw/palma_fx/manifest.csv", show_col_types = FALSE)
protected <- read_csv("data/interim/external_16_fx_daily_history_inputs.csv", show_col_types = FALSE)

# Source archives, live source workbooks and canonical input files must remain byte-exact.
stopifnot(all(map_chr(manifest$raw_file, digest::digest, algo = "sha256", file = TRUE) == manifest$source_sha256),
  all(map_chr(manifest$source_path, digest::digest, algo = "sha256", file = TRUE) == manifest$source_sha256),
  identical(map_chr(protected$path, digest::digest, algo = "sha256", file = TRUE), protected$sha256),
  !anyDuplicated(daily[c("date", "variable_id", "quote_currency")]),
  all(daily$dataset_id == "external_16_fx_daily_history"),
  all(daily$quality_status == "usable"), all(daily$rate_adjusted > 0),
  all(abs(daily$rate_nominal * daily$nominal_conversion_factor - daily$rate_adjusted) <=
    1e-12 * daily$rate_adjusted))

# Independent comparisons cover every existing canonical raw/adjusted pair, not a sample.
mapping <- tribble(~variable_id, ~raw_column, ~adjusted_column,
  "parallel", "rate_yad", "rate_yad",
  "official_bcv_smc", "rate_smc", "rate_smc_adj",
  "official_bcv_index", "rate_indx", "rate_indx_adj",
  "official_bcv_intervention", "rate_int", "rate_int_adj")
for (i in seq_len(nrow(mapping))) {
  rule <- mapping[i, ]
  expected <- canonical %>% transmute(date, raw_expected = .data[[rule$raw_column]],
    adjusted_expected = .data[[rule$adjusted_column]]) %>% filter(!is.na(adjusted_expected))
  actual <- daily %>% filter(variable_id == rule$variable_id) %>%
    select(date, rate_raw, rate_adjusted, canonical)
  check <- expected %>% left_join(actual, by = "date", relationship = "one-to-one")
  stopifnot(nrow(check) == nrow(expected), all(check$canonical),
    identical(check$raw_expected, check$rate_raw), identical(check$adjusted_expected, check$rate_adjusted))
}
headline <- canonical %>% transmute(date, expected = coalesce(rate_smc_adj, rate_indx_adj)) %>%
  filter(!is.na(expected)) %>% left_join(daily %>% filter(variable_id == "official_headline") %>%
    select(date, rate_adjusted, canonical), by = "date", relationship = "one-to-one")
stopifnot(all(headline$canonical), identical(headline$expected, headline$rate_adjusted))

# Physical coordinates must survive blank leading rows in the PAP workbook.
pap <- historical %>% filter(workbook_id == "palma_notes", source_cell == "C51")
stopifnot(nrow(pap) == 1L, pap$date == as.Date("2005-01-03"), pap$rate_raw == 2.74,
  pap$source_currency == "VEF", pap$currency_nominal == "VEB",
  abs(pap$rate_nominal - 2740) < 1e-8, abs(pap$rate_adjusted - 2.74e-11) < 1e-22)
chart <- historical %>% filter(workbook_id == "palma_charts", sheet == "Gr\u00e1fico 15.3", source_cell == "C2")
stopifnot(nrow(chart) == 1L, chart$date == as.Date("2018-02-05"),
  chart$source_currency == "VES_2018", chart$currency_nominal == "VEF",
  abs(chart$rate_nominal - 24995.76513862113) < 1e-7,
  abs(chart$rate_adjusted - 2.499576513862113e-7) < 1e-18)

# Check known source amounts independently against all three legal currency boundaries.
scales <- read_csv("registry/venezuela_currency_scales.csv", show_col_types = FALSE)
boundaries <- tribble(~date, ~currency, ~factor,
  "2007-12-31", "VEB", 1e-14, "2008-01-01", "VEF", 1e-11,
  "2018-08-19", "VEF", 1e-11, "2018-08-20", "VES_2018", 1e-6,
  "2021-09-30", "VES_2018", 1e-6, "2021-10-01", "VES_current", 1) %>% mutate(date = as.Date(date))
checks <- daily %>% filter(variable_id == "parallel") %>% inner_join(boundaries, by = "date")
stopifnot(nrow(checks) == 6L, all(checks$currency_nominal == checks$currency),
  all(checks$nominal_conversion_factor == checks$factor))

# January-October 1990 and SITME remain monthly-only; no synthetic daily quotes appear.
stopifnot(any(historical$variable_id == "official_sitme" & historical$frequency == "monthly"),
  !"official_sitme" %in% daily$variable_id, min(daily$date) == as.Date("1990-11-05"),
  sum(historical$frequency == "monthly_anchor" & historical$date >= as.Date("1990-01-01")) == 20L,
  all(daily$carried_days[daily$value_method == "carried_forward"] <= 7L),
  all(daily$observation_date <= daily$date))
bad_ids <- historical %>% filter(quality_status != "usable") %>% pull(observation_id)
stopifnot(!any(daily$observation_id %in% bad_ids),
  any(historical$quality_status == "invalid_date"),
  any(historical$quality_status == "conflicting_source_key"),
  any(historical$quality_status == "date_out_of_sequence"))

# Authority must outrank historical priority, even if a later edit gives history rank -100.
fixture <- daily %>% filter(variable_id == "parallel", canonical) %>% slice(1L)
competitor <- fixture %>% mutate(canonical = FALSE, source_id = "ecoanalitica_palma",
  rate_adjusted = rate_adjusted * 2, observation_id = "synthetic_competing_history", priority = -100)
authority <- fixture %>% mutate(priority = 100)
winner <- palma_fx_select_daily(bind_rows(authority, competitor))
stopifnot(nrow(winner) == 1L, winner$canonical, winner$rate_adjusted == fixture$rate_adjusted)
tie <- competitor %>% mutate(priority = 100, observation_id = "synthetic_conflicting_history")
tie2 <- tie %>% mutate(rate_adjusted = rate_adjusted * 2, observation_id = "second_history")
failure <- tryCatch(palma_fx_select_daily(bind_rows(tie, tie2)), palma_fx_priority_error = identity)
stopifnot(inherits(failure, "palma_fx_priority_error"))

# Cached acquisition/consolidation must reproduce output bytes without touching canonical inputs.
paths <- c("data/final/external_15_fx_palma.csv", "data/final/external_16_fx_daily_history.csv",
  "data/interim/external_16_fx_daily_history_overlap.csv")
before <- map_chr(paths, digest::digest, algo = "sha256", file = TRUE)
run_script_isolated("scripts/external_15_fx_palma.R")
run_script_isolated("scripts/external_16_fx_daily_history.R")
stopifnot(identical(before, map_chr(paths, digest::digest, algo = "sha256", file = TRUE)),
  identical(protected$sha256, map_chr(protected$path, digest::digest, algo = "sha256", file = TRUE)))
cat("Passed: source archives/cells, every canonical value, precedence, three reconversions, quarantine, monthly exclusions and cached replay.\n")
