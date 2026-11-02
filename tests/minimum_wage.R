# Check source fidelity, monthly selection and all three Venezuelan reconversions.
library(dplyr)
library(readr)
source("R/source_helpers.R")
observations <- read_csv("data/final/real_13_minimum_wage_wikipedia.csv", show_col_types = FALSE)
monthly <- read_csv("data/final/real_14_minimum_wage_monthly.csv", show_col_types = FALSE)

# Each variable has one row per covered month and every row points to immutable source bytes.
months <- seq.Date(as.Date("1974-05-01"), max(monthly$date), by = "month")
stopifnot(nrow(monthly) == 2L * length(months),
  !anyDuplicated(monthly[c("date", "variable_id")]),
  setequal(monthly$variable_id, c("minimum_wage", "food_allowance")),
  setequal(monthly$date, months), max(monthly$date) == lubridate::floor_date(max(observations$date), "month"),
  all(monthly$source_date <= monthly$month_end),
  all(monthly$value_status[is.na(monthly$value)] == "source_missing"))
same_currency <- monthly %>% filter(!source_currency_rebased, !is.na(value_raw))
stopifnot(identical(same_currency$value_raw, same_currency$source_value_raw))
stopifnot(all(vapply(unique(monthly$source_file), digest::digest, character(1),
  algo = "sha256", file = TRUE) == unique(monthly$source_sha256)))

# Multiple updates in May 2000 select the latest food allowance, not a monthly average.
may_2000 <- monthly %>% filter(date == as.Date("2000-05-01"))
stopifnot(all(may_2000$source_date == as.Date("2000-05-24")),
  may_2000$value_raw[may_2000$variable_id == "minimum_wage"] == 144000,
  may_2000$value_raw[may_2000$variable_id == "food_allowance"] == 127600)

# Current-bolivar values retain nominal increases while removing denomination breaks.
expected <- tibble::tribble(
  ~date, ~variable_id, ~currency_raw, ~value_raw, ~value,
  "1974-05-01", "minimum_wage", "VEB", 450, 450e-14,
  "1974-05-01", "food_allowance", "VEB", 0, 0,
  "2007-12-01", "minimum_wage", "VEB", 614790, 614790e-14,
  "2008-01-01", "minimum_wage", "VEF", 614.79, 614.79e-11,
  "2018-07-01", "minimum_wage", "VEF", 3000000, 3e-5,
  "2018-08-01", "minimum_wage", "VES_2018", 30, 3e-5,
  "2018-08-01", "food_allowance", "VES_2018", 21.96, 21.96e-6,
  "2018-09-01", "minimum_wage", "VES_2018", 1800, 0.0018,
  "2021-09-01", "minimum_wage", "VES_2018", 7000000, 7,
  "2021-10-01", "minimum_wage", "VES_current", 7, 7,
  "2022-03-01", "minimum_wage", "VES_current", 130, 130
) %>% mutate(date = as.Date(date))
actual <- expected %>% left_join(monthly, by = c("date", "variable_id"),
  suffix = c("_expected", ""), relationship = "one-to-one")
stopifnot(all(actual$currency_raw == actual$currency_raw_expected),
  all(abs(actual$value_raw - actual$value_raw_expected) < 1e-8),
  all(abs(actual$value - actual$value_expected) <= 1e-12 * abs(actual$value_expected)),
  all(monthly$source_currency_rebased[monthly$date == as.Date("2018-08-01")]),
  all(monthly$carried_forward[monthly$date == as.Date("2018-08-01")]))

# Known punctuation anomalies must be flagged and explicit source blanks must not fill down.
anomalies <- observations %>% filter(punctuation_normalized)
stopifnot(all(c(1464, 1676.9) %in% anomalies$value_raw),
  all(anomalies$variable_id == "food_allowance"))
missing <- monthly %>% filter(date == as.Date("2025-05-01"), variable_id == "food_allowance")
stopifnot(nrow(missing) == 1L, is.na(missing$value_raw), is.na(missing$value),
  missing$value_status == "source_missing", !missing$carried_forward)

# Cached rebuilding must reproduce the exact extracts, including original retrieval metadata.
before <- unname(tools::md5sum(c("data/raw/real_13_minimum_wage_wikipedia.csv",
  "data/final/real_13_minimum_wage_wikipedia.csv", "data/final/real_14_minimum_wage_monthly.csv")))
Sys.setenv(MINIMUM_WAGE_OFFLINE = "true")
run_script_isolated("scripts/real_13_minimum_wage_wikipedia.R")
run_script_isolated("scripts/real_14_minimum_wage_monthly.R")
after <- unname(tools::md5sum(c("data/raw/real_13_minimum_wage_wikipedia.csv",
  "data/final/real_13_minimum_wage_wikipedia.csv", "data/final/real_14_minimum_wage_monthly.csv")))
stopifnot(identical(before, after))
cat("Passed: source hashes, monthly selection, currency boundaries, punctuation, missingness and offline rebuild.\n")
