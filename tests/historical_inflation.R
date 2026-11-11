# Check historical inflation against native indices and the intended geography boundary.
library(dplyr)
library(readr)
library(stringr)
cpi <- read_csv("data/final/prices_07_cpi_caracas_bcv.csv", show_col_types = FALSE)
caracas <- read_csv("data/final/prices_08_inflation_caracas_bcv.csv", show_col_types = FALSE)
history <- read_csv("data/final/prices_08_inflation_historical_bcv.csv", show_col_types = FALSE)
national <- read_csv("data/final/prices_04_inflation_groups_bcv.csv", show_col_types = FALSE) %>%
  filter(series_id == "headline", variable_id == "inflation_mom")

# Preserve source coverage, missing initial inflation, negative cells and immutable bytes.
stopifnot(identical(cpi$date, seq.Date(as.Date("1950-01-01"), max(cpi$date), by = "month")),
  !anyDuplicated(cpi$date), is.na(cpi$mom_var_pct[cpi$date == as.Date("1950-01-01")]),
  abs(cpi$cpi_index[cpi$date == as.Date("1950-01-01")] - 0.02894657) < 1e-10,
  abs(cpi$mom_var_pct[cpi$date == as.Date("1985-02-01")] + 0.4) < 1e-10)
negative_cells <- cpi %>% filter(str_detect(mom_var_raw, "^\\("), !is.na(mom_var_pct))
stopifnot(nrow(negative_cells) > 0L, all(negative_cells$mom_var_pct < 0),
  all(unname(tools::md5sum(unique(cpi$source_file))) == unique(cpi$source_md5)))

# Recompute each native monthly rate; the first index must never acquire a made-up rate.
expected <- cpi %>% arrange(date) %>%
  transmute(date, expected = 100 * (cpi_index / lag(cpi_index) - 1)) %>% filter(!is.na(expected))
observed <- caracas %>% filter(variable_id == "inflation_mom") %>%
  inner_join(expected, by = "date", relationship = "one-to-one")
stopifnot(nrow(observed) == nrow(cpi) - 1L,
  all(abs(observed$value - observed$expected) < 1e-10),
  min(observed$date) == as.Date("1950-02-01"),
  !anyDuplicated(caracas[c("variable_id", "date")]))

# A continuous reference uses rates, with the geography change confined to January 2008.
stopifnot(identical(history$date, seq.Date(as.Date("1950-02-01"), max(national$date), by = "month")),
  identical(history$date[history$geography_break], as.Date("2008-01-01")),
  all(history$geography[history$date < as.Date("2008-01-01")] == "caracas_metropolitan_area"),
  all(history$geography[history$date >= as.Date("2008-01-01")] == "venezuela_national"),
  all(history$purpose == "historical_reference"))
national_check <- history %>% inner_join(national %>% select(date, expected = value),
  by = "date", relationship = "one-to-one")
stopifnot(nrow(national_check) == nrow(national),
  all(abs(national_check$value - national_check$expected) < 1e-10))
cat("Passed: source coverage, negative rates, initial missing rate, native CPI calculations and geography boundary.\n")
