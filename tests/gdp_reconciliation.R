# Recover known synthetic GDP levels and reject damaged inputs without replacing outputs.
library(tidyverse)

# Isolate fixtures from the user's downloaded observations and generated datasets.
project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
script_path <- file.path(project_root, "scripts/real_10_gdp_reconciliation_bcv.R")
fixture_root <- tempfile(pattern = "gdp_reconciliation_")
dir.create(file.path(fixture_root, "R"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(fixture_root, "data/processed"), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.copy(file.path(project_root, "R/source_helpers.R"),
                   file.path(fixture_root, "R/source_helpers.R")))

# Known unequal quarter levels provide independent expected indices and growth rates.
levels <- crossing(year = 2007:2026, quarter = 1:4) %>%
  filter(year < 2026L | quarter <= 2L) %>% arrange(year, quarter) %>% mutate(
    gdp_index = c(22, 24, 25, 29)[quarter] *
      exp(0.03 * (year - 2007L) + 0.07 * sin((year - 2007L) * quarter)),
    date = lubridate::make_date(year, 3L * quarter - 2L, 1L)
  )
quarterly_fixture <- levels %>% group_by(quarter) %>%
  mutate(value = 100 * (gdp_index / lag(gdp_index, n = 1L) - 1)) %>% ungroup() %>%
  filter(year > 2007L) %>% transmute(
    series_label = "Fixture", year, quarter, value, date, frequency = "quarterly",
    unit = "percent_yoy", provisional = year >= 2025L,
    source_url = "https://example.invalid/quarterly", downloaded_at = "2026-10-12 12:00:00"
  )
annual_fixture <- levels %>% filter(year <= 2025L) %>% group_by(year) %>%
  summarise(gdp_index = sum(gdp_index), .groups = "drop") %>% arrange(year) %>%
  mutate(value = 100 * (gdp_index / lag(gdp_index, n = 1L) - 1)) %>% filter(year > 2007L) %>%
  transmute(series_label = "Fixture", year, value, date = lubridate::make_date(year, 1L, 1L),
    frequency = "annual", unit = "percent_yoy", provisional = year >= 2025L,
    source_url = "https://example.invalid/annual", downloaded_at = "2026-10-12 12:00:00")

# Use a temporary working directory only inside this local scope, restoring it on exit.
local({
  on.exit(setwd(project_root), add = TRUE)
  setwd(fixture_root)
  for (case in c("valid", "missing_quarter", "duplicate_quarter", "wrong_date",
                 "invalid_growth", "inconsistent_annual", "unidentified_shares")) {
    a <- annual_fixture
    q <- quarterly_fixture
    expected_class <- "gdp_reconciliation_input_error"
    if (case == "missing_quarter") q <- q %>% slice(-10L)
    if (case == "duplicate_quarter") q <- bind_rows(q, q %>% slice(10L))
    if (case == "wrong_date") q <- q %>% mutate(date = if_else(row_number() == 10L, date + 1L, date))
    if (case == "invalid_growth") q <- q %>% mutate(value = if_else(row_number() == 10L, -100, value))
    if (case == "inconsistent_annual") {
      a <- a %>% mutate(value = if_else(year == 2020L, value + 0.5, value))
      expected_class <- "gdp_reconciliation_growth_error"
    }
    if (case == "unidentified_shares") {
      a <- a %>% mutate(value = 1)
      q <- q %>% mutate(value = 1)
      expected_class <- "gdp_reconciliation_rank_error"
    }
    for (annual_file in c("real_01_gdp_a_a_bcv", "real_02_gdp_a_s_bcv")) {
      write_csv(a, paste0("data/processed/", annual_file, ".csv"))
    }
    for (quarterly_file in c("real_03_gdp_q_a_bcv", "real_04_gdp_q_s_bcv")) {
      write_csv(q, paste0("data/processed/", quarterly_file, ".csv"))
    }
    if (case != "valid") {
      previous_hashes <- tools::md5sum(c("data/final/real_10_gdp_quarterly_bcv.csv",
                                        "data/final/real_10_gdp_annual_bcv.csv"))
    }
    result <- tryCatch(source(script_path, local = new.env(parent = globalenv())),
      gdp_reconciliation_input_error = identity,
      gdp_reconciliation_growth_error = identity,
      gdp_reconciliation_rank_error = identity,
      gdp_reconciliation_share_error = identity)
    if (case != "valid") {
      stopifnot(inherits(result, expected_class),
        identical(previous_hashes, tools::md5sum(names(previous_hashes))))
      next
    }

    # Compare against known levels, not against another implementation of the fitted shares.
    generated <- read_csv("data/final/real_10_gdp_quarterly_bcv.csv", show_col_types = FALSE) %>%
      filter(family == "activity")
    expected <- levels %>% mutate(
      qoq = 100 * (gdp_index / lag(gdp_index, n = 1L) - 1),
      rolling_total = slider::slide_dbl(gdp_index, sum, .before = 3L, .complete = TRUE),
      rolling_yoy = 100 * (rolling_total / lag(rolling_total, n = 4L) - 1)
    ) %>% group_by(quarter) %>%
      mutate(previous_level = lag(gdp_index, n = 1L)) %>% ungroup() %>%
      group_by(year) %>% mutate(ytd = 100 * (cumsum(gdp_index) / cumsum(previous_level) - 1)) %>%
      ungroup() %>% filter(year > 2007L)
    stopifnot(nrow(generated) == 74L,
      max(abs(generated$gdp_index - expected$gdp_index)) < 1e-8,
      max(abs(generated$qoq_unadjusted_percent - expected$qoq)) < 1e-8,
      identical(is.na(generated$rolling_four_quarter_yoy_percent), is.na(expected$rolling_yoy)),
      max(abs(generated$rolling_four_quarter_yoy_percent - expected$rolling_yoy), na.rm = TRUE) < 1e-8,
      max(abs(generated$ytd_yoy_percent - expected$ytd)) < 1e-8)
    # Partial-year annual weights retain the full preceding-year denominator.
    h1_weights <- generated %>% filter(year == 2026L) %>% summarise(total = sum(previous_year_annual_weight))
    stopifnot(h1_weights$total > 0, h1_weights$total < 1)
  }
})
message("GDP reconstruction fixtures and input-rejection checks passed.")
