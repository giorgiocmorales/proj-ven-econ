# Offline regression checks for report cutoffs, per-variable freshness, and schemas.
library(tidyverse)
source("R/data_contract_helpers.R")
source("R/report_inputs.R")
source("R/report_figures.R")

# Build a small fixture from the tracked registry; no downloaded data are needed.
config <- read_report_config(path = "config/report.yml")
settings <- list(client = "Test", data_cutoff = "2026-09-07", history_start = "2019-01-01", summary_first_year = 2021, summary_last_year = 2025)
registry <- readr::read_csv("registry/report_series.csv", show_col_types = FALSE)
fixture <- registry %>%
  transmute(
    variable_id, section, display_label, frequency, unit, observation_role,
    source_id = source_key, dataset_id = source_key,
    date = as.Date(if_else(frequency == "annual", "2025-12-01", "2026-09-01")),
    value = 10
  )
valid <- prepare_report_data(
  data = fixture, settings = settings,
  registry_path = "registry/report_series.csv"
)
stopifnot(nrow(valid$data) == nrow(registry), all(valid$data$date <= as.Date(settings$data_cutoff)))

# Future observations are excluded without discarding valid observations at the cutoff.
future <- fixture %>% mutate(date = as.Date("2027-01-01"), value = 99)
bounded <- prepare_report_data(
  data = bind_rows(fixture, future), settings = settings,
  registry_path = "registry/report_series.csv"
)
stopifnot(identical(valid$data, bounded$data))

# A fresh official rate must not mask a stale alternative-rate series.
stale <- fixture %>% mutate(date = if_else(variable_id == "alternative_fx_yadio", as.Date("2026-07-01"), date))
stale_error <- tryCatch(
  prepare_report_data(data = stale, settings = settings, registry_path = "registry/report_series.csv"),
  report_freshness_error = identity
)
stopifnot(inherits(stale_error, "report_freshness_error"))

# Required variables absent at the cutoff must fail rather than produce an empty panel.
missing_error <- tryCatch(
  prepare_report_data(data = fixture[-1, ], settings = settings, registry_path = "registry/report_series.csv"),
  report_data_error = identity
)
stopifnot(inherits(missing_error, "report_data_error"))

# Invalid source-layer keys and numeric values must fail even without the build runner.
fixture_path <- tempfile(fileext = ".csv")
readr::write_csv(bind_rows(fixture, fixture[1, ]), fixture_path)
duplicate_error <- tryCatch(read_report_series(file_path = fixture_path), report_data_error = identity)
stopifnot(inherits(duplicate_error, "report_data_error"))
fixture$value[1] <- Inf
readr::write_csv(fixture, fixture_path)
value_error <- tryCatch(read_report_series(file_path = fixture_path), report_data_error = identity)
stopifnot(inherits(value_error, "report_data_error"))
unlink(fixture_path)

message("Report pipeline regression checks passed.")
