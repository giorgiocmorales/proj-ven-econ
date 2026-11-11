# Audit source observations before report analysis; do not mistake file age for data freshness.
library(tidyverse)
source("R/report_inputs.R")
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
cutoff <- as.Date(config$params$data_cutoff)
inventory <- readr::read_csv("registry/report_availability.csv", show_col_types = FALSE,
                             col_types = cols(.default = col_character()))

# Record missing inputs explicitly and profile each selected series at its own grain.
availability <- purrr::pmap_dfr(inventory, function(series_id, file_path, frequency,
                                                   date_column, value_column, filter_column,
                                                   filter_value, key_columns) {
  if (!file.exists(file_path)) {
    return(tibble(series_id, file_path, frequency, cutoff, status = "missing_file"))
  }
  data <- readr::read_csv(file_path, show_col_types = FALSE,
                          col_types = cols(.default = col_character()))
  readr::stop_for_problems(data)
  keys <- str_split(key_columns, fixed("|"))[[1]]
  required <- c(date_column, value_column, keys, if (!is.na(filter_column)) filter_column)
  if (!all(required %in% names(data))) {
    rlang::abort(paste("Availability audit input lacks required columns:", file_path),
                 class = "report_availability_schema_error")
  }
  if (!is.na(filter_column)) data <- data %>% filter(.data[[filter_column]] == filter_value)
  dates <- readr::parse_date(data[[date_column]], format = "%Y-%m-%d")
  values <- readr::parse_double(data[[value_column]])
  readr::stop_for_problems(values)
  if (any(is.infinite(values))) {
    rlang::abort(paste("Non-finite audit values:", file_path), class = "report_availability_value_error")
  }
  if (anyNA(dates)) rlang::abort(paste("Invalid audit dates:", file_path), class = "report_availability_date_error")
  # Compare completed periods with the cutoff, including monthly and quarterly date anchors.
  period_ends <- switch(frequency,
    monthly = lubridate::ceiling_date(dates, "month") - lubridate::days(1),
    quarterly = lubridate::ceiling_date(dates, "quarter") - lubridate::days(1),
    annual = lubridate::ceiling_date(dates, "year") - lubridate::days(1),
    daily = dates, weekly = dates,
    rlang::abort(paste("Unsupported audit frequency:", frequency), class = "report_availability_frequency_error"))
  available <- is.finite(values)
  eligible <- available & period_ends <= cutoff
  latest <- if (any(available)) max(dates[available]) else as.Date(NA)
  latest_at_cutoff <- if (any(eligible)) max(dates[eligible]) else as.Date(NA)
  latest_period_end <- if (any(eligible)) max(period_ends[eligible]) else as.Date(NA)
  duplicates <- sum(duplicated(data[keys]))
  tibble(series_id, file_path, frequency, cutoff,
    status = if (duplicates > 0L) "duplicate_keys" else if (!any(eligible)) "no_data_at_cutoff" else "available",
    rows = nrow(data), duplicate_keys = duplicates, missing_values = sum(!available),
    latest_available = latest, latest_at_cutoff, latest_period_end,
    lag_days = as.integer(cutoff - latest_period_end),
    file_modified_at = as.character(file.info(file_path)$mtime))
})

# Persist the audit even when a required input needs repair; the report runner validates next.
dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(availability, "output/reports/data_availability.csv")
print(availability %>% select(series_id, frequency, status, latest_available, latest_at_cutoff, lag_days), n = Inf)
