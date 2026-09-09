# Read one report edition's settings and enforce explicit, consistent date windows.
read_report_config <- function(path) {
  # Preserve accented client metadata even when the host starts R in the C locale.
  config <- yaml::yaml.load(paste(readLines(path, encoding = "UTF-8", warn = TRUE), collapse = "\n"))
  required_params <- c("client", "data_cutoff", "history_start", "recent_history_start", "summary_first_year", "summary_last_year")
  if (!all(c("title", "subtitle", "date", "params") %in% names(config)) ||
      !all(required_params %in% names(config$params))) {
    rlang::abort("Report configuration is missing required metadata or parameters", class = "report_config_error")
  }

  dates <- readr::parse_date(c(config$date, config$params$data_cutoff, config$params$history_start, config$params$recent_history_start), format = "%Y-%m-%d")
  years <- c(config$params$summary_first_year, config$params$summary_last_year)
  if (anyNA(dates) || dates[3] > dates[4] || dates[4] > dates[2] || dates[2] > dates[1] ||
      !is.numeric(years) || anyNA(years) || any(years != floor(years)) ||
      years[1] > years[2] || years[2] > lubridate::year(dates[2])) {
    rlang::abort("Report dates or summary years are inconsistent", class = "report_config_error")
  }
  config
}

# Validate every mapped source before rebuilding the report data layer.
validate_report_sources <- function(registry_path, contracts_path, reference_date) {
  registry <- readr::read_csv(registry_path, show_col_types = FALSE)
  contracts <- read_data_contracts(path = contracts_path)
  missing_contracts <- setdiff(unique(registry$source_file), contracts$file_path)
  if (length(missing_contracts) > 0) {
    rlang::abort(paste("Report sources lack data contracts:", paste(missing_contracts, collapse = ", ")), class = "report_contract_error")
  }
  results <- validate_data_contracts(
    contracts = contracts %>% filter(.data$file_path %in% registry$source_file),
    reference_date = reference_date
  )
  failures <- results %>% filter(.data$status == "fail")
  if (nrow(failures) > 0) {
    rlang::abort(paste(c("Report source validation failed:", paste(failures$dataset_id, failures$check, failures$details, sep = ": ")), collapse = "\n"), class = "report_validation_error")
  }
  invisible(results)
}

# Apply the edition cutoff to each variable and report coverage without inventing vintages.
prepare_report_data <- function(data, settings, registry_path) {
  registry <- readr::read_csv(registry_path, show_col_types = FALSE)
  cutoff <- as.Date(settings$data_cutoff)
  # Report variables need their own limits: a combined file can contain annual and quarterly rows.
  if (!"max_age_days" %in% names(registry) || anyNA(registry$max_age_days) || any(registry$max_age_days < 0)) {
    rlang::abort("Report registry is missing valid per-variable freshness limits", class = "report_contract_error")
  }
  selected <- data %>% filter(.data$date <= cutoff)
  missing_variables <- setdiff(registry$variable_id, unique(selected$variable_id))
  if (length(missing_variables) > 0 || any(!is.finite(selected$value))) {
    rlang::abort(paste("Report has missing or non-finite variables at the cutoff:", paste(missing_variables, collapse = ", ")), class = "report_data_error")
  }
  coverage <- selected %>%
    group_by(.data$variable_id, .data$display_label, .data$frequency, .data$source_id) %>%
    summarise(first_date = min(.data$date), latest_date = max(.data$date), .groups = "drop") %>%
    left_join(registry %>% select("variable_id", "source_file", "max_age_days"), by = "variable_id") %>%
    mutate(age_days = as.integer(cutoff - .data$latest_date))
  stale <- coverage %>% filter(is.na(.data$max_age_days) | .data$age_days > .data$max_age_days)
  if (nrow(stale) > 0) {
    rlang::abort(paste("Report variables are stale at the cutoff:", paste(stale$variable_id, collapse = ", ")), class = "report_freshness_error")
  }
  list(
    data = selected,
    figures = selected %>% filter(.data$date >= as.Date(settings$history_start)),
    coverage = coverage
  )
}
