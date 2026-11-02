# Validate report-critical outputs after the domain extractors have run.

library(tidyverse)

source("R/source_helpers.R")
source("R/data_contract_helpers.R")

ensure_project_dirs()

contracts <- read_data_contracts(path = "registry/data_contracts.csv")
# Data validation is independent of edition-specific report assemblies.
contracts <- contracts %>% filter(startsWith(.data$file_path, "data/final/"))
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 1L) rlang::abort("Supply at most one validation reference date", class = "pipeline_arguments_error")
reference_date <- if (length(arguments)) readr::parse_date(arguments[[1]], format = "%Y-%m-%d") else Sys.Date()
if (is.na(reference_date)) rlang::abort("Invalid validation reference date", class = "pipeline_arguments_error")
validation_results <- validate_data_contracts(
  contracts = contracts,
  reference_date = reference_date
)

dir.create("output/validation", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(validation_results, "output/validation/data_contract_results.csv")

failed_results <- validation_results %>%
  dplyr::filter(.data$status == "fail")

if (nrow(failed_results) > 0) {
  failure_summary <- failed_results %>%
    dplyr::mutate(message = paste(.data$dataset_id, .data$check, .data$details, sep = ": ")) %>%
    dplyr::pull(.data$message)

  stop(
    paste(
      c("Data contract validation failed:", failure_summary),
      collapse = "\n"
    ),
    call. = FALSE
  )
}

message("All report-critical data contracts passed.")
