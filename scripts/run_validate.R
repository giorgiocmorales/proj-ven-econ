# Validate report-critical outputs after the domain extractors have run.

library(tidyverse)

source("R/source_helpers.R")
source("R/data_contract_helpers.R")

ensure_project_dirs()

contracts <- read_data_contracts(path = "registry/data_contracts.csv")
validation_results <- validate_data_contracts(
  contracts = contracts,
  reference_date = Sys.Date()
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
