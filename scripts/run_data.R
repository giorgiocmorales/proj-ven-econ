# Update a dataset/domain through its declared dependencies and isolated publication.
# Usage: Rscript scripts/run_data.R <dataset_id|domain|all> <rebuild|resume|update> [failed_run_id]
library(dplyr)
source("R/source_helpers.R")
source("R/data_contract_helpers.R")
source("R/pipeline_helpers.R")
arguments <- commandArgs(trailingOnly = TRUE)
if (!length(arguments) %in% c(2L, 3L)) {
  rlang::abort("Usage: run_data.R <dataset_id|domain|all> <rebuild|resume|update> [failed_run_id]", class = "pipeline_arguments_error")
}
pipeline_run(target = arguments[[1]], mode = arguments[[2]], parent_run = if (length(arguments) == 3L) arguments[[3]] else "")
