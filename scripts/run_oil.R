# Build the oil data scope through explicit dependencies; report rendering is separate.
library(dplyr)
source("R/source_helpers.R")
source("R/data_contract_helpers.R")
source("R/pipeline_helpers.R")
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) > 2L) {
  rlang::abort("Supply a mode and, optionally, a failed run ID", class = "pipeline_arguments_error")
}
mode <- if (length(arguments)) arguments[[1]] else source_acquisition_mode()
pipeline_run(target = "oil", mode = mode, parent_run = if (length(arguments) == 2L) arguments[[2]] else "")
