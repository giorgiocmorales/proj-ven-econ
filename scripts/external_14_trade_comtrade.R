# Download merchandise trade with separate direct and mirror series.
# Usage: Rscript scripts/external_14_trade_comtrade.R config/comtrade.yml resume
# update refreshes all queries; resume downloads missing queries; rebuild is offline.
# Copy the YAML profile to change country, periods, frequency or HS products.
library(tidyverse)
source("R/comtrade_helpers.R")

# Require explicit settings and mode; no automatic API calls from the macro runner.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2) {
  rlang::abort("Usage: Rscript scripts/external_14_trade_comtrade.R <profile.yml> <update|resume|rebuild>",
    class = "comtrade_arguments_error")
}
comtrade_run(config_path = arguments[[1]], mode = arguments[[2]])
