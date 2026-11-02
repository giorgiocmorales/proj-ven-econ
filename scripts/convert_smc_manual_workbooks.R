# Convert all legacy BCV SMC workbooks needed for a complete historical build.

library(tidyverse)
library(readxl)

source("R/smc_manual_helpers.R")

manual_fix_files <- smc_manual_fix_registry()
prepare_smc_manual_workbooks(
  manual_fixes = manual_fix_files,
  converter_path = "scripts/convert_smc_manual_workbooks.ps1"
)

message("All legacy SMC workbooks are available as readable XLSX files.")
