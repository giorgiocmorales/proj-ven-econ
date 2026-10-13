source("R/source_helpers.R")

run_script_isolated("scripts/financial_01_download_bcv.R")
run_script_isolated("scripts/financial_02_balances_bcv.R")
run_script_isolated("scripts/financial_03_reserve_coefficients_bcv.R")
run_script_isolated("scripts/financial_04_reserve_positions_bcv.R")
run_script_isolated("scripts/financial_05_combine.R")
