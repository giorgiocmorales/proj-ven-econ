source("R/source_helpers.R")

run_script_isolated("scripts/run_prices.R")
run_script_isolated("scripts/run_monetary.R")
run_script_isolated("scripts/run_financial.R")
run_script_isolated("scripts/run_oil.R")
run_script_isolated("scripts/run_external.R")
run_script_isolated("scripts/run_real.R")
run_script_isolated("scripts/run_fiscal.R")
run_script_isolated("scripts/report_01_data_layer.R")
run_script_isolated("scripts/run_validate.R")
# Render the configured client edition only after all domain contracts pass.
run_script_isolated("scripts/run_report.R")
