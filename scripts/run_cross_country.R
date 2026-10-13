# Run the one-time cross-country exercise separately from the Venezuela report pipeline.
# Rscript scripts/run_cross_country.R downloads, validates, and screens all API sources.
# Set CROSS_COUNTRY_REFRESH=false for a fully cached rebuild. Set
# CROSS_COUNTRY_REFRESH_DATA360=true alongside it to refresh only the fiscal-space source.
# CROSS_COUNTRY_RAW_ONLY=true stops after saving raw inputs, without running the screen.
# Provider failures stop the runner before the screening step.
source("R/source_helpers.R")
source("R/cross_country_api_helpers.R")

run_script_isolated("scripts/cross_country_01_download.R")
if (!cross_country_refresh_flag("CROSS_COUNTRY_RAW_ONLY", FALSE)) {
  run_script_isolated("scripts/cross_country_02_screen.R")
}
