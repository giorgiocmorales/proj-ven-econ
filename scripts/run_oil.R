source("R/source_helpers.R")

# Refresh the inventory, validate downloads, and process the complete corpus.
run_script_isolated("scripts/catalog_opec_momr.R")
run_script_isolated("scripts/check_opec_momr_registry.R")
run_script_isolated("scripts/oil_09_download_opec_momr_archive.R")
run_script_isolated("scripts/oil_13_process_momr_corpus.R")
# Publish report views only after the corpus passes validation.
run_script_isolated("scripts/oil_01_prod_opec.R")
run_script_isolated("scripts/oil_02_price_opec.R")
run_script_isolated("scripts/oil_03_price_imf.R")
