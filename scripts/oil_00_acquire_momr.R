# Refresh source discovery only on explicit update; rebuilds use the retained inventory/PDFs.
source("R/source_helpers.R")
mode <- source_acquisition_mode()
if (mode == "update") run_script_isolated("scripts/catalog_opec_momr.R")
run_script_isolated("scripts/check_opec_momr_registry.R")
if (mode != "rebuild") run_script_isolated("scripts/oil_09_download_opec_momr_archive.R")
