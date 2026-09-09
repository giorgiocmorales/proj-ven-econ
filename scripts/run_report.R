# Rebuild validated report inputs and render PDF and Word from one client configuration.
# Set REPORT_CONFIG to another YAML file to build a different client edition.
library(tidyverse)
source("R/source_helpers.R")
source("R/data_contract_helpers.R")
source("R/report_inputs.R")

config_path <- normalizePath(Sys.getenv("REPORT_CONFIG", "config/report.yml"), winslash = "/", mustWork = TRUE)
config <- read_report_config(path = config_path)
validate_report_sources(
  registry_path = "registry/report_series.csv",
  contracts_path = "registry/data_contracts.csv",
  reference_date = as.Date(config$params$data_cutoff)
)
run_script_isolated("scripts/report_01_data_layer.R")
dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)

# Quarto runs from the project root; both formats execute the same validation and figures.
quarto_path <- Sys.which("quarto")
if (!nzchar(quarto_path)) {
  rlang::abort("Quarto CLI is required to render the client report", class = "report_runtime_error")
}

# Preserve the exact configuration and harmonized observations used by this edition.
snapshot_sources <- c(config_path, "data/final/report_series.csv")
snapshot_paths <- c("output/reports/report_config.yml", "output/reports/report_series.csv")
if (!all(file.copy(snapshot_sources, snapshot_paths, overwrite = TRUE))) {
  rlang::abort("Could not save the report edition's input snapshot", class = "report_snapshot_error")
}
manifest_paths <- c(snapshot_paths, "renv.lock", "_quarto.yml", "report/venezuela_macro.qmd", "report/templates/ema_reference.docx", "R/report_figures.R", "R/report_tables.R", "R/report_inputs.R", "registry/report_series.csv")
readr::write_csv(
  tibble(file_path = manifest_paths, md5 = unname(tools::md5sum(manifest_paths))),
  "output/reports/build_manifest.csv"
)
for (output_format in c("pdf", "docx")) {
  status <- system2(quarto_path, args = c(
    "render", "report/venezuela_macro.qmd", "--to", output_format,
    "--metadata-file", shQuote(config_path), "--execute-dir", shQuote(getwd())
  ))
  if (status != 0L) {
    rlang::abort(sprintf("Quarto %s render failed with exit status %s", output_format, status), class = "report_render_error")
  }
}
