# Rebuild validated report inputs and render PDF and Word from one client configuration.
# Set REPORT_CONFIG to another YAML file to build a different client edition.
library(tidyverse)
source("R/source_helpers.R")
source("R/data_contract_helpers.R")
source("R/report_inputs.R")
# Set the locale before parsing graph scripts that contain Spanish labels.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for report labels", class = "report_runtime_error")
  }
}

config_path <- normalizePath(Sys.getenv("REPORT_CONFIG", "config/report.yml"), winslash = "/", mustWork = TRUE)
config <- read_report_config(path = config_path)
run_script_isolated("scripts/report_00_availability.R")
validate_report_sources(
  registry_path = "registry/report_series.csv",
  contracts_path = "registry/data_contracts.csv",
  reference_date = as.Date(config$params$data_cutoff)
)
run_script_isolated("scripts/report_01_data_layer.R")
run_script_isolated("scripts/report_02_graphs.R")
dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)

# Quarto runs from the project root; both formats execute the same validation and figures.
quarto_path <- Sys.which("quarto")
if (!nzchar(quarto_path)) {
  rlang::abort("Quarto CLI is required to render the client report", class = "report_runtime_error")
}

# Preserve the exact configuration and harmonized observations used by this edition.
snapshot_paths <- c("output/reports/report_config.yml", "output/reports/report_series.csv")
# Save the resolved month-end cutoff alongside the observations used by this edition.
yaml::write_yaml(config, snapshot_paths[1])
# The edition snapshot contains only observations within its analysis cutoff.
read_report_series(file_path = "data/final/report_series.csv") %>%
  filter(.data$date <= as.Date(config$params$data_cutoff)) %>%
  readr::write_csv(snapshot_paths[2])
# Include editable sections and automated preparation in the edition's audit trail.
manifest_paths <- c(snapshot_paths, "renv.lock", "_quarto.yml", "report/venezuela_macro.qmd", "report/docx_layout.lua", list.files("report/sections", pattern = "\\.qmd$", full.names = TRUE), "report/templates/estevez_format.docx", "R/report_build.R", "R/report_figures.R", "R/report_tables.R", "R/report_inputs.R", "registry/report_series.csv")
readr::write_csv(
  # Extend the edition receipt to cover the actual graph scripts, inputs, and exported assets.
  tibble(file_path = unique(c(manifest_paths, "scripts/run_report.R", "scripts/export_report_pdf.ps1", "scripts/report_00_availability.R",
    "scripts/report_01_data_layer.R", "output/reports/graph_manifest.csv", "output/reports/data_availability.csv",
    readr::read_csv("output/reports/graph_manifest.csv", show_col_types = FALSE)$file_path))) %>%
    mutate(md5 = unname(tools::md5sum(.data$file_path))),
  "output/reports/build_manifest.csv"
)
status <- system2(quarto_path, args = c(
  "render", "report/venezuela_macro.qmd", "--to", "docx",
  "--metadata-file", shQuote(config_path), "--execute-dir", shQuote(getwd())
))
if (status != 0L) {
  rlang::abort(sprintf("Quarto Word render failed with exit status %s", status), class = "report_render_error")
}

# Derive the PDF from the editable Word report to preserve the client's page furniture.
# This template-faithful exporter requires Microsoft Word on the Windows build host.
if (.Platform$OS.type != "windows") {
  rlang::abort("Template-faithful PDF export requires a Windows host with Microsoft Word",
               class = "report_runtime_error")
}
pdf_status <- system2("powershell.exe", args = c(
  "-NoProfile", "-NonInteractive", "-File", shQuote("scripts/export_report_pdf.ps1"),
  "-InputPath", shQuote(normalizePath("output/reports/report/venezuela_macro.docx", winslash = "/", mustWork = TRUE)),
  "-OutputPath", shQuote(file.path(getwd(), "output/reports/report/venezuela_macro.pdf"))
))
if (pdf_status != 0L) {
  rlang::abort(sprintf("Word PDF export failed with exit status %s", pdf_status), class = "report_render_error")
}
