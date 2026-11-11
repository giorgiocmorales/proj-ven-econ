# Build report assets exclusively through the standardized domain graph scripts.
library(tidyverse)
source("R/source_helpers.R")
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for graph labels", class = "report_graph_locale_error")
  }
}
graph_registry <- readr::read_csv("registry/report_graphs.csv", show_col_types = FALSE)
# Reusable GDP reconstructions are published by the data pipeline, not graph scripts.
for (graph_script in unique(graph_registry$script_path)) run_script_isolated(graph_script)
asset_paths <- unique(c(graph_registry$image_path, graph_registry$data_path))
if (!all(file.exists(asset_paths))) {
  rlang::abort("Standardized graph scripts did not produce every required report asset", class = "report_graph_build_error")
}
# A checksum receipt lets direct Quarto renders reject modified scripts, settings, or assets.
input_paths <- unique(c(
  Sys.getenv("REPORT_CONFIG", "config/report.yml"), "R/report_inputs.R", "R/report_figures.R",
  "scripts/report_02_graphs.R", graph_registry$script_path, "registry/report_graphs.csv",
  "registry/report_series.csv", "registry/data_contracts.csv", "R/source_helpers.R",
  unlist(strsplit(graph_registry$input_files, ";", fixed = TRUE)),
  readr::read_csv("registry/report_series.csv", show_col_types = FALSE)$source_file,
  "output/reports/report_series.csv", asset_paths
))
if (!all(file.exists(input_paths))) rlang::abort("Graph receipt has missing inputs", class = "report_graph_input_error")
dir.create("output/reports", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(tibble(file_path = input_paths, md5 = unname(tools::md5sum(input_paths))),
                 "output/reports/graph_manifest.csv")
