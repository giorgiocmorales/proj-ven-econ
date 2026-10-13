# Prepare validated inputs, figures, and tables for the EMA macro report.

# Windows hosts starting in the C locale need UTF-8 for Spanish chart labels and fonts.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    stop("A UTF-8 locale is required to render the Spanish client report", call. = FALSE)
  }
}

library(tidyverse)
library(knitr)
knitr::opts_chunk$set(dev = if (knitr::is_latex_output()) "cairo_pdf" else "ragg_png")
# Fit paired charts below analysis text within the Word template's retained margins.
knitr::opts_chunk$set(out.width = if (knitr::is_latex_output()) "100%" else "85%")

source(here::here("R", "chart_theme.R"))
source(here::here("R", "report_figures.R"), encoding = "UTF-8")
source(here::here("R", "report_tables.R"))
source(here::here("R", "data_contract_helpers.R"))
source(here::here("R", "report_inputs.R"))
params <- read_report_config(path = Sys.getenv("REPORT_CONFIG", here::here("config", "report.yml")))$params

# Recheck source contracts when rendering directly, then apply this edition's windows.
validate_report_sources(
  registry_path = here::here("registry", "report_series.csv"),
  contracts_path = here::here("registry", "data_contracts.csv"),
  reference_date = as.Date(params$data_cutoff)
)
# Direct renders must not reuse a report layer older than its source inputs or mapping.
report_registry <- readr::read_csv(here::here("registry", "report_series.csv"), show_col_types = FALSE)
report_dependencies <- here::here(c(
  unique(report_registry$source_file), "registry/report_series.csv", "scripts/report_01_data_layer.R"
))
report_layer_path <- here::here("data", "final", "report_series.csv")
if (!file.exists(report_layer_path) || any(file.mtime(report_dependencies) > file.mtime(report_layer_path))) {
  rlang::abort("Report layer is missing or older than its inputs. Run Rscript scripts/run_report.R", class = "report_cache_error")
}
report_inputs <- prepare_report_data(
  data = read_report_series(report_layer_path),
  settings = params,
  registry_path = here::here("registry", "report_series.csv")
)
report_data <- report_inputs$data
executive_table <- build_executive_table(
  data = report_data,
  closing_year = params$summary_last_year
)
external_table <- build_external_table(
  data = report_data,
  first_year = params$summary_first_year,
  last_year = params$summary_last_year
)

# Consume the graph scripts' exact exports; do not rebuild a second set of report plots.
graph_registry <- readr::read_csv(here::here("registry", "report_graphs.csv"), show_col_types = FALSE)
receipt_path <- here::here("output", "reports", "graph_manifest.csv")
if (!file.exists(receipt_path)) {
  rlang::abort("Graph receipt is missing. Run Rscript scripts/run_report.R", class = "report_graph_cache_error")
}
graph_receipt <- readr::read_csv(receipt_path, show_col_types = FALSE)
current_hashes <- unname(tools::md5sum(here::here(graph_receipt$file_path)))
if (anyNA(current_hashes) || !identical(current_hashes, graph_receipt$md5)) {
  rlang::abort("Graph inputs or assets changed. Run Rscript scripts/run_report.R", class = "report_graph_cache_error")
}
report_graphs <- setNames(here::here(graph_registry$image_path), graph_registry$figure_id)
dir.create(here::here("output", "reports"), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(report_inputs$coverage, here::here("output", "reports", "source_coverage.csv"))
readr::write_csv(executive_table, here::here("output", "reports", "executive_table.csv"))
