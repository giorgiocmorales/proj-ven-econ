# Single R entry point for provenance audits, immutable staging and isolated builds.
# See README.md for the explicit command arguments and supported source profiles.
library(dplyr)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for source provenance", class = "repro_locale_error")
  }
}
source("R/reproducibility.R")
Sys.setlocale("LC_COLLATE", "C")
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
store <- file.path(root, "data/reproducibility")
manifest_dir <- file.path(root, "registry/reproducibility")
dir.create(manifest_dir, recursive = TRUE, showWarnings = FALSE)
sources <- readr::read_csv("registry/reproducibility_sources.csv", show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character()), na = character())
arguments <- commandArgs(trailingOnly = TRUE)
arguments[arguments == "-"] <- ""
if (!length(arguments)) rlang::abort("Specify baseline, audit, verify, objects, policies, ingest, update, rebuild, or replay", class = "repro_config_error")
action <- arguments[1]
if (action == "baseline" && length(arguments) == 1L) {
  repro_baseline(root, store, manifest_dir)
  repro_audit(root, store, manifest_dir)
} else if (action == "audit" && length(arguments) == 1L) {
  repro_audit(root, store, manifest_dir)
} else if (action == "verify" && length(arguments) == 1L) {
  repro_dependencies(root)
  repro_checks(root, store, manifest_dir, sources)
  repro_check_objects(store, manifest_dir)
} else if (action == "objects" && length(arguments) == 1L) {
  repro_check_objects(store, manifest_dir)
} else if (action == "policies" && length(arguments) == 1L) {
  repro_policies(root, store, manifest_dir)
} else if (action == "ingest" && length(arguments) == 5L) {
  repro_ingest(arguments[2], arguments[3], arguments[4], arguments[5], store, sources)
} else if (action %in% c("update", "rebuild", "replay") && length(arguments) == 5L) {
  repro_dependencies(root)
  repro_run(root, store, sources, arguments[2], arguments[3], action, arguments[4], arguments[5])
} else {
  rlang::abort(paste("Invalid arguments for", action,
    "- builds require source_id run_id parent_run ingestion; use '-' for an absent parent/ingestion"),
    class = "repro_config_error")
}
