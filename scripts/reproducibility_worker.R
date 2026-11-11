# Worker launched only by run_reproducibility.R with an immutable run configuration.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) stop("Worker requires one configuration path")
config <- readRDS(arguments[1])
.libPaths(config$library_paths)
setwd(config$workspace)
if (identical(config$mode, "test")) {
  sys.source(config$script, envir = new.env(parent = globalenv()), keep.source = TRUE)
  quit(save = "no", status = 0L)
}
source("R/reproducibility.R")
library(dplyr)
repro_dependencies(getwd())
options(repro.source = config$source_id)
options(repro.command = list(entry_point = "scripts/run_reproducibility.R",
  arguments = c(config$mode, config$source_id, basename(config$run_dir),
    if (nzchar(config$parent_run)) config$parent_run else "-",
    if (nzchar(config$ingestion)) config$ingestion else "-")))
# Pin source generation dates to the run clock; HTTP receipts use base::Sys.time
# for real retrieval times. Replay reuses its parent's clock and environment.
Sys.time <- local({ instant <- as.POSIXct(config$clock, origin = "1970-01-01", tz = "UTC"); function() instant })
Sys.Date <- local({ date <- as.Date(Sys.time(), tz = "UTC"); function() date })
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", ".UTF-8")
Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_COLLATE", "C")
if (length(config$inherited_environment)) do.call(Sys.setenv, as.list(config$inherited_environment))
# Inherit the active project's pinned library, rather than installing globally.
for (setting in config$environment[nzchar(config$environment)]) {
  parts <- strsplit(setting, "=", fixed = TRUE)[[1]]
  if (length(parts) != 2L) rlang::abort("Invalid environment setting", class = "repro_config_error")
  do.call(Sys.setenv, stats::setNames(list(parts[2]), parts[1]))
}
transport <- repro_transport(if (config$mode == "replay") "replay" else "update",
  file.path(config$run_dir, "http"), file.path(config$store, "runs", config$parent_run, "http"), config$store)
options(repro.download = transport,
  repro.json = local({ fetch <- transport; function(url) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    fetch(url, path, 3L, 120L)
    jsonlite::fromJSON(path, flatten = TRUE)
  }}),
  repro.imf = local({ fetch <- transport; parser <- repro_imf_request
    function(flow_id, key, start_year, end_year) parser(flow_id, key, start_year, end_year, fetch)
  }))
# Source cache semantics also apply to direct parser calls in isolated builds.
options(pipeline.mode = if (config$mode == "rebuild") "rebuild" else "update")
# Offline profiles read only staged inputs. Shared transport calls are rejected
# unless this is a response replay; direct-network profiles cannot claim replay.
if (config$mode == "rebuild") {
  # A cached cross-country rebuild must override provider-specific refresh flags.
  if (config$source_id == "cross_country_macro") {
    Sys.setenv(CROSS_COUNTRY_REFRESH = "false", CROSS_COUNTRY_REFRESH_WDI = "false",
      CROSS_COUNTRY_REFRESH_IMF = "false", CROSS_COUNTRY_REFRESH_DATA360 = "false")
  }
  options(repro.download = function(url, dest_path, max_attempts, timeout_seconds) {
    if (!file.exists(dest_path)) rlang::abort(paste("Missing offline input", dest_path), class = "repro_replay_missing")
    dest_path
  }, repro.json = function(url) rlang::abort("Offline API request", class = "repro_offline_error"),
  repro.imf = function(flow_id, key, start_year, end_year) rlang::abort("Offline IMF request", class = "repro_offline_error"))
}
# Save the actual R/package runtime and requested transformation sequence.
writeLines(capture.output(sessionInfo()), file.path(config$run_dir, "session-info.txt"))
for (script in config$scripts) {
  message("Running ", script)
  environment <- new.env(parent = globalenv())
  if (!is.null(config$script_arguments) && nzchar(config$script_arguments)) {
    values <- strsplit(gsub("{mode}", config$mode, config$script_arguments, fixed = TRUE), ";", fixed = TRUE)[[1]]
    environment$commandArgs <- local({ arguments <- values; function(trailingOnly) arguments })
  }
  sys.source(script, envir = environment, keep.source = TRUE)
}
