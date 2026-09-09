# Run after a successful corpus build to verify deterministic outputs and cache reuse.
source("R/source_helpers.R")
outputs <- c("data/processed/oil_13_momr_corpus.csv", "data/final/oil_13_momr_latest.csv",
             "data/final/oil_13_momr_coverage.csv", "data/final/oil_13_momr_gaps.csv",
             "data/final/oil_13_momr_report_audit.csv")
if (!all(file.exists(outputs))) {
  rlang::abort("Build the OPEC corpus before testing cached reproducibility", class = "opec_test_prerequisite")
}
before <- tools::md5sum(outputs)
caches <- list.files("data/interim/opec", pattern = "^[0-9]{4}-[0-9]{2}\\.rds$", full.names = TRUE)
cache_times <- file.info(caches)$mtime
run_script_isolated("scripts/oil_13_process_momr_corpus.R")
stopifnot(identical(before, tools::md5sum(outputs)), identical(cache_times, file.info(caches)$mtime))
message("OPEC cached rebuild is byte-identical; extraction caches were reused.")
