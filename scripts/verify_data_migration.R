# Compare published numeric observations and keys with a preserved pre-migration snapshot.
# Usage: Rscript scripts/verify_data_migration.R <snapshot_directory_containing_payload>
library(dplyr)
source("R/data_contract_helpers.R")
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L || !dir.exists(file.path(arguments[[1]], "payload"))) {
  rlang::abort("Supply one preserved snapshot directory containing payload/", class = "migration_arguments_error")
}
baseline <- file.path(arguments[[1]], "payload")
mapping <- readr::read_csv("registry/data_layer_migration.csv", show_col_types = FALSE)
procedures <- readr::read_csv("registry/pipeline_steps.csv", show_col_types = FALSE)
outputs <- unique(unlist(strsplit(procedures$output_files, ";", fixed = TRUE)))
outputs <- outputs[startsWith(outputs, "data/final/")]
contracts <- read_data_contracts("registry/data_contracts.csv")

# Source metadata paths and timestamps can change; observation keys and numeric values cannot.
results <- lapply(outputs, function(path) {
  legacy <- mapping$legacy_path[mapping$file_path == path]
  candidates <- c(file.path(baseline, legacy), file.path(baseline, path),
    file.path(baseline, "data/processed", basename(path)))
  before_path <- candidates[file.exists(candidates)][1]
  if (is.na(before_path) && basename(path) == "external_09_res_wb.csv") {
    before_path <- file.path(baseline, "data/processed/external_08_res_wb.csv")
  }
  if (is.na(before_path) || !file.exists(before_path)) {
    rlang::abort(paste("No baseline for", path), class = "migration_baseline_error")
  }
  before <- readr::read_csv(before_path, show_col_types = FALSE, num_threads = 1L,
    col_types = readr::cols(.default = readr::col_character()))
  after <- readr::read_csv(path, show_col_types = FALSE, num_threads = 1L,
    col_types = readr::cols(.default = readr::col_character()))
  # Exclusions move to interim; the daily history retains their carry-blocking dates.
  if (basename(path) == "external_15_fx_palma.csv") before <- before %>% filter(.data$quality_status == "usable")
  contract <- contracts %>% filter(.data$file_path == path)
  if (nrow(contract) != 1L) rlang::abort(paste("Missing unique contract", path), class = "migration_contract_error")
  keys <- split_contract_fields(contract$key_columns[[1]])
  missing_keys <- nrow(anti_join(before, after, by = keys))
  added_keys <- nrow(anti_join(after, before, by = keys))
  if (missing_keys || added_keys) rlang::abort(paste("Changed observation keys:", path,
    missing_keys, "removed;", added_keys, "added"), class = "migration_value_error")
  columns <- setdiff(intersect(names(before), names(after)), keys)
  columns <- columns[!grepl("file|path|hash|md5|sha256|downloaded|retrieved|generated|modified", columns)]
  paired <- inner_join(before, after, by = keys, suffix = c("_before", "_after"), relationship = "one-to-one")
  numeric_columns <- columns[vapply(columns, function(column) {
    values <- c(paired[[paste0(column, "_before")]], paired[[paste0(column, "_after")]])
    values <- values[!is.na(values)]
    length(values) > 0L && all(grepl("^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$", values))
  }, logical(1))]
  maximum_error <- 0
  for (column in numeric_columns) {
    old <- as.numeric(paired[[paste0(column, "_before")]])
    new <- as.numeric(paired[[paste0(column, "_after")]])
    if (any(is.na(old) != is.na(new))) rlang::abort(paste("Changed missingness", path, column), class = "migration_value_error")
    errors <- abs(new - old) / pmax(abs(old), 1)
    maximum_error <- max(c(maximum_error, errors), na.rm = TRUE)
    if (any(errors > 1e-12, na.rm = TRUE)) {
      rlang::abort(paste("Changed numeric observations", path, column), class = "migration_value_error")
    }
  }
  tibble(file_path = path, baseline_file = before_path, observations = nrow(after),
    numeric_columns = length(numeric_columns), max_relative_error = maximum_error,
    added_columns = paste(setdiff(names(after), names(before)), collapse = ";"), status = "pass")
})
dir.create("output/validation", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(bind_rows(results), "output/validation/data_migration_comparison.csv")
message("Observation keys and numeric values match across ", length(results), " final publications")
