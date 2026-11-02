# Verify cached acquisition, dependency selection and publication failure safety offline.
library(dplyr)
source("R/source_helpers.R")
source("R/data_contract_helpers.R")
source("R/pipeline_helpers.R")
run_pipeline_tests <- function(project_root) {
fixture <- tempfile("pipeline-tests-")
dir.create(fixture)
original_mode <- getOption("pipeline.mode")
on.exit({ setwd(project_root); options(pipeline.mode = original_mode); unlink(fixture, recursive = TRUE) }, add = TRUE)

# Transport fixtures prove that an existing raw file triggers no request in cached modes.
helper <- new.env(parent = globalenv())
sys.source(file.path(project_root, "R/source_helpers.R"), envir = helper)
calls <- 0L
helper$download.file <- function(url, destfile, mode, quiet, method) {
  calls <<- calls + 1L
  writeLines("new source", destfile)
  0L
}
setwd(fixture)
dir.create("data/raw", recursive = TRUE)
writeLines("retained source", "data/raw/input.csv")
for (mode in c("rebuild", "resume")) {
  options(pipeline.mode = mode)
  helper$download_latest_file("https://example.invalid/source", "data/raw/input.csv", 1L, 1L)
}
stopifnot(calls == 0L, identical(readLines("data/raw/input.csv"), "retained source"))
options(pipeline.mode = "rebuild")
failure <- tryCatch(helper$download_latest_file("https://example.invalid/missing", "data/raw/missing.csv", 1L, 1L),
  source_cache_error = identity)
stopifnot(inherits(failure, "source_cache_error"), calls == 0L)
options(pipeline.mode = "update")
helper$download_latest_file("https://example.invalid/source", "data/raw/input.csv", 1L, 1L)
stopifnot(calls == 1L, identical(readLines("data/raw/input.csv"), "new source"),
  length(list.files("data/raw/vintages", recursive = TRUE)) == 2L)

# Minimal producer fixtures expose a shared dependency and an unrelated branch.
for (directory in c("R", "scripts", "registry", "config", "data/final")) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
writeLines("fixture", "renv.lock")
writeLines("fixture", "DESCRIPTION")
writeLines("date,value\n2020-01-01,2", "data/raw/source.csv")
writeLines("value\n99", "data/final/result.csv")
writeLines(c('# Prepare one retained source.',
  'data <- readr::read_csv("data/raw/source.csv", show_col_types = FALSE)',
  'readr::write_csv(data, "data/final/source.csv")'), "scripts/source.R")
writeLines(c('# Derive the reusable result.',
  'data <- readr::read_csv("data/final/source.csv", show_col_types = FALSE)',
  'data$value <- data$value * 2',
  'readr::write_csv(data, "data/final/result.csv")'), "scripts/result.R")
writeLines('stop("Unrelated branch must not run")', "scripts/unrelated.R")
steps <- tibble(step_id = c("source", "result", "unrelated"), domain = c("core", "core", "research"),
  script_path = c("scripts/source.R", "scripts/result.R", "scripts/unrelated.R"),
  depends_on = c("", "source", ""), input_files = c("data/raw/source.csv", "", ""),
  output_files = c("data/final/source.csv", "data/final/result.csv", "data/final/unrelated.csv"), arguments = "")
datasets <- tibble(dataset_id = "public_result", producer_step = "result")
readr::write_csv(steps, "registry/pipeline_steps.csv")
readr::write_csv(datasets, "registry/series_registry.csv")
contracts <- tibble(dataset_id = c("source", "result"), file_path = c("data/final/source.csv", "data/final/result.csv"), required_columns = "missing",
  key_columns = "date", date_column = "date", min_rows = 1L, max_age_days = 10L,
  non_negative_columns = "value", latest_non_missing_columns = "value")
# A deliberately unmet column contract must prevent every output from being published.
readr::write_csv(contracts, "registry/data_contracts.csv")
stopifnot(identical(pipeline_plan("public_result", steps, datasets)$step_id, c("source", "result")))
cycle <- steps
cycle$depends_on[[1]] <- "result"
failure <- tryCatch(pipeline_plan("result", cycle, datasets), pipeline_dependency_error = identity)
stopifnot(inherits(failure, "pipeline_dependency_error"))
failure <- tryCatch(pipeline_plan("missing", steps, datasets), pipeline_target_error = identity)
stopifnot(inherits(failure, "pipeline_target_error"))
before <- digest::digest(file = "data/final/result.csv", algo = "sha256")
failure <- tryCatch(pipeline_run("public_result", "rebuild", ""), pipeline_validation_error = identity)
stopifnot(inherits(failure, "pipeline_validation_error"), getwd() == normalizePath(fixture, winslash = "/"),
  digest::digest(file = "data/final/result.csv", algo = "sha256") == before,
  !file.exists("data/final/source.csv"), !file.exists("data/final/unrelated.csv"))
failed_runs <- list.dirs("data/pipeline/runs", recursive = FALSE, full.names = TRUE)
failed_parent <- failed_runs[vapply(failed_runs, function(path) {
  file.exists(file.path(path, "result.json")) && jsonlite::read_json(file.path(path, "result.json"))$status == "failed"
}, logical(1))][[1]]
# Simulate a response acquired before interruption; continuation must retain it without fetching again.
writeLines("captured response", file.path(failed_parent, "workspace/data/raw/acquired.csv"))

# Valid declared outputs are published together; raw bytes remain unchanged.
contracts$required_columns <- "date,value"
readr::write_csv(contracts, "registry/data_contracts.csv")
raw_hash <- digest::digest(file = "data/raw/source.csv", algo = "sha256")
receipt <- pipeline_run("public_result", "rebuild", basename(failed_parent))
stopifnot(readr::read_csv("data/final/result.csv", show_col_types = FALSE)$value == 4,
  file.exists("data/final/source.csv"), !file.exists("data/final/unrelated.csv"),
  digest::digest(file = "data/raw/source.csv", algo = "sha256") == raw_hash,
  jsonlite::read_json(file.path(receipt, "result.json"))$status == "success",
  identical(readLines("data/raw/acquired.csv"), "captured response"))
# A changed source scope cannot silently remove the previously published dates.
writeLines("date,value\n2021-01-01,2", "data/raw/source.csv")
failure <- tryCatch(pipeline_run("public_result", "rebuild", ""), pipeline_coverage_error = identity)
stopifnot(inherits(failure, "pipeline_coverage_error"),
  readr::read_csv("data/final/result.csv", show_col_types = FALSE)$date == as.Date("2020-01-01"))
setwd(project_root)
options(pipeline.mode = original_mode)
unlink(fixture, recursive = TRUE)
message("Offline acquisition, dependency planning and transactional publication checks passed")
}
run_pipeline_tests(project_root = getwd())

# Captured provider bytes are reparsed offline without inventing acquisition timestamps.
check_captured_api <- function(project_root) {
  fixture <- tempfile("captured-api-tests-")
  dir.create(fixture)
  previous_mode <- getOption("pipeline.mode")
  on.exit({setwd(project_root); options(pipeline.mode = previous_mode); unlink(fixture, recursive = TRUE)}, add = TRUE)
  setwd(fixture)
  dir.create("registry")
  dir.create("data/raw/api/fixture", recursive = TRUE)
  readr::write_csv(tibble(dataset_id = "fixture", raw_file = ""), "registry/series_registry.csv")
  url <- "https://api.worldbank.org/v2/country/VEN/indicator/TEST?format=json&per_page=20000"
  path <- file.path("data/raw/api/fixture", paste0(digest::digest(url, algo = "sha256", serialize = FALSE), ".json"))
  jsonlite::write_json(list(list(page = 1L), list(list(date = "2023", value = 10), list(date = "2024", value = 20))),
    path, auto_unbox = TRUE)
  options(pipeline.mode = "rebuild")
  result <- fetch_wb_indicator_set(dataset_id = "fixture", source_id = "wb",
    indicator_map = tibble(target_variable = "test", indicator_code = "TEST"), country = "VEN",
    start_year = 2023L, end_year = 2024L, max_attempts = 1L, timeout_seconds = 1L)
  stopifnot(nrow(result$data) == 2L, !nrow(result$errors), all(is.na(result$data$downloaded_at)),
    identical(result$data$value, c(10, 20)))
  message("Captured World Bank response reparsing and unknown acquisition-time checks passed")
}
check_captured_api(project_root = getwd())
