# Check the curated Reuters rebuild in a temporary workspace without changing raw inputs.
library(tidyverse)

check_reuters_pipeline <- function(project_root) {
  original_directory <- getwd()
  test_directory <- tempfile(pattern = "reuters-exports-")
  dir.create(test_directory)
  on.exit(setwd(original_directory), add = TRUE)
  on.exit(unlink(test_directory, recursive = TRUE), add = TRUE, after = TRUE)
  for (directory in c("R", "scripts", "config", "data/raw/reuters_oil_exports")) {
    dir.create(file.path(test_directory, directory), recursive = TRUE)
  }
  # Copy only the actual offline inputs and shared code needed by the processor and graph.
  input_paths <- c("R/source_helpers.R", "R/report_figures.R", "R/report_inputs.R",
    "scripts/oil_15_exports_reuters.R", "scripts/oil_91_exports_graph.R", "config/report.yml",
    file.path("data/raw/reuters_oil_exports", c("snapshot.json", "reviewed_sources.json",
      "chart_2020_05_04.js", "chart_2021_06_02.json", "user_gap_fills.csv")))
  stopifnot(all(file.copy(file.path(project_root, input_paths), file.path(test_directory, input_paths))))
  baseline <- readr::read_csv(file.path(project_root,
    "data/raw/reuters_oil_exports/review_2026-10-20/oil_15_exports_reuters_monthly.csv"),
    show_col_types = FALSE)
  setwd(test_directory)
  raw_path <- "data/raw/reuters_oil_exports/reviewed_sources.json"
  reviewed <- jsonlite::fromJSON(raw_path, simplifyVector = FALSE)
  hashes_before <- tools::md5sum(input_paths)
  sys.source("scripts/oil_15_exports_reuters.R", envir = new.env(parent = globalenv()))
  monthly <- readr::read_csv("data/final/oil_15_exports_reuters_monthly.csv", show_col_types = FALSE)
  evidence <- readr::read_csv("data/interim/oil_15_exports_reuters_vintages.csv", show_col_types = FALSE)

  # The initial integration preserves the archived baseline; later reviewed vintages may revise it.
  comparison <- inner_join(monthly, baseline %>% filter(!is.na(.data$exports_bpd)),
    by = "date", suffix = c("", "_baseline"))
  if (all(monthly$retrieved_date == as.Date("2026-10-20"))) {
    stopifnot(nrow(comparison) == sum(!is.na(baseline$exports_bpd)),
      identical(comparison$exports_bpd, comparison$exports_bpd_baseline),
      identical(comparison$status, comparison$status_baseline))
  }
  stopifnot(identical(hashes_before, tools::md5sum(input_paths)),
    all(monthly$exports_bpd[!is.na(monthly$exports_bpd)] > 0),
    all(monthly$retrieved_date >= monthly$source_date, na.rm = TRUE))
  # Preserve the user's original kb/d units and identify all three additions separately.
  fills <- monthly %>% filter(.data$status == "user_supplied_gap_fill") %>% arrange(.data$date)
  stopifnot(identical(fills$month, c("2019-02", "2021-08", "2022-12")),
    isTRUE(all.equal(fills$exports_bpd, c(987360, 629490, 596000))),
    all(fills$raw_unit == "thousand_bpd"), all(fills$access == "user_supplied"),
    all(is.na(fills$source_url)), !anyNA(monthly$exports_bpd))
  selected <- evidence %>% filter(.data$selected)
  latest <- evidence %>% filter(.data$eligible) %>% group_by(.data$date) %>%
    summarise(latest_source_date = max(.data$source_date), .groups = "drop")
  selection_audit <- left_join(selected, latest, by = "date")
  stopifnot(nrow(selected) == sum(!is.na(monthly$exports_bpd)),
    all(selected$eligible),
    all(selection_audit$source_date == selection_audit$latest_source_date))

  # Rebuilding the same evidence must produce identical outputs.
  final_paths <- list.files("data/final", full.names = TRUE)
  final_hashes <- tools::md5sum(final_paths)
  sys.source("scripts/oil_15_exports_reuters.R", envir = new.env(parent = globalenv()))
  stopifnot(identical(final_hashes, tools::md5sum(final_paths)))

  # Unknown provenance and units must fail explicitly before any output is replaced.
  for (invalid_field in c("access", "unit")) {
    invalid <- reviewed
    if (invalid_field == "access") invalid[[1]]$access <- "unreviewed"
    if (invalid_field == "unit") invalid[[1]]$observations[[1]][[3]] <- "tonnes"
    jsonlite::write_json(invalid, raw_path, auto_unbox = TRUE, pretty = TRUE)
    expected_class <- if (invalid_field == "access") "reuters_provenance_error" else "reuters_observation_error"
    actual <- tryCatch(sys.source("scripts/oil_15_exports_reuters.R", envir = new.env(parent = globalenv())),
      reuters_provenance_error = identity, reuters_observation_error = identity)
    stopifnot(inherits(actual, expected_class), identical(final_hashes, tools::md5sum(final_paths)))
  }
  jsonlite::write_json(reviewed, raw_path, auto_unbox = TRUE, pretty = TRUE)

  # A user-supplied candidate must never replace an existing eligible Reuters value.
  user_path <- "data/raw/reuters_oil_exports/user_gap_fills.csv"
  original_fills <- read_csv(user_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  candidate <- original_fills[1, ] %>% mutate(date = "2026-09-01", raw_value = "999")
  write_csv(bind_rows(original_fills, candidate), user_path)
  sys.source("scripts/oil_15_exports_reuters.R", envir = new.env(parent = globalenv()))
  priority_result <- read_csv("data/final/oil_15_exports_reuters_monthly.csv", show_col_types = FALSE) %>%
    filter(.data$month == "2026-09")
  stopifnot(priority_result$exports_bpd == 1080000, priority_result$access != "user_supplied")
  write_csv(original_fills, user_path)
  sys.source("scripts/oil_15_exports_reuters.R", envir = new.env(parent = globalenv()))

  # A conflicting same-day report must be resolved by review rather than source-ID order.
  conflict <- reviewed[[1]]
  conflict$url <- paste0(conflict$url, "?conflicting-test-vintage")
  conflict$observations[[1]][[2]] <- "999"
  jsonlite::write_json(append(reviewed, list(conflict)), raw_path, auto_unbox = TRUE, pretty = TRUE)
  conflict_error <- tryCatch(sys.source("scripts/oil_15_exports_reuters.R", envir = new.env(parent = globalenv())),
    reuters_vintage_conflict = identity)
  stopifnot(inherits(conflict_error, "reuters_vintage_conflict"),
    identical(final_hashes, tools::md5sum(final_paths)))
  jsonlite::write_json(reviewed, raw_path, auto_unbox = TRUE, pretty = TRUE)

  # An earlier report edition must exclude later months and retain visible line breaks.
  config <- yaml::read_yaml("config/report.yml")
  config$date <- "2025-02-07"
  yaml::write_yaml(config, "config/report.yml")
  graph_environment <- new.env(parent = globalenv())
  sys.source("scripts/oil_91_exports_graph.R", envir = graph_environment)
  graph <- graph_environment$exports
  stopifnot(max(graph$date) == as.Date("2025-01-01"),
    sum(is.na(graph$exports_bpd)) == sum(is.na(monthly$exports_bpd[monthly$date <= as.Date("2025-01-01")])),
    file.exists("outputs/figures/oil_15_exports_reuters_monthly.png"))
  message("Reuters raw snapshot, selection, failure handling and report cutoff checks passed.")
}

check_reuters_pipeline(project_root = normalizePath(getwd(), winslash = "/", mustWork = TRUE))
