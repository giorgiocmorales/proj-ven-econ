# Verify API pagination, provider overrides, and runner failure propagation offline.
library(tidyverse)
source("R/source_helpers.R")
source("R/cross_country_api_helpers.R")
runner_path <- normalizePath("scripts/run_cross_country.R", winslash = "/")
source_helpers_path <- normalizePath("R/source_helpers.R", winslash = "/")
api_helpers_path <- normalizePath("R/cross_country_api_helpers.R", winslash = "/")
fixture_dir <- tempfile("cross_country_api_")
dir.create(fixture_dir)

## Error assertions ----
# Catch only the expected error class so unrelated failures retain their root cause.
expect_macro_error <- function(expression, error_class) {
  caught <- tryCatch({ force(expression); FALSE },
                     macro_api_schema_error = function(e) inherits(e, error_class),
                     macro_duplicate_error = function(e) inherits(e, error_class),
                     macro_cache_error = function(e) inherits(e, error_class),
                     macro_config_error = function(e) inherits(e, error_class),
                     macro_value_error = function(e) inherits(e, error_class))
  stopifnot(isTRUE(caught))
}

## Provider flags ----
# An unset provider flag inherits its explicit fallback; invalid values must fail.
flag_name <- "CROSS_COUNTRY_TEST_REFRESH"
original_flag <- Sys.getenv(flag_name, unset = NA_character_)
Sys.unsetenv(flag_name)
stopifnot(cross_country_refresh_flag(flag_name, TRUE),
          !cross_country_refresh_flag(flag_name, FALSE))
Sys.setenv(CROSS_COUNTRY_TEST_REFRESH = "false")
stopifnot(!cross_country_refresh_flag(flag_name, TRUE))
Sys.setenv(CROSS_COUNTRY_TEST_REFRESH = "invalid")
expect_macro_error(cross_country_refresh_flag(flag_name, TRUE), "macro_config_error")
if (is.na(original_flag)) Sys.unsetenv(flag_name) else
  do.call(Sys.setenv, setNames(list(original_flag), flag_name))

## Data360 pages ----
# Two small pages prove that the reader follows offsets through the advertised total.
indicator_id <- "WB_CCDFS_PBY"
observation <- tibble(REF_AREA = "VEN", INDICATOR = indicator_id, TIME_PERIOD = "2000",
                      OBS_VALUE = "1.5", FREQ = "A", UNIT_MEASURE = "PT_GDP",
                      UNIT_MULT = 0L, OBS_STATUS = "A")
second_observation <- observation %>% mutate(TIME_PERIOD = "2001", OBS_VALUE = "2.5")
first_path <- file.path(fixture_dir, paste0(indicator_id, "_offset_0.json"))
second_path <- file.path(fixture_dir, paste0(indicator_id, "_offset_1.json"))
jsonlite::write_json(list(count = 2L, value = observation), first_path, auto_unbox = TRUE)
jsonlite::write_json(list(count = 2L, value = second_observation), second_path, auto_unbox = TRUE)
result <- read_data360_pages(indicator_id, fixture_dir, FALSE)
stopifnot(nrow(result) == 2L, identical(result$TIME_PERIOD, c("2000", "2001")))

# Reject changing counts, duplicate keys, empty pages, and unrelated indicators.
jsonlite::write_json(list(count = 3L, value = second_observation), second_path, auto_unbox = TRUE)
expect_macro_error(read_data360_pages(indicator_id, fixture_dir, FALSE), "macro_api_schema_error")
jsonlite::write_json(list(count = 2L, value = observation), second_path, auto_unbox = TRUE)
expect_macro_error(read_data360_pages(indicator_id, fixture_dir, FALSE), "macro_api_schema_error")
jsonlite::write_json(list(count = 2L, value = list()), second_path, auto_unbox = TRUE)
expect_macro_error(read_data360_pages(indicator_id, fixture_dir, FALSE), "macro_api_schema_error")
jsonlite::write_json(list(count = 2L, value = observation %>% mutate(INDICATOR = "WB_CCDFS_FBY")),
                     first_path, auto_unbox = TRUE)
expect_macro_error(read_data360_pages(indicator_id, fixture_dir, FALSE), "macro_api_schema_error")
jsonlite::write_json(list(count = 2L, value = observation %>% select(-OBS_VALUE)),
                     first_path, auto_unbox = TRUE)
expect_macro_error(read_data360_pages(indicator_id, fixture_dir, FALSE), "macro_api_schema_error")
expect_macro_error(cache_response("https://example.org", "missing.json", fixture_dir, FALSE), "macro_cache_error")

## World Bank WDI pages ----
# The classic API's page number and total count are checked independently of Data360.
jsonlite::write_json(list(list(page = 1L, pages = 2L, total = 2L), observation),
                     file.path(fixture_dir, "wdi_page_1.json"), auto_unbox = TRUE)
jsonlite::write_json(list(list(page = 2L, pages = 2L, total = 2L), second_observation),
                     file.path(fixture_dir, "wdi_page_2.json"), auto_unbox = TRUE)
stopifnot(nrow(read_wb_pages("https://example.org?format=json", "wdi", fixture_dir, FALSE)) == 2L)
jsonlite::write_json(list(list(page = 2L, pages = 2L, total = 3L), second_observation),
                     file.path(fixture_dir, "wdi_page_2.json"), auto_unbox = TRUE)
expect_macro_error(read_wb_pages("https://example.org?format=json", "wdi", fixture_dir, FALSE), "macro_api_schema_error")
jsonlite::write_json(list(list(page = 2L, pages = 2L, total = NA_integer_), second_observation),
                     file.path(fixture_dir, "wdi_page_2.json"), auto_unbox = TRUE)
expect_macro_error(read_wb_pages("https://example.org?format=json", "wdi", fixture_dir, FALSE), "macro_api_schema_error")

## Numeric validity ----
# Scientific notation and legitimate missing values remain valid; malformed data fails.
stopifnot(identical(cross_country_numeric(c("1.5", "1e2", NA_character_), "fixture"), c(1.5, 100, NA_real_)))
expect_macro_error(cross_country_numeric("NaN", "fixture"), "macro_value_error")
expect_macro_error(cross_country_numeric("Inf", "fixture"), "macro_value_error")
expect_macro_error(cross_country_numeric("bad value", "fixture"), "macro_value_error")

## Runner sequencing ----
# A failing downloader must prevent the screening script from executing.
runner_fixture <- file.path(fixture_dir, "runner")
dir.create(file.path(runner_fixture, "R"), recursive = TRUE)
dir.create(file.path(runner_fixture, "scripts"))
writeLines(paste0('source("', source_helpers_path, '")'), file.path(runner_fixture, "R/source_helpers.R"))
# Include the runner's API helper so this fixture reaches the deliberate download failure.
stopifnot(file.copy(api_helpers_path, file.path(runner_fixture, "R/cross_country_api_helpers.R")))
writeLines('rlang::abort("Deliberate fixture failure", class = "macro_api_schema_error")',
           file.path(runner_fixture, "scripts/cross_country_01_download.R"))
writeLines('writeLines("Screen ran", "screen_sentinel.txt")',
           file.path(runner_fixture, "scripts/cross_country_02_screen.R"))
project_dir <- getwd()
setwd(runner_fixture)
expect_macro_error(source(runner_path), "macro_api_schema_error")
stopifnot(!file.exists("screen_sentinel.txt"))

## Download failures ----
# A transport error retains its original message and leaves the previous cache intact.
helper_env <- new.env(parent = globalenv())
sys.source(source_helpers_path, envir = helper_env)
helper_env$download.file <- function(url, destfile, mode, quiet, method) {
  stop("Fixture transport failure", call. = FALSE)
}
preserved_path <- file.path(fixture_dir, "preserved_cache.json")
writeLines("Previous cache", preserved_path)
transport_error <- tryCatch(
  helper_env$download_latest_file("https://example.org", preserved_path, max_attempts = 1, timeout_seconds = 1),
  simpleError = function(e) conditionMessage(e)
)
stopifnot(grepl("Fixture transport failure", transport_error, fixed = TRUE),
          identical(readLines(preserved_path), "Previous cache"))
setwd(project_dir)
message("Cross-country API pagination, configuration, numeric checks, and runner failure propagation passed.")
