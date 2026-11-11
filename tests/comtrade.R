# Exercise mirror direction, partial coverage, row caps, and offline failure safety.
library(tidyverse)
source("R/comtrade_helpers.R")

# Require a particular typed failure instead of allowing an unrelated error to pass.
expect_comtrade_error <- function(expression, expected_class) {
  caught <- tryCatch(force(expression), comtrade_config_error = identity,
    comtrade_truncation_error = identity, comtrade_dimension_error = identity,
    comtrade_observation_error = identity, comtrade_cache_error = identity,
    comtrade_response_error = identity, comtrade_schema_error = identity)
  stopifnot(inherits(caught, expected_class))
}

# Run synthetic evidence in an isolated directory, preserving the project's actual data.
check_comtrade <- function(project_root) {
  original_directory <- getwd()
  test_directory <- tempfile("comtrade-tests-")
  dir.create(test_directory)
  on.exit(setwd(original_directory), add = TRUE)
  on.exit(unlink(test_directory, recursive = TRUE), add = TRUE, after = TRUE)
  profile <- yaml::read_yaml(file.path(project_root, "config/comtrade.yml"))
  profile$start_period <- "2024"
  profile$end_period <- "2024"
  setwd(test_directory)
  yaml::write_yaml(profile, "profile.yml")
  config <- comtrade_config("profile.yml")
  queries <- comtrade_queries(config)
  stopifnot(identical(queries$flow, c("X", "M", "M", "X")))

  # The reporter reference includes an overlapping group, which must be excluded.
  reporters <- tibble(reporterCode = c(862L, 842L, 156L, 97L, 490L),
    reporterDesc = c("Venezuela", "USA", "China", "European Union", "Other Asia, nes"),
    reporterCodeIsoAlpha3 = c("VEN", "USA", "CHN", "EU", "OAS"), isGroup = c(FALSE, FALSE, FALSE, TRUE, TRUE))
  mirror_query <- queries %>% filter(.data$basis == "mirror", .data$direction == "exports")
  mirror_rows <- tibble(typeCode = "C", freqCode = "A", period = "2024",
    reporterCode = c(842L, 156L, 97L), partnerCode = 862L, partner2Code = 0L,
    flowCode = "M", cmdCode = "TOTAL", classificationCode = "H6",
    isOriginalClassification = TRUE, customsCode = "C00", motCode = 0L,
    primaryValue = c(100, 200, 900))
  body <- list(data = mirror_rows, count = 3L, error = "")
  records <- comtrade_records(body, mirror_query, config, reporters)
  receipt <- list(url = "https://example.test", retrieved_at = "2026-10-30T00:00:00Z", sha256 = "test")
  summary <- comtrade_summary(records, mirror_query, config, receipt)
  stopifnot(summary$value_usd == 300, summary$reporter_count == 2,
    summary$excluded_reporter_count == 1, summary$status == "available_reporters_only",
    summary$valuation == "imports_usually_CIF")
  empty <- comtrade_records(list(data = list(), count = 0, error = ""), mirror_query, config, reporters)
  missing <- comtrade_summary(empty, mirror_query, config, receipt)
  stopifnot(is.na(missing$value_usd), missing$status == "no_records", missing$reporter_count == 0)
  zeros <- body
  zeros$data$primaryValue <- c(0, 0, 0)
  zero_summary <- comtrade_summary(comtrade_records(zeros, mirror_query, config, reporters), mirror_query, config, receipt)
  stopifnot(zero_summary$value_usd == 0, zero_summary$reporter_count == 2)
  self_trade <- body
  self_trade$data <- bind_rows(mirror_rows, mirror_rows[1, ] %>% mutate(reporterCode = 862L))
  self_trade$count <- 4L
  self_summary <- comtrade_summary(comtrade_records(self_trade, mirror_query, config, reporters), mirror_query, config, receipt)
  stopifnot(self_summary$value_usd == 300, self_summary$excluded_reporter_count == 2)
  separate_area <- body
  separate_area$data <- bind_rows(mirror_rows, mirror_rows[1, ] %>% mutate(reporterCode = 490L, primaryValue = 50))
  separate_area$count <- 4L
  area_summary <- comtrade_summary(comtrade_records(separate_area, mirror_query, config, reporters), mirror_query, config, receipt)
  stopifnot(area_summary$value_usd == 350, area_summary$reporter_count == 3)

  # A reported world total must retain its own value, rather than use mirror partners.
  direct_query <- queries %>% filter(.data$basis == "direct", .data$direction == "exports")
  direct_body <- list(data = mirror_rows[1, ] %>% mutate(reporterCode = 862L, partnerCode = 0L,
    flowCode = "X", primaryValue = 450), count = 1L, error = "")
  direct_summary <- comtrade_summary(comtrade_records(direct_body, direct_query, config, reporters),
    direct_query, config, receipt)
  stopifnot(direct_summary$value_usd == 450, direct_summary$basis == "direct",
    direct_summary$status == "reporter_world_total", direct_summary$valuation == "exports_usually_FOB")

  # Truncated, mismatched, duplicate, missing-value and API-error responses must fail.
  capped <- config
  capped$max_records <- 3L
  expect_comtrade_error(comtrade_records(body, mirror_query, capped, reporters), "comtrade_truncation_error")
  wrong <- body
  wrong$data$partnerCode <- 0L
  expect_comtrade_error(comtrade_records(wrong, mirror_query, config, reporters), "comtrade_dimension_error")
  wrong <- body
  wrong$data$reporterCode <- c(842L, 842L, 97L)
  expect_comtrade_error(comtrade_records(wrong, mirror_query, config, reporters), "comtrade_observation_error")
  wrong <- body
  wrong$data$primaryValue[1] <- NA_real_
  expect_comtrade_error(comtrade_records(wrong, mirror_query, config, reporters), "comtrade_dimension_error")
  expect_comtrade_error(comtrade_records(list(data = list(), count = 0, error = "Access denied"),
    mirror_query, config, reporters), "comtrade_response_error")
  expect_comtrade_error(comtrade_records(list(data = list(), count = 1, error = ""),
    mirror_query, config, reporters), "comtrade_schema_error")

  # Monthly boundaries are calendar-aware; invalid months and missing settings fail.
  monthly <- profile
  monthly$frequency <- "M"
  monthly$start_period <- "202311"
  monthly$end_period <- "202402"
  yaml::write_yaml(monthly, "monthly.yml")
  stopifnot(identical(comtrade_config("monthly.yml")$periods, c("202311", "202312", "202401", "202402")))
  monthly$start_period <- "202313"
  yaml::write_yaml(monthly, "monthly.yml")
  expect_comtrade_error(comtrade_config("monthly.yml"), "comtrade_config_error")

  # Seed exact response caches, then exercise the real runner without any network calls.
  for (directory in c("objects", "cache")) dir.create(file.path(config$raw_directory, directory), recursive = TRUE)
  save_response <- function(url, payload) {
    bytes <- charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE, dataframe = "rows", null = "null"))
    sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    raw_path <- file.path(config$raw_directory, "objects", paste0(sha256, ".json"))
    writeBin(bytes, raw_path)
    cached <- list(url = url, raw_path = raw_path, sha256 = sha256,
      retrieved_at = "2026-10-30T00:00:00Z", http_status = 200L,
      etag = NULL, last_modified = NULL, retrieval = "downloaded")
    cache_path <- file.path(config$raw_directory, "cache",
      paste0(digest::digest(url, algo = "sha256", serialize = FALSE), ".json"))
    jsonlite::write_json(cached, cache_path, auto_unbox = TRUE, null = "null")
    raw_path
  }
  reference_path <- save_response("https://comtradeapi.un.org/files/v1/app/reference/Reporters.json", list(results = reporters))
  for (index in seq_len(nrow(queries))) {
    query <- queries[index, ]
    request <- httr2::request("https://comtradeapi.un.org/public/v1/preview/C/A/HS") %>%
      httr2::req_url_query(period = query$period, partnerCode = query$partner, cmdCode = query$product,
        flowCode = query$flow, partner2Code = "0", customsCode = "C00", motCode = "0",
        maxRecords = config$max_records, includeDesc = "true")
    if (!is.na(query$reporter)) request <- httr2::req_url_query(request, reporterCode = query$reporter)
    payload <- if (query$basis == "direct") list(data = list(), count = 0L, error = "") else
      list(data = mirror_rows %>% mutate(flowCode = query$flow), count = 3L, error = "")
    save_response(request$url, payload)
  }
  result <- comtrade_run("profile.yml", "rebuild")
  output_paths <- c("data/final/external_14_trade_comtrade_A.csv",
    "data/final/external_14_trade_comtrade_A_observations.csv")
  stopifnot(nrow(result$summary) == 4, nrow(result$observations) == 6)
  output_hashes <- tools::md5sum(output_paths)
  comtrade_run("profile.yml", "rebuild")
  stopifnot(identical(output_hashes, tools::md5sum(output_paths)))

  # A broken cached object stops rebuilds and cannot overwrite the existing final data.
  writeBin(charToRaw("tampered"), reference_path)
  expect_comtrade_error(comtrade_run("profile.yml", "rebuild"), "comtrade_cache_error")
  stopifnot(identical(output_hashes, tools::md5sum(output_paths)))
  message("Comtrade checks passed: mirror directions, coverage, validation, offline rebuild and failure preservation")
}

check_comtrade(normalizePath(".", winslash = "/", mustWork = TRUE))
