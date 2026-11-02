# UN Comtrade acquisition and validation using the project's existing dependencies.
# Requests follow https://uncomtrade.org/docs/un-comtrade-api/ and the mirror query
# described at https://uncomtrade.org/docs/how-to-retrieve-data-for-world-exports-to-a-country-in-un-comtrade/.

# Code 490 is a separate customs area, despite isGroup in the API reference; its trade
# is not included in China's data: https://uncomtrade.org/docs/taiwan-province-of-china-trade-data/.
comtrade_separate_customs_areas <- c(490L)

# Validate an explicit profile before any network request or output replacement.
comtrade_config <- function(path) {
  config <- yaml::read_yaml(path)
  required <- c("country_code", "frequency", "start_period", "end_period", "classification",
    "products", "include_direct", "include_mirror", "access", "max_records",
    "request_interval_seconds", "timeout_seconds", "max_attempts", "raw_directory", "dataset_id")
  if (!is.list(config) || !all(required %in% names(config)) || any(vapply(config[required], is.null, logical(1)))) {
    rlang::abort("Comtrade profile is missing required settings", class = "comtrade_config_error")
  }
  scalar <- setdiff(required, "products")
  if (any(lengths(config[scalar]) != 1L) || anyNA(unlist(config[required]))) {
    rlang::abort("Comtrade settings must be nonmissing scalars, except products", class = "comtrade_config_error")
  }
  config$products <- as.character(unlist(config$products))
  if (!is.numeric(config$country_code) || config$country_code <= 0 || config$country_code %% 1 != 0 ||
      !config$frequency %in% c("A", "M") || !config$access %in% c("public_preview", "authenticated") ||
      !grepl("^(HS|H[0-9])$", config$classification) || length(config$products) == 0 ||
      any(!grepl("^(TOTAL|[0-9]{2}|[0-9]{4}|[0-9]{6})$", config$products)) ||
      anyDuplicated(config$products) || !is.logical(config$include_direct) ||
      !is.logical(config$include_mirror) || !(config$include_direct || config$include_mirror) ||
      !grepl("^[a-z][a-z0-9_]+$", config$dataset_id) ||
      !is.character(config$raw_directory) || !nzchar(config$raw_directory)) {
    rlang::abort("Invalid Comtrade country, frequency, products, access, or dataset settings", class = "comtrade_config_error")
  }
  limits <- c("max_records", "request_interval_seconds", "timeout_seconds", "max_attempts")
  if (any(!vapply(config[limits], is.numeric, logical(1))) ||
      any(!is.finite(unlist(config[limits]))) || any(unlist(config[limits]) <= 0) ||
      config$request_interval_seconds < 1.1 || config$max_records %% 1 != 0 ||
      config$max_attempts %% 1 != 0 || config$max_records > 100000 ||
      (config$access == "public_preview" && config$max_records > 500)) {
    rlang::abort("Invalid Comtrade request limits; preview allows at most 500 rows", class = "comtrade_config_error")
  }
  # Parse calendar periods strictly, including month boundaries and ordering.
  period_pattern <- if (config$frequency == "A") "^[0-9]{4}$" else "^[0-9]{6}$"
  boundaries <- as.character(c(config$start_period, config$end_period))
  if (any(!grepl(period_pattern, boundaries))) {
    rlang::abort("Comtrade periods must be YYYY for annual or YYYYMM for monthly", class = "comtrade_config_error")
  }
  dates <- as.Date(paste0(boundaries, if (config$frequency == "A") "0101" else "01"), format = "%Y%m%d")
  if (anyNA(dates) || dates[1] > dates[2] ||
      any(format(dates, if (config$frequency == "A") "%Y" else "%Y%m") != boundaries)) {
    rlang::abort("Invalid or reversed Comtrade calendar periods", class = "comtrade_config_error")
  }
  config$periods <- format(seq(dates[1], dates[2], by = if (config$frequency == "A") "year" else "month"),
    if (config$frequency == "A") "%Y" else "%Y%m")
  config
}

# Query one period/product/flow at a time so preview queries stay below their cap.
comtrade_queries <- function(config) {
  bases <- c(if (config$include_direct) "direct", if (config$include_mirror) "mirror")
  tidyr::expand_grid(period = config$periods, product = config$products,
    direction = c("exports", "imports"), basis = bases) %>%
    dplyr::mutate(flow = dplyr::if_else((.data$direction == "exports") == (.data$basis == "direct"), "X", "M"),
      reporter = dplyr::if_else(.data$basis == "direct", as.character(config$country_code), NA_character_),
      partner = dplyr::if_else(.data$basis == "direct", "0", as.character(config$country_code)))
}

# Archive response bytes, safe headers, hashes and retrieval times; keys never enter URLs.
# update refreshes every request; resume reuses saved requests; rebuild forbids network access.
comtrade_fetch <- function(url, config, mode, run_directory) {
  query_hash <- digest::digest(url, algo = "sha256", serialize = FALSE)
  cache_path <- file.path(config$raw_directory, "cache", paste0(query_hash, ".json"))
  if (mode != "update" && file.exists(cache_path)) {
    receipt <- jsonlite::read_json(cache_path, simplifyVector = TRUE)
    if (!identical(receipt$url, url) || !file.exists(receipt$raw_path) ||
        !identical(digest::digest(file = receipt$raw_path, algo = "sha256", serialize = FALSE), receipt$sha256)) {
      rlang::abort("Comtrade cached response is missing or has changed", class = "comtrade_cache_error")
    }
    receipt$retrieval <- "cached"
  } else {
    if (mode == "rebuild") {
      rlang::abort(paste("No saved Comtrade response for", url), class = "comtrade_cache_error")
    }
    request <- httr2::request(url) %>%
      httr2::req_user_agent("proj-ven-econ UN Comtrade research downloader") %>%
      httr2::req_timeout(seconds = config$timeout_seconds) %>%
      httr2::req_throttle(capacity = 1, fill_time_s = config$request_interval_seconds,
        realm = "proj-ven-econ-comtrade") %>%
      httr2::req_retry(max_tries = config$max_attempts, retry_on_failure = TRUE)
    if (config$access == "authenticated" && grepl("/data/v1/get/", url, fixed = TRUE)) {
      key <- Sys.getenv("COMTRADE_PRIMARY", unset = "")
      if (!nzchar(key)) rlang::abort("Set COMTRADE_PRIMARY before authenticated requests", class = "comtrade_credentials_error")
      request <- httr2::req_headers(request, `Ocp-Apim-Subscription-Key` = key,
        .redact = "Ocp-Apim-Subscription-Key")
    }
    # httr2 raises typed HTTP/connection errors and honors Retry-After for rate limits.
    response <- httr2::req_perform(request)
    bytes <- httr2::resp_body_raw(response)
    sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    raw_path <- file.path(config$raw_directory, "objects", paste0(sha256, ".json"))
    if (!file.exists(raw_path)) writeBin(bytes, raw_path)
    receipt <- list(url = url, raw_path = raw_path, sha256 = sha256,
      retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      http_status = httr2::resp_status(response), etag = httr2::resp_header(response, "etag"),
      last_modified = httr2::resp_header(response, "last-modified"), retrieval = "downloaded")
    jsonlite::write_json(receipt, cache_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  }
  # A content-addressed file must still match its name if an existing object is reused.
  if (!identical(digest::digest(file = receipt$raw_path, algo = "sha256", serialize = FALSE), receipt$sha256)) {
    rlang::abort("Comtrade archived response checksum mismatch", class = "comtrade_cache_error")
  }
  # Every run points to its exact response vintage, including successful empty queries.
  jsonlite::write_json(receipt, file.path(run_directory, paste0(query_hash, ".json")),
    auto_unbox = TRUE, pretty = TRUE, null = "null")
  list(body = jsonlite::fromJSON(receipt$raw_path), receipt = receipt)
}

# Reject API error bodies, truncated previews, wrong query dimensions and duplicates.
comtrade_records <- function(body, query, config, reporters) {
  if (!is.list(body) || !all(c("data", "count", "error") %in% names(body)) ||
      length(body$count) != 1 || is.na(body$count) || !is.numeric(body$count) || body$count < 0 ||
      !(is.null(body$error) || identical(body$error, "") || identical(body$error, FALSE))) {
    rlang::abort("Comtrade returned an API error or an unexpected response envelope", class = "comtrade_response_error")
  }
  records <- body$data
  size <- if (is.null(records) || length(records) == 0) 0L else if (is.data.frame(records)) nrow(records) else NA_integer_
  if (is.na(size) || size != body$count) {
    rlang::abort("Comtrade count does not match the returned records", class = "comtrade_schema_error")
  }
  if (size >= config$max_records) {
    rlang::abort("Comtrade response reached the row cap; narrow the query before using it", class = "comtrade_truncation_error")
  }
  if (size == 0L) return(tibble::tibble())
  required <- c("typeCode", "freqCode", "period", "reporterCode", "partnerCode", "partner2Code",
    "flowCode", "cmdCode", "classificationCode", "isOriginalClassification", "customsCode", "motCode", "primaryValue")
  if (!all(required %in% names(records))) {
    rlang::abort("Comtrade response is missing trade dimensions or values", class = "comtrade_schema_error")
  }
  dimensions <- records[, required]
  if (anyNA(dimensions) || any(records$typeCode != "C" | records$freqCode != config$frequency |
      as.character(records$period) != query$period | records$flowCode != query$flow |
      records$cmdCode != query$product | records$partnerCode != as.integer(query$partner) |
      records$partner2Code != 0 | records$customsCode != "C00" | records$motCode != 0) ||
      any(!records$reporterCode %in% reporters$reporterCode) ||
      (query$basis == "direct" && any(records$reporterCode != config$country_code)) ||
      (config$classification == "HS" && any(!records$isOriginalClassification)) ||
      (config$classification != "HS" && any(records$classificationCode != config$classification))) {
    rlang::abort("Comtrade response does not match the requested dimensions or reporter reference", class = "comtrade_dimension_error")
  }
  if (!is.numeric(records$primaryValue) || any(!is.finite(records$primaryValue) | records$primaryValue < 0) ||
      anyDuplicated(records$reporterCode)) {
    rlang::abort("Comtrade response has invalid values or duplicate reporter totals", class = "comtrade_observation_error")
  }
  tibble::as_tibble(records) %>%
    dplyr::left_join(reporters %>% dplyr::select(reporterCode, reporterDesc, reporterCodeIsoAlpha3, isGroup),
      by = "reporterCode", suffix = c("_api", "")) %>%
    dplyr::mutate(is_overlapping_group = .data$isGroup & !.data$reporterCode %in% comtrade_separate_customs_areas,
      included = !.data$is_overlapping_group & (query$basis == "direct" | .data$reporterCode != config$country_code),
      exclusion_reason = dplyr::case_when(.data$is_overlapping_group ~ "reporter_group",
        !.data$included ~ "self_trade", TRUE ~ NA_character_))
}

# Preserve both measurement bases; a missing series is NA, never a fabricated zero.
comtrade_summary <- function(records, query, config, receipt) {
  included <- if (nrow(records) == 0) records else dplyr::filter(records, .data$included)
  tibble::tibble(country_code = config$country_code, frequency = config$frequency,
    period = query$period,
    date = as.Date(paste0(query$period, if (config$frequency == "A") "0101" else "01"), format = "%Y%m%d"),
    product = query$product, classification_requested = config$classification,
    direction = query$direction, basis = query$basis, api_flow = query$flow,
    value_usd = if (nrow(included) == 0) NA_real_ else sum(included$primaryValue),
    unit = "current_USD", reporter_count = nrow(included), excluded_reporter_count = nrow(records) - nrow(included),
    status = if (nrow(included) == 0) "no_records" else if (query$basis == "mirror") "available_reporters_only" else "reporter_world_total",
    valuation = if (query$flow == "M") "imports_usually_CIF" else "exports_usually_FOB",
    source_url = receipt$url, retrieved_at = receipt$retrieved_at, response_sha256 = receipt$sha256)
}

# Run independently of the macro runner: trade requests should be deliberate and resumable.
# Existing final extracts are replaced only after every query and validation succeeds.
comtrade_run <- function(config_path, mode) {
  if (length(mode) != 1 || !mode %in% c("update", "resume", "rebuild")) {
    rlang::abort("Comtrade mode must be update, resume, or rebuild", class = "comtrade_config_error")
  }
  config <- comtrade_config(config_path)
  if (config$access == "authenticated" && mode != "rebuild" && !nzchar(Sys.getenv("COMTRADE_PRIMARY", unset = ""))) {
    rlang::abort("Set COMTRADE_PRIMARY before authenticated requests", class = "comtrade_credentials_error")
  }
  for (directory in c("objects", "cache", "runs")) {
    dir.create(file.path(config$raw_directory, directory), recursive = TRUE, showWarnings = FALSE)
  }
  run_directory <- tempfile(pattern = paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ-", tz = "UTC"), mode, "-"),
    tmpdir = file.path(config$raw_directory, "runs"))
  dir.create(run_directory)
  if (!file.copy(config_path, file.path(run_directory, "config.yml"))) {
    rlang::abort("Could not preserve the Comtrade profile", class = "comtrade_output_error")
  }
  success <- FALSE
  on.exit(jsonlite::write_json(list(success = success, mode = mode,
    completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    file.path(run_directory, "result.json"), auto_unbox = TRUE, pretty = TRUE), add = TRUE)

  # The official reporter table identifies regional groups that must not be summed with members.
  reference <- comtrade_fetch("https://comtradeapi.un.org/files/v1/app/reference/Reporters.json", config, mode, run_directory)
  reporters <- reference$body$results
  if (!is.data.frame(reporters) || !all(c("reporterCode", "reporterDesc", "reporterCodeIsoAlpha3", "isGroup") %in% names(reporters)) ||
      anyNA(reporters[c("reporterCode", "isGroup")]) || !is.logical(reporters$isGroup) ||
      anyDuplicated(reporters$reporterCode) ||
      !config$country_code %in% reporters$reporterCode[!reporters$isGroup | reporters$reporterCode %in% comtrade_separate_customs_areas]) {
    rlang::abort("Comtrade reporter reference is invalid or the focal country is a group", class = "comtrade_reference_error")
  }
  queries <- comtrade_queries(config)
  endpoint <- if (config$access == "public_preview") "public/v1/preview" else "data/v1/get"
  observations <- vector("list", nrow(queries))
  summaries <- vector("list", nrow(queries))
  for (index in seq_len(nrow(queries))) {
    query <- queries[index, ]
    request <- httr2::request(paste("https://comtradeapi.un.org", endpoint, "C", config$frequency, config$classification, sep = "/")) %>%
      httr2::req_url_query(period = query$period, partnerCode = query$partner, cmdCode = query$product,
        flowCode = query$flow, partner2Code = "0", customsCode = "C00", motCode = "0",
        maxRecords = config$max_records, includeDesc = "true")
    if (!is.na(query$reporter)) request <- httr2::req_url_query(request, reporterCode = query$reporter)
    message(sprintf("Comtrade %s/%s: %s %s %s %s", index, nrow(queries), query$period, query$product, query$direction, query$basis))
    result <- comtrade_fetch(request$url, config, mode, run_directory)
    records <- comtrade_records(result$body, query, config, reporters)
    summaries[[index]] <- comtrade_summary(records, query, config, result$receipt)
    if (nrow(records) > 0) {
      observations[[index]] <- records %>% dplyr::mutate(focal_country_code = config$country_code,
        direction = query$direction, basis = query$basis, source_url = result$receipt$url,
        retrieved_at = result$receipt$retrieved_at, response_sha256 = result$receipt$sha256)
    }
  }
  summary <- dplyr::bind_rows(summaries)
  observations <- dplyr::bind_rows(observations) %>%
    dplyr::mutate(date = as.Date(paste0(.data$period,
      if (config$frequency == "A") "0101" else "01"), format = "%Y%m%d"))
  if (anyNA(observations$date)) {
    rlang::abort("Invalid Comtrade observation period", class = "comtrade_observation_error")
  }
  if (nrow(observations) == 0) {
    rlang::abort("No Comtrade observations found; existing outputs were preserved", class = "comtrade_empty_error")
  }
  # Save the run's outputs before promoting them; old run outputs and response objects remain intact.
  readr::write_csv(summary, file.path(run_directory, "summary.csv"), na = "")
  readr::write_csv(observations, file.path(run_directory, "observations.csv"), na = "")
  dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
  destinations <- c(file.path("data/final", paste0(config$dataset_id, "_", config$frequency, "_observations.csv")),
    file.path("data/final", paste0(config$dataset_id, "_", config$frequency, ".csv")))
  if (!all(file.copy(file.path(run_directory, c("observations.csv", "summary.csv")), destinations, overwrite = TRUE))) {
    rlang::abort(paste("Could not publish Comtrade outputs; preserved run:", run_directory), class = "comtrade_output_error")
  }
  success <- TRUE
  message(sprintf("Saved %s series rows and %s source observations; run: %s", nrow(summary), nrow(observations), run_directory))
  invisible(list(summary = summary, observations = observations, run_directory = run_directory))
}
