# API cache and pagination helpers for the one-time cross-country pipeline.
# Refresh failures propagate; cached mode never silently falls back to the network.

## Refresh configuration ----
# Provider overrides inherit the global setting only when they are not supplied.
cross_country_refresh_flag <- function(env_name, fallback) {
  text <- tolower(Sys.getenv(env_name, if (fallback) "true" else "false"))
  if (!text %in% c("true", "false")) {
    rlang::abort(paste(env_name, "must be true or false."), class = "macro_config_error")
  }
  text == "true"
}

## Cached API requests ----
# Reuse the project's download helper, with every argument supplied explicitly.
cache_response <- function(url, filename, raw_dir, refresh) {
  path <- file.path(raw_dir, filename)
  if (refresh) {
    download_latest_file(url, path, max_attempts = 3, timeout_seconds = 45)
  }
  if (!file.exists(path) || file.info(path)$size == 0) {
    rlang::abort(paste("Missing or empty API cache:", path), class = "macro_cache_error")
  }
  path
}

# Follow every World Bank page rather than assuming a large page includes all rows.
read_wb_pages <- function(endpoint, stem, raw_dir, refresh) {
  page <- 1L
  pages <- 1L
  parts <- list()
  while (page <= pages) {
    url <- paste0(endpoint, "&page=", page)
    path <- cache_response(url, paste0(stem, "_page_", page, ".json"), raw_dir, refresh)
    payload <- jsonlite::fromJSON(path, flatten = TRUE)
    if (length(payload) != 2 || !is.data.frame(payload[[2]])) {
      rlang::abort(paste("Invalid World Bank response:", url), class = "macro_api_schema_error")
    }
    metadata <- payload[[1]]
    if (!all(c("pages", "page", "total") %in% names(metadata))) {
      rlang::abort("World Bank pagination fields are missing.", class = "macro_api_schema_error")
    }
    pagination <- cross_country_numeric(unlist(metadata[c("pages", "page", "total")]),
                                        "World Bank pagination")
    if (length(pagination) != 3L || anyNA(pagination) || any(pagination < 1) ||
        any(pagination != floor(pagination)) || pagination[[1]] < page || pagination[[2]] != page) {
      rlang::abort("Invalid World Bank pagination metadata.", class = "macro_api_schema_error")
    }
    if (page > 1L && total != pagination[[3]]) {
      rlang::abort("World Bank total changed during pagination.", class = "macro_api_schema_error")
    }
    pages <- pagination[[1]]
    total <- pagination[[3]]
    parts[[page]] <- as_tibble(payload[[2]]) %>%
      mutate(source_updated_at = as.character(payload[[1]]$lastupdated %||% NA_character_),
             downloaded_at = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE))
    page <- page + 1L
  }
  result <- bind_rows(parts)
  if (nrow(result) != as.integer(payload[[1]]$total)) {
    rlang::abort("World Bank row count does not match pagination total.", class = "macro_api_schema_error")
  }
  result
}

# Data360 returns at most 1,000 records per request. Check the advertised count,
# cache every page, and reject incomplete downloads instead of truncating the data.
read_data360_pages <- function(indicator_id, raw_dir, refresh) {
  endpoint <- sprintf(
    "https://data360api.worldbank.org/data360/data?DATABASE_ID=WB_CCDFS&INDICATOR=%s&timePeriodFrom=1950&timePeriodTo=2025",
    indicator_id
  )
  offset <- 0L
  total <- 1L
  parts <- list()
  while (offset < total) {
    path <- cache_response(paste0(endpoint, "&skip=", offset),
                           paste0(indicator_id, "_offset_", offset, ".json"), raw_dir, refresh)
    payload <- jsonlite::fromJSON(path, flatten = TRUE)
    if (is.null(payload$count) || !is.data.frame(payload$value) || nrow(payload$value) == 0L) {
      rlang::abort(paste("Invalid or empty Data360 page:", path), class = "macro_api_schema_error")
    }
    required_fields <- c("REF_AREA", "INDICATOR", "TIME_PERIOD", "OBS_VALUE", "FREQ",
                         "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS")
    if (!all(required_fields %in% names(payload$value)) ||
        anyNA(payload$value$INDICATOR) || any(payload$value$INDICATOR != indicator_id) ||
        anyNA(payload$value$REF_AREA) || anyNA(payload$value$TIME_PERIOD)) {
      rlang::abort(paste("Unexpected Data360 schema or indicator:", path), class = "macro_api_schema_error")
    }
    page_total <- cross_country_numeric(payload$count, "Data360 count")
    if (length(page_total) != 1L || is.na(page_total) || page_total < 1L || page_total != floor(page_total)) {
      rlang::abort("Invalid Data360 observation count.", class = "macro_api_schema_error")
    }
    if (offset > 0L && total != page_total) {
      rlang::abort("Data360 total changed during pagination.", class = "macro_api_schema_error")
    }
    total <- page_total
    parts[[length(parts) + 1L]] <- as_tibble(payload$value) %>%
      mutate(downloaded_at = format(file.info(path)$mtime, tz = "UTC", usetz = TRUE),
             source_url = endpoint)
    offset <- offset + nrow(payload$value)
  }
  result <- bind_rows(parts)
  if (nrow(result) != total || anyDuplicated(result[c("REF_AREA", "INDICATOR", "TIME_PERIOD")])) {
    rlang::abort("Data360 pages are incomplete or have duplicate observations.", class = "macro_api_schema_error")
  }
  result
}

## Numeric observations ----
# Package parsers report malformed values; promote those failures to explicit errors
# so invalid provider values cannot quietly become missing observations.
cross_country_numeric <- function(values, field) {
  result <- readr::parse_double(as.character(values), na = c("", "NA"))
  if (nrow(readr::problems(result)) > 0L || any(is.nan(result)) ||
      any(!is.finite(result) & !is.na(result))) {
    rlang::abort(paste("Invalid numeric API field:", field), class = "macro_value_error")
  }
  result
}
