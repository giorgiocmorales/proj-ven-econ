ensure_project_dirs <- function() {
  dirs <- c(
    "data/raw",
    "data/interim",
    "data/final",
    "data/manual_fix",
    "registry",
    "output",
    "outputs",
    "outputs/data",
    "output/figures",
    "outputs/figures"
  )

  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

# Resolve acquisition intent independently of the dataset transformation.
source_acquisition_mode <- function() {
  fallback <- if (is.function(getOption("repro.download"))) "update" else "resume"
  mode <- getOption("pipeline.mode", Sys.getenv("PIPELINE_MODE", fallback))
  if (length(mode) != 1L || !mode %in% c("rebuild", "resume", "update")) {
    rlang::abort("Acquisition mode must be rebuild, resume or update", class = "source_mode_error")
  }
  mode
}

# Preserve an acquired version before changing a raw convenience path.
archive_source_file <- function(path) {
  if (!file.exists(path) || file.info(path)$size == 0) {
    rlang::abort(paste("Missing or empty source", path), class = "source_cache_error")
  }
  hash <- digest::digest(file = path, algo = "sha256")
  destination <- file.path("data/raw/vintages", substr(hash, 1L, 2L), paste0(hash, ".", tools::file_ext(path)))
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(destination) && !file.copy(path, destination, copy.date = TRUE)) {
    rlang::abort(paste("Cannot preserve source", path), class = "source_archive_error")
  }
  if (digest::digest(file = destination, algo = "sha256") != hash) {
    rlang::abort(paste("Source archive checksum mismatch", destination), class = "source_archive_error")
  }
  invisible(destination)
}

# Retain prior acquisition metadata on rebuilds; generation time is not retrieval time.
source_retrieval_time <- function(path, dataset_id) {
  ledger_path <- "data/raw/acquisitions.csv"
  if (file.exists(ledger_path) && file.exists(path)) {
    ledger <- readr::read_csv(ledger_path, show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()))
    matched <- ledger[ledger$path == path & ledger$sha256 == digest::digest(file = path, algo = "sha256"), ]
    if (nrow(matched)) return(tail(matched$retrieved_at, 1L))
  }
  prior_path <- file.path("data/final", paste0(dataset_id, ".csv"))
  if (source_acquisition_mode() != "update" && file.exists(prior_path)) {
    prior <- readr::read_csv(prior_path, show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()))
    if ("downloaded_at" %in% names(prior)) {
      timestamps <- unique(prior$downloaded_at[!is.na(prior$downloaded_at)])
      if (length(timestamps) == 1L) return(timestamps)
    }
  }
  NA_character_
}

# Reuse preserved API extracts when original response bytes are unavailable historically.
cached_api_source <- function(dataset_id) {
  registry <- readr::read_csv("registry/series_registry.csv", show_col_types = FALSE)
  legacy <- registry$raw_file[registry$dataset_id == dataset_id]
  candidates <- c(file.path("data/raw", paste0(dataset_id, ".csv")),
    legacy[grepl("[.]csv$", legacy)], file.path("data/final", paste0(dataset_id, ".csv")))
  path <- candidates[file.exists(candidates)][1]
  mode <- source_acquisition_mode()
  if (mode == "update") return(NULL)
  # New acquisitions retain complete provider bytes and can be reparsed on rebuild.
  if (length(list.files(file.path("data/raw/api", dataset_id), pattern = "[.](json|xml)$"))) return(NULL)
  if (is.na(path)) {
    if (mode == "rebuild") rlang::abort(paste("Missing cached API extract", dataset_id), class = "source_cache_error")
    return(NULL)
  }
  data <- readr::read_csv(path, show_col_types = FALSE)
  readr::stop_for_problems(data)
  if (!nrow(data) || !all(c("dataset_id", "source_id", "date", "target_variable") %in% names(data))) {
    rlang::abort(paste("Invalid cached API extract", path), class = "source_cache_error")
  }
  if (any(data$dataset_id != dataset_id)) {
    data <- data %>% dplyr::mutate(source_dataset_id = .data$dataset_id, dataset_id = .env$dataset_id)
  }
  list(data = data, errors = tibble::tibble())
}

download_latest_file <- function(url, dest_path, max_attempts = 3, timeout_seconds = 120) {
  # Rebuilds never fetch; resume fetches only missing files; updates check revisions.
  mode <- source_acquisition_mode()
  if (mode != "update" && file.exists(dest_path) && file.info(dest_path)$size > 0) return(dest_path)
  if (mode == "rebuild") {
    rlang::abort(paste("Missing offline source", dest_path), class = "source_cache_error")
  }
  # The reproducibility runner captures/replays exact responses in a private run.
  transport <- getOption("repro.download")
  if (is.function(transport)) return(transport(url, dest_path, max_attempts, timeout_seconds))
  ensure_project_dirs()

  dest_dir <- dirname(dest_path)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  timeout_old <- getOption("timeout")
  on.exit(options(timeout = timeout_old), add = TRUE)
  options(timeout = timeout_seconds)

  download_path <- tempfile(
    pattern = paste0(".", basename(dest_path), "_"),
    tmpdir = dest_dir,
    fileext = ".download"
  )
  backup_path <- tempfile(
    pattern = paste0(".", basename(dest_path), "_"),
    tmpdir = dest_dir,
    fileext = ".backup"
  )
  on.exit(unlink(download_path), add = TRUE)
  on.exit(unlink(backup_path), add = TRUE)

  last_error <- NULL

  for (attempt in seq_len(max_attempts)) {
    ok <- tryCatch(
      {
        download_status <- download.file(
          url = url,
          destfile = download_path,
          mode = "wb",
          quiet = TRUE,
          method = "libcurl"
        )

        if (download_status != 0) {
          stop(sprintf("Download returned status %s", download_status))
        }

        TRUE
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        FALSE
      }
    )

    if (ok && file.exists(download_path) && file.info(download_path)$size > 0) {
      if (file.exists(dest_path)) archive_source_file(dest_path)
      archive_source_file(download_path)
      if (file.exists(dest_path) && !file.rename(dest_path, backup_path)) {
        stop(sprintf("Could not preserve existing file before refresh: %s", dest_path), call. = FALSE)
      }

      if (!file.rename(download_path, dest_path)) {
        if (file.exists(backup_path) && !file.rename(backup_path, dest_path)) {
          stop(sprintf("Could not restore existing file after refresh failure: %s", dest_path), call. = FALSE)
        }

        stop(sprintf("Could not promote refreshed download: %s", dest_path), call. = FALSE)
      }

      if (file.exists(backup_path)) {
        unlink(backup_path)
      }

      # Record actual acquisition only; cache reads never fabricate a new download time.
      receipt <- tibble::tibble(path = dest_path, url = url,
        sha256 = digest::digest(file = dest_path, algo = "sha256"),
        retrieved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      ledger <- "data/raw/acquisitions.csv"
      readr::write_csv(receipt, ledger, append = file.exists(ledger))

      return(dest_path)
    }

    # Preserve transport failures; only successful empty responses get this message.
    if (ok) {
      last_error <- "Downloaded file is empty"
    }
    unlink(download_path)

    if (attempt < max_attempts) {
      Sys.sleep(1.5 * attempt)
    }
  }

  stop(
    sprintf(
      "Download failed after %s attempts: %s. Last error: %s",
      max_attempts,
      basename(dest_path),
      last_error
    ),
    call. = FALSE
  )
}

stack_excel_workbook <- function(file_path) {
  sheets <- bcv_excel_sheets(file_path)

  purrr::map_dfr(sheets, function(sheet_name) {
    df <- read_bcv_sheet(file_path, sheet = sheet_name, col_names = FALSE)
    df <- tibble::as_tibble(df, .name_repair = "minimal")

    if (ncol(df) == 0) {
      return(tibble::tibble(sheet_name = sheet_name, row_id = integer()))
    }

    names(df) <- sprintf("col_%02d", seq_len(ncol(df)))

    df %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
      dplyr::mutate(sheet_name = sheet_name, row_id = dplyr::row_number(), .before = 1)
  })
}

is_zip_workbook <- function(file_path) {
  con <- file(file_path, "rb")
  on.exit(close(con), add = TRUE)
  signature <- readBin(con, what = "raw", n = 2)
  identical(signature, charToRaw("PK"))
}

with_excel_extension <- function(file_path) {
  if (!grepl("\\.xls$", file_path, ignore.case = TRUE) || !is_zip_workbook(file_path)) {
    return(file_path)
  }

  temp_path <- tempfile(fileext = ".xlsx")
  file.copy(file_path, temp_path, overwrite = TRUE)
  temp_path
}

bcv_excel_sheets <- function(file_path) {
  readxl::excel_sheets(with_excel_extension(file_path))
}

read_bcv_sheet <- function(file_path, sheet, col_names = FALSE, ...) {
  readxl::read_excel(
    path = with_excel_extension(file_path),
    sheet = sheet,
    col_names = col_names,
    ...
  )
}

clean_bcv_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  dplyr::na_if(x, "")
}

clean_bcv_numeric <- function(x) {
  x_chr <- clean_bcv_text(x)
  suppressWarnings(as.numeric(x_chr))
}

parse_bcv_year <- function(x) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(x), "\\d{4}")))
}

is_bcv_provisional <- function(x) {
  stringr::str_detect(dplyr::coalesce(as.character(x), ""), "\\*")
}

parse_bcv_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  x_chr <- clean_bcv_text(x)
  x_chr <- stringr::str_remove_all(x_chr, "\\(\\*\\)|\\*")
  x_chr <- stringr::str_replace_all(x_chr, "/+", "/")
  x_chr <- stringr::str_trim(x_chr)

  # Normalize Spanish month names because the runtime locale may not parse them.
  spanish_month_patterns <- c(
    "(?i)\\bene(?:ro)?\\b" = "01",
    "(?i)\\bfeb(?:rero)?\\b" = "02",
    "(?i)\\bmar(?:zo)?\\b" = "03",
    "(?i)\\babr(?:il)?\\b" = "04",
    "(?i)\\bmay(?:o)?\\b" = "05",
    "(?i)\\bjun(?:io)?\\b" = "06",
    "(?i)\\bjul(?:io)?\\b" = "07",
    "(?i)\\bago(?:sto)?\\b" = "08",
    "(?i)\\b(?:sep|set)(?:tiembre)?\\b" = "09",
    "(?i)\\boct(?:ubre)?\\b" = "10",
    "(?i)\\bnov(?:iembre)?\\b" = "11",
    "(?i)\\bdic(?:iembre)?\\b" = "12"
  )
  x_chr <- stringr::str_replace_all(x_chr, spanish_month_patterns)

  serial <- suppressWarnings(as.numeric(x_chr))
  serial_date <- as.Date(serial, origin = "1899-12-30")
  parsed_date <- suppressWarnings(lubridate::dmy(x_chr))

  dplyr::if_else(
    !is.na(parsed_date),
    parsed_date,
    dplyr::if_else(!is.na(serial) & serial > 20000, serial_date, as.Date(NA))
  )
}

parse_bcv_month_date <- function(month_label, year_label) {
  month_key <- stringr::str_to_lower(clean_bcv_text(month_label))
  month_key <- iconv(month_key, from = "", to = "ASCII//TRANSLIT")
  month_map <- c(
    enero = 1L,
    febrero = 2L,
    marzo = 3L,
    abril = 4L,
    mayo = 5L,
    junio = 6L,
    julio = 7L,
    agosto = 8L,
    septiembre = 9L,
    setiembre = 9L,
    octubre = 10L,
    noviembre = 11L,
    diciembre = 12L
  )

  year <- parse_bcv_year(year_label)
  month <- unname(month_map[month_key])
  out <- rep(as.Date(NA), length(year))
  valid <- !is.na(year) & !is.na(month)
  out[valid] <- as.Date(sprintf("%04d-%02d-01", year[valid], month[valid]))
  out
}

parse_bcv_quarter <- function(x) {
  x_chr <- stringr::str_to_upper(clean_bcv_text(x))
  dplyr::case_when(
    stringr::str_detect(x_chr, "\\bIV\\b|4") ~ 4L,
    stringr::str_detect(x_chr, "\\bIII\\b|3") ~ 3L,
    stringr::str_detect(x_chr, "\\bII\\b|2") ~ 2L,
    stringr::str_detect(x_chr, "\\bI\\b|1") ~ 1L,
    TRUE ~ NA_integer_
  )
}

make_bcv_quarter_date <- function(year, quarter) {
  month <- c("01", "04", "07", "10")[quarter]
  as.Date(sprintf("%04d-%s-01", as.integer(year), month))
}

period_to_date <- function(period) {
  period_chr <- as.character(period)
  out <- rep(as.Date(NA), length(period_chr))

  annual <- stringr::str_detect(period_chr, "^\\d{4}$")
  quarterly <- stringr::str_detect(period_chr, "^\\d{4}-Q[1-4]$")
  monthly <- stringr::str_detect(period_chr, "^\\d{4}-\\d{2}$")

  if (any(annual, na.rm = TRUE)) {
    out[annual] <- as.Date(paste0(period_chr[annual], "-01-01"))
  }
  if (any(quarterly, na.rm = TRUE)) {
    out[quarterly] <- make_bcv_quarter_date(
      as.integer(stringr::str_sub(period_chr[quarterly], 1, 4)),
      as.integer(stringr::str_sub(period_chr[quarterly], 7, 7))
    )
  }
  if (any(monthly, na.rm = TRUE)) {
    out[monthly] <- as.Date(paste0(period_chr[monthly], "-01"))
  }

  out
}

extract_matching_sheets_from_workbook <- function(file_path, patterns) {
  workbook <- stack_excel_workbook(file_path)
  text_cols <- grep("^col_", names(workbook), value = TRUE)

  if (length(text_cols) == 0) {
    stop(sprintf("No tabular columns found in workbook: %s", basename(file_path)))
  }

  regex <- stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE)
  workbook_by_sheet <- split(workbook, workbook$sheet_name)

  matched_sheets <- purrr::keep(workbook_by_sheet, function(sheet_df) {
    any(stringr::str_detect(unlist(sheet_df[text_cols], use.names = FALSE), regex), na.rm = TRUE)
  })

  if (length(matched_sheets) == 0) {
    stop(sprintf(
      "No matching OPEC sheet found in %s for patterns: %s",
      basename(file_path),
      paste(patterns, collapse = ", ")
    ))
  }

  dplyr::bind_rows(matched_sheets)
}

write_interim_dataset <- function(df, dataset_id) {
  ensure_project_dirs()
  out_path <- file.path("data", "interim", paste0(dataset_id, ".csv"))
  readr::write_csv(df, out_path)
  invisible(out_path)
}

write_final_dataset <- function(df, dataset_id) {
  ensure_project_dirs()
  out_path <- file.path("data", "final", paste0(dataset_id, ".csv"))
  readr::write_csv(df, out_path)
  invisible(out_path)
}

write_raw_dataset <- function(df, dataset_id) {
  ensure_project_dirs()
  out_path <- file.path("data", "raw", paste0(dataset_id, ".csv"))
  readr::write_csv(df, out_path)
  invisible(out_path)
}

write_api_source_result <- function(result, dataset_id) {
  # Partial or failed queries must not replace a historical source extract.
  if (nrow(result$errors) > 0L || nrow(result$data) == 0L) {
    details <- if ("error_message" %in% names(result$errors)) {
      paste(unique(result$errors$error_message), collapse = "; ")
    } else "No observations returned"
    rlang::abort(paste("Incomplete API result for", dataset_id, details), class = "source_api_incomplete_error")
  }
  write_raw_dataset(result$data, dataset_id)
  write_final_dataset(result$data, dataset_id)

  error_id <- paste0(dataset_id, "_errors")
  error_path <- file.path("data", "raw", paste0(error_id, ".csv"))

  if (nrow(result$errors) > 0) {
    write_raw_dataset(result$errors, error_id)
  } else if (file.exists(error_path)) {
    unlink(error_path)
  }

  invisible(list(
    raw = file.path("data", "raw", paste0(dataset_id, ".csv")),
    final = file.path("data", "final", paste0(dataset_id, ".csv")),
    errors = error_path
  ))
}

run_script_isolated <- function(script_path) {
  sys.source(script_path, envir = new.env(parent = globalenv()))
}

parse_year_env <- function(var_name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(var_name, as.character(default))))

  if (is.na(value)) {
    rlang::abort(paste(var_name, "must be an integer year"), class = "source_configuration_error")
  }

  value
}

fetch_imf_indicator_set <- function(
  dataset_id,
  source_id,
  indicator_map,
  start_year = parse_year_env("START_YEAR", 2000),
  end_year = parse_year_env("END_YEAR", as.integer(format(Sys.Date(), "%Y")) + 5)
) {
  cached <- cached_api_source(dataset_id)
  if (!is.null(cached)) return(cached)
  results <- purrr::pmap(indicator_map, function(...) {
    row <- tibble::as_tibble(list(...))

    country <- row$country[[1]]
    indicator <- row$indicator_code[[1]]
    frequency <- row$frequency[[1]]
    key_extra <- row$key_extra[[1]]
    flow_id <- row$flow_id[[1]]

    key_parts <- c(country, indicator)

    if (!is.na(key_extra) && nzchar(key_extra)) {
      key_parts <- c(key_parts, strsplit(key_extra, ".", fixed = TRUE)[[1]])
    }

    key <- paste(c(key_parts, frequency), collapse = ".")

    tryCatch(
      {
        # Capture SDMX bytes through the installed provider's request builder.
        imf_reader <- getOption("repro.imf")
        captured <- file.path("data/raw/api", dataset_id,
          paste0(digest::digest(c(flow_id, key), algo = "sha256"), ".xml"))
        sdmx_raw <- if (is.function(imf_reader)) {
          imf_reader(flow_id, key, start_year, end_year)
        } else {
          # Reuse the installed-provider request builder already used by response replay.
          adapter <- new.env(parent = globalenv())
          sys.source("R/reproducibility.R", envir = adapter)
          adapter$repro_imf_request(flow_id, key, start_year, end_year,
            function(url, dest_path, max_attempts, timeout_seconds) {
              download_latest_file(url, captured, max_attempts, timeout_seconds)
              if (!file.copy(captured, dest_path, overwrite = TRUE)) {
                rlang::abort("Cannot stage captured IMF response", class = "source_archive_error")
              }
            })
        }

        df <- as.data.frame(sdmx_raw)

        if (nrow(df) == 0) {
          return(list(data = tibble::tibble(), error = tibble::tibble()))
        }

        # The SDMX reader can return a series metadata row for a query with no observations.
        if (!any(c("TIME_PERIOD", "OBS_VALUE") %in% names(df)) &&
            !is.function(imf_reader) && file.exists(captured)) {
          response <- xml2::read_xml(captured)
          if (length(xml2::xml_find_all(response, ".//*[local-name()='Series']")) > 0L &&
              length(xml2::xml_find_all(response, ".//*[local-name()='Obs']")) == 0L) {
            return(list(data = tibble::tibble(), error = tibble::tibble()))
          }
        }

        df <- tibble::as_tibble(df) %>%
          dplyr::mutate(
            dataset_id = dataset_id,
            source_id = source_id,
            source_url = "https://api.imf.org",
            target_variable = row$target_variable[[1]],
            requested_country = country,
            requested_indicator = indicator,
            requested_frequency = frequency,
            requested_key = key,
            date = period_to_date(.data$TIME_PERIOD),
            downloaded_at = if (is.function(imf_reader)) format(Sys.time(), "%Y-%m-%d %H:%M:%S") else
              source_retrieval_time(captured, .env$dataset_id),
            dplyr::across(dplyr::any_of(c("SCALE", "OBS_VALUE")), as.numeric)
          )

        list(data = df, error = tibble::tibble())
      },
      error = function(e) {
        list(
          data = tibble::tibble(),
          error = tibble::tibble(
            dataset_id = dataset_id,
            source_id = source_id,
            flow_id = flow_id,
            country = country,
            indicator_code = indicator,
            frequency = frequency,
            key = key,
            error_message = conditionMessage(e)
          )
        )
      }
    )
  })

  list(
    data = dplyr::bind_rows(purrr::map(results, "data")),
    errors = dplyr::bind_rows(purrr::map(results, "error"))
  )
}

fetch_wb_indicator_set <- function(
  dataset_id,
  source_id,
  indicator_map,
  country = "VEN",
  start_year = parse_year_env("START_YEAR", 2000),
  end_year = parse_year_env("END_YEAR", as.integer(format(Sys.Date(), "%Y"))),
  max_attempts = 3,
  timeout_seconds = 180
) {
  cached <- cached_api_source(dataset_id)
  if (!is.null(cached)) return(cached)
  timeout_old <- getOption("timeout")
  on.exit(options(timeout = timeout_old), add = TRUE)
  options(timeout = timeout_seconds)

  results <- purrr::pmap(indicator_map, function(...) {
    row <- tibble::as_tibble(list(...))
    indicator <- row$indicator_code[[1]]
    source_url <- sprintf(
      "https://api.worldbank.org/v2/country/%s/indicator/%s?format=json&per_page=20000",
      country,
      indicator
    )

    tryCatch(
      {
        raw_json <- NULL
        last_error <- NULL

        for (attempt in seq_len(max_attempts)) {
          raw_json <- tryCatch(
            {
              # The runner retains the exact JSON response before transformation.
              json_reader <- getOption("repro.json")
              if (is.function(json_reader)) json_reader(source_url) else {
                captured <- file.path("data/raw/api", dataset_id,
                  paste0(digest::digest(source_url, algo = "sha256", serialize = FALSE), ".json"))
                download_latest_file(source_url, captured, max_attempts, timeout_seconds)
                jsonlite::fromJSON(captured, flatten = TRUE)
              }
            },
            error = function(e) {
              last_error <<- e
              NULL
            }
          )

          if (!is.null(raw_json)) {
            break
          }

          if (attempt < max_attempts) {
            Sys.sleep(1.5 * attempt)
          }
        }

        if (is.null(raw_json)) {
          stop(conditionMessage(last_error))
        }

        if (length(raw_json) < 2 || is.null(raw_json[[2]])) {
          return(list(data = tibble::tibble(), error = tibble::tibble()))
        }

        df_raw <- tibble::as_tibble(raw_json[[2]])

        if (!all(c("date", "value") %in% names(df_raw))) {
          stop("World Bank endpoint returned no tabular date/value payload.")
        }

        df <- df_raw %>%
          dplyr::mutate(
            dataset_id = dataset_id,
            source_id = source_id,
            source_url = source_url,
            target_variable = row$target_variable[[1]],
            requested_country = country,
            requested_indicator = indicator,
            year = as.integer(.data$date),
            date = as.Date(sprintf("%s-01-01", year)),
            value = as.numeric(value),
            downloaded_at = if (is.function(getOption("repro.json"))) format(Sys.time(), "%Y-%m-%d %H:%M:%S") else
              source_retrieval_time(captured, .env$dataset_id)
          ) %>%
          dplyr::filter(year >= start_year, year <= end_year)

        list(data = df, error = tibble::tibble())
      },
      error = function(e) {
        list(
          data = tibble::tibble(),
          error = tibble::tibble(
            dataset_id = dataset_id,
            source_id = source_id,
            country = country,
            indicator_code = indicator,
            error_message = conditionMessage(e)
          )
        )
      }
    )
  })

  list(
    data = dplyr::bind_rows(purrr::map(results, "data")),
    errors = dplyr::bind_rows(purrr::map(results, "error"))
  )
}

download_opec_momr_appendix <- function(
  page_url = "https://www.opec.org/monthly-oil-market-report.html",
  raw_name = "opec_momr_latest.xlsx"
) {
  ensure_project_dirs()

  page <- rvest::read_html(page_url)
  links <- rvest::html_elements(page, "a")

  candidates <- tibble::tibble(
    href = rvest::html_attr(links, "href"),
    text = rvest::html_text2(links)
  ) %>%
    dplyr::filter(!is.na(href), href != "") %>%
    dplyr::mutate(
      href_lower = stringr::str_to_lower(href),
      text_lower = stringr::str_to_lower(text),
      is_xlsx = stringr::str_detect(href_lower, "\\.xlsx?$"),
      is_appendix = stringr::str_detect(href_lower, "appendix") |
        stringr::str_detect(text_lower, "appendix")
    ) %>%
    dplyr::filter(is_xlsx | is_appendix) %>%
    dplyr::arrange(dplyr::desc(is_appendix), dplyr::desc(is_xlsx))

  if (nrow(candidates) == 0) {
    stop("Could not find an OPEC MOMR appendix download link.")
  }

  appendix_url <- xml2::url_absolute(candidates$href[[1]], page_url)

  download_latest_file(
    url = appendix_url,
    dest_path = file.path("data", "raw", raw_name)
  )
}
