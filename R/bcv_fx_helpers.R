# Parse the HTML tables served by BCV's official XLS export endpoints.
bcv_fx_table <- function(page, kind) {
  headers <- switch(kind,
    intervention = c("Fecha de Intervenci\u00f3n", "Nro. de Intervenci\u00f3n", "Tipo de Cambio Bs./EUR"),
    banks = c("Fecha del Indicador", "Banco", "Compra", "Venta"),
    rlang::abort("Unknown BCV FX table kind", class = "bcv_fx_config_error"))
  tables <- rvest::html_elements(page, "table") %>% rvest::html_table(fill = TRUE)
  matches <- purrr::keep(tables, ~ identical(stringr::str_squish(names(.x)), headers))
  if (length(matches) != 1L) {
    rlang::abort("Expected one BCV FX table with the source headers", class = "bcv_fx_schema_error")
  }
  data <- matches[[1]]
  names(data) <- if (kind == "intervention") c("date", "intervention_id", "rate") else c("date", "bank", "bid", "ask")
  values <- if (kind == "intervention") "rate" else c("bid", "ask")
  # Preserve missing bank quotes, but reject malformed values and invalid dates.
  raw_values <- data %>% dplyr::select(dplyr::all_of(values)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_squish(as.character(.x))))
  if (any(purrr::map_lgl(raw_values, ~ any(!is.na(.x) & !.x %in% c("", "NA", "-", "N/D") &
      !stringr::str_detect(.x, "^[0-9]+(?:\\.[0-9]{3})*(?:,[0-9]+)?$"))))) {
    rlang::abort("Malformed BCV FX number", class = "bcv_fx_value_error")
  }
  data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_squish(as.character(.x))),
      date = lubridate::dmy(.data$date, quiet = TRUE)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(values), ~ readr::parse_number(.x,
      locale = readr::locale(decimal_mark = ",", grouping_mark = "."), na = c("", "NA", "-", "N/D"))))
  if (anyNA(data$date) || any(purrr::map_lgl(data[values], ~ any(!is.na(.x) & (!is.finite(.x) | .x <= 0)))) ||
      any(purrr::map_lgl(data[values], ~ nrow(readr::problems(.x)) > 0L)) ||
      any(rowSums(!is.na(data[values])) == 0L)) {
    rlang::abort("Missing or invalid BCV FX quotes or dates", class = "bcv_fx_value_error")
  }
  key <- if (kind == "intervention") "intervention_id" else "bank"
  if (any(is.na(data[[key]]) | data[[key]] == "")) {
    rlang::abort("Missing BCV FX identifiers", class = "bcv_fx_key_error")
  }
  # Collapse only identical rows; conflicting quotes remain available for review.
  data %>% dplyr::distinct() %>% dplyr::arrange(.data$date, .data[[key]])
}

# Build explicit Spanish date filters independent of the runtime locale.
bcv_fx_export_request <- function(export_url, start_date, end_date) {
  months <- c("Ene", "Feb", "Mar", "Abr", "Mayo", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  dates <- c(start_date, end_date)
  labels <- paste(format(dates, "%d"), months[lubridate::month(dates)], format(dates, "%Y"))
  httr2::request(export_url) %>%
    httr2::req_url_query(`field_fecha_del_indicador_value[min][date]` = labels[[1]],
      `field_fecha_del_indicador_value[max][date]` = labels[[2]]) %>%
    httr2::req_timeout(seconds = 60) %>% httr2::req_retry(max_tries = 2, retry_on_failure = TRUE)
}

# Archive calendar months and publish only after every response passes validation.
import_bcv_fx <- function(source_url, view_selector, kind, raw_dir, processed_path) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  refresh <- Sys.getenv("BCV_FX_REFRESH_HISTORY", "false")
  if (!refresh %in% c("true", "false")) {
    rlang::abort("BCV_FX_REFRESH_HISTORY must be true or false", class = "bcv_fx_config_error")
  }
  checked_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  listing_path <- file.path(raw_dir, "listing.html")
  download_latest_file(source_url, listing_path, max_attempts = 2L, timeout_seconds = 60L)
  view <- xml2::read_html(listing_path) %>% rvest::html_element(view_selector)
  sample <- bcv_fx_table(view, kind)
  first_date <- view %>% rvest::html_element('input[name="field_fecha_del_indicador_value[min][date]"]') %>%
    rvest::html_attr("value") %>% parse_bcv_date()
  export_url <- view %>% rvest::html_elements('a[href*="/cambiaria/export/"]') %>%
    rvest::html_attr("href") %>% xml2::url_absolute(source_url) %>% unique()
  if (length(export_url) != 1L || !startsWith(export_url, "https://www.bcv.org.ve/cambiaria/export/") ||
      length(first_date) != 1L || is.na(first_date) || nrow(sample) == 0L) {
    rlang::abort("Missing BCV FX history boundary or export link", class = "bcv_fx_schema_error")
  }
  # Bound requests by month to avoid timeouts on the unrestricted bank export.
  last_date <- max(sample$date)
  months <- seq.Date(lubridate::floor_date(first_date, "month"), lubridate::floor_date(last_date, "month"), by = "month")
  paths <- file.path(raw_dir, paste0(format(months, "%Y-%m"), ".xls"))
  requests <- purrr::map(months, ~ bcv_fx_export_request(export_url, .x,
    min(lubridate::ceiling_date(.x, "month") - 1, last_date)))
  receipts_path <- file.path(raw_dir, "download_log.csv")
  previous <- if (file.exists(receipts_path)) readr::read_csv(receipts_path, show_col_types = FALSE) else tibble::tibble()
  hashes <- unname(tools::md5sum(paths))
  reusable <- rep(FALSE, length(paths))
  if (nrow(previous) > 0L) {
    reusable <- purrr::map2_lgl(paths, hashes, ~ any(previous$raw_file == .x & previous$raw_md5 == .y &
      previous$status %in% c("downloaded", "cached"), na.rm = TRUE))
  }
  mode <- source_acquisition_mode()
  if (mode == "rebuild" && any(!reusable)) {
    rlang::abort("Missing or checksum-mismatched BCV monthly cache", class = "bcv_fx_cache_error")
  }
  fetch <- which(!reusable | (mode == "update" & (months == max(months) | refresh == "true")))
  staged <- paste0(paths[fetch], ".download")
  responses <- purrr::map(seq_along(fetch), function(j) {
    i <- fetch[[j]]
    tryCatch(download_latest_file(requests[[i]]$url, staged[[j]],
      max_attempts = 2L, timeout_seconds = 60L), error = identity)
  })
  receipts <- vector("list", length(paths))
  tables <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    j <- match(i, fetch)
    path <- if (is.na(j)) paths[[i]] else staged[[j]]
    # Log all failures and stop the build after auditing every month.
    result <- tryCatch({
      if (!is.na(j) && inherits(responses[[j]], "error")) stop(responses[[j]])
      table <- bcv_fx_table(xml2::read_html(path), kind)
      if (any(format(table$date, "%Y-%m") != format(months[[i]], "%Y-%m"))) {
        rlang::abort("Export returned dates outside its requested month", class = "bcv_fx_range_error")
      }
      if (!is.na(j) && file.exists(paths[[i]])) archive_source_file(paths[[i]])
      if (!is.na(j) && !file.copy(path, paths[[i]], overwrite = TRUE)) {
        rlang::abort(paste("Cannot archive", path), class = "bcv_fx_write_error")
      }
      table
    }, error = identity)
    ok <- !inherits(result, "error")
    receipts[[i]] <- tibble::tibble(month = format(months[[i]], "%Y-%m"), raw_file = paths[[i]],
      export_url = requests[[i]]$url, checked_at = checked_at,
      raw_md5 = if (ok) unname(tools::md5sum(paths[[i]])) else NA_character_,
      rows = if (ok) nrow(result) else NA_integer_,
      status = if (!ok) "error" else if (is.na(j)) "cached" else "downloaded",
      details = if (ok) NA_character_ else conditionMessage(result))
    if (ok) tables[[i]] <- result
  }
  receipts <- dplyr::bind_rows(receipts)
  readr::write_csv(receipts, receipts_path)
  history_path <- file.path(raw_dir, "download_history.csv")
  readr::write_csv(receipts, history_path, append = file.exists(history_path))
  if (any(receipts$status == "error")) {
    rlang::abort(paste("Incomplete BCV FX download; inspect", receipts_path), class = "bcv_fx_download_error")
  }
  data <- dplyr::bind_rows(tables)
  # The intervention export is capped at ten rows per request. Its full listing
  # supplies omitted history; preserve an explicit audit instead of losing observations.
  if (kind == "intervention") {
    if (nrow(dplyr::anti_join(data, sample, by = names(sample))) > 0L) {
      rlang::abort("Intervention export disagrees with its full listing", class = "bcv_fx_coverage_error")
    }
    omitted <- dplyr::anti_join(sample, data, by = names(sample))
    readr::write_csv(omitted, file.path(raw_dir, "export_omissions.csv"))
    data <- dplyr::bind_rows(data, omitted)
  }
  # Require every visible listing observation to agree with the downloaded history.
  if (nrow(dplyr::anti_join(sample, data, by = names(sample))) > 0L || min(data$date) > first_date) {
    rlang::abort("BCV FX exports do not cover or match the listing", class = "bcv_fx_coverage_error")
  }
  key <- if (kind == "intervention") "intervention_id" else "bank"
  data <- data %>% dplyr::add_count(.data$date, .data[[key]], name = "source_key_count") %>%
    dplyr::mutate(source_key_conflict = .data$source_key_count > 1L) %>%
    dplyr::select(-"source_key_count") %>%
    dplyr::mutate(currency = if (kind == "intervention") "EUR" else "USD",
    unit = paste0("published_Bs/", .data$currency), source_id = "bcv", source_url = source_url,
    export_url = export_url,
    downloaded_at = if (mode == "update") checked_at else
      source_retrieval_time(listing_path, tools::file_path_sans_ext(basename(processed_path))),
    retrieval_method = if (kind == "intervention") "listing_verified_with_exports" else "monthly_export") %>%
    dplyr::arrange(.data$date)
  # Keep original quote denominations; do not mix EUR interventions into the USD headline.
  readr::write_csv(data, processed_path)
  unlink(staged)
  message(kind, ": ", nrow(data), " observations; ", min(data$date), " to ", max(data$date))
  invisible(data)
}
