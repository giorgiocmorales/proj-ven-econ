opec_report_slug <- function(report_month) {
  format(as.Date(report_month), "%Y-%m")
}

opec_period_label <- function(date) {
  paste0(month.abb[as.integer(format(as.Date(date), "%m"))], format(as.Date(date), "%y"))
}

opec_previous_month_label <- function(date) {
  opec_period_label(seq(as.Date(date), length = 2, by = "-1 month")[[2]])
}

opec_raw_dir <- function(report_month) {
  file.path("data", "raw", "opec", opec_report_slug(report_month))
}

opec_download_report <- function(report, download_appendix = FALSE) {
  report_month <- as.Date(report$report_month[[1]])
  raw_dir <- opec_raw_dir(report_month)
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  appendix_path <- file.path(raw_dir, "appendix.xlsx")
  pdf_path <- file.path(raw_dir, "momr.pdf")

  if (download_appendix && !is.na(report$appendix_url[[1]]) && nzchar(report$appendix_url[[1]])) {
    # The production and price extractors consume the PDF only. Some archive
    # reports do not expose a matching appendix, so do not block a valid PDF
    # extraction when its optional appendix download fails.
    tryCatch(
      download_latest_file(report$appendix_url[[1]], appendix_path),
      error = function(e) {
        message(sprintf("Optional OPEC appendix unavailable: %s", report$appendix_url[[1]]))
        NULL
      }
    )
  }

  if (!is.na(report$pdf_url[[1]]) && nzchar(report$pdf_url[[1]])) {
    download_latest_file(report$pdf_url[[1]], pdf_path)
  }

  list(appendix_path = appendix_path, pdf_path = pdf_path)
}

find_opec_pdf_page <- function(pdf_text, patterns, fallback_page = NA_integer_) {
  regex <- stringr::regex(paste(patterns, collapse = "|"), ignore_case = TRUE, dotall = TRUE)
  hits <- which(stringr::str_detect(pdf_text, regex))

  if (length(hits) > 0) {
    return(hits[[1]])
  }

  if (!is.na(fallback_page)) {
    return(fallback_page)
  }

  stop(sprintf("Could not find PDF page matching patterns: %s", paste(patterns, collapse = ", ")))
}

parse_opec_number <- function(x) {
  x <- stringr::str_replace_all(x, ",", "")
  x <- stringr::str_replace_all(x, "\\*", "")
  x <- dplyr::na_if(x, "..")
  suppressWarnings(as.numeric(x))
}

period_to_opec_date <- function(period) {
  period <- as.character(period)
  out <- rep(as.Date(NA), length(period))

  annual <- stringr::str_detect(period, "^\\d{4}$")
  quarterly <- stringr::str_detect(period, "^\\dQ\\d{2}$")
  monthly <- stringr::str_detect(period, "^[A-Za-z]{3}\\d{2}$")

  if (any(annual, na.rm = TRUE)) {
    out[annual] <- as.Date(paste0(period[annual], "-01-01"))
  }

  if (any(quarterly, na.rm = TRUE)) {
    quarter <- as.integer(stringr::str_sub(period[quarterly], 1, 1))
    year <- paste0("20", stringr::str_sub(period[quarterly], 3, 4))
    month <- c("01", "04", "07", "10")[quarter]
    out[quarterly] <- as.Date(paste0(year, "-", month, "-01"))
  }

  if (any(monthly, na.rm = TRUE)) {
    month <- match(stringr::str_sub(period[monthly], 1, 3), month.abb)
    year <- paste0("20", stringr::str_sub(period[monthly], 4, 5))
    out[monthly] <- as.Date(sprintf("%s-%02d-01", year, month))
  }

  out
}

opec_period_type <- function(period) {
  dplyr::case_when(
    stringr::str_detect(period, "^\\d{4}$") ~ "annual",
    stringr::str_detect(period, "^\\dQ") ~ "quarterly",
    stringr::str_detect(period, "^[A-Za-z]{3}") ~ "monthly",
    stringr::str_detect(period, "ytd$") ~ "year_to_date",
    TRUE ~ "change"
  )
}

extract_opec_venezuela_line <- function(text, source_type, observation_month = NULL) {
  # Select the requested table explicitly; never reuse a secondary row as direct data.
  source_heading <- switch(source_type,
    secondary_sources = "secondary sources",
    direct_communication = "direct communication",
    stop(sprintf("Unknown production source: %s", source_type))
  )
  heading <- stringr::str_locate(text, stringr::regex(
    paste0("Table[^\\n]*crude oil production[^\\n]*", source_heading), ignore_case = TRUE
  ))
  if (is.na(heading[1, 1])) {
    stop(sprintf("Missing production table heading for %s", source_type))
  }
  text <- stringr::str_sub(text, heading[1, 2] + 1L)
  next_table <- stringr::str_locate(text, "Table [0-9]")
  if (!is.na(next_table[1, 1])) {
    text <- stringr::str_sub(text, 1L, next_table[1, 1] - 1L)
  }
  pattern <- "Venezuela\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+([0-9,.]+|\\.\\.)\\s+(-?[0-9,.]+|\\.\\.)"
  matches <- stringr::str_match_all(text, pattern)[[1]]

  if (nrow(matches) != 1) {
    stop(sprintf("Could not find Venezuela production row for %s", source_type))
  }

  values <- matches[ifelse(source_type == "secondary_sources", 1L, nrow(matches)), 2:10]
  annual_periods <- if (is.null(observation_month)) {
    c("2024", "2025")
  } else {
    report_year <- as.integer(format(as.Date(observation_month), "%Y"))
    as.character(c(report_year - 2L, report_year - 1L))
  }

  periods <- c(annual_periods, "q_minus_2", "q_minus_1", "q_current", "m_minus_2", "m_minus_1", "m_current", "mom_change")

  tibble::tibble(period = periods, value = parse_opec_number(values)) |>
    dplyr::mutate(
      source_type = source_type,
      date = period_to_opec_date(period),
      period_type = opec_period_type(period)
    )
}

extract_opec_price_line <- function(text, label, variable_id, observation_month) {
  pattern <- sprintf(
    "%s\\s+(-?[0-9,.]+)\\s+(-?[0-9,.]+)\\s+(-?[0-9,.]+)\\s+(-?[0-9,.]+)\\s+(-?[0-9,.]+)",
    stringr::str_replace_all(label, "([()])", "\\\\\\1")
  )

  match <- stringr::str_match(text, pattern)

  if (all(is.na(match))) {
    stop(sprintf("Could not find OPEC price row: %s", label))
  }

  tibble::tibble(
    variable_id = variable_id,
    label = label,
    period = c(
      opec_previous_month_label(observation_month),
      opec_period_label(observation_month),
      paste0(opec_period_label(observation_month), "/", opec_previous_month_label(observation_month)),
      paste0(as.integer(format(as.Date(observation_month), "%Y")) - 1, "_ytd"),
      paste0(format(as.Date(observation_month), "%Y"), "_ytd")
    ),
    value = parse_opec_number(match[2:6])
  )
}

read_active_opec_reports <- function(path = file.path("registry", "opec_momr_reports.csv")) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      report_month = readr::col_date(),
      observation_month = readr::col_date(),
      appendix_url = readr::col_character(),
      pdf_url = readr::col_character(),
      pdf_source = readr::col_character(),
      page_price = readr::col_integer(),
      page_production = readr::col_integer(),
      status = readr::col_character(),
      notes = readr::col_character()
    )
  ) |>
    dplyr::filter(status == "active")
}
