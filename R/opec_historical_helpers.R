# Helpers for extracting all-country OPEC MOMR production tables.

opec_pdf_text_cache <- new.env(parent = emptyenv())

normalise_opec_country <- function(country) {
  dplyr::recode(
    stringr::str_squish(country),
    "IR Iran" = "Iran",
    "UAE" = "United Arab Emirates",
    "SP Libyan AJ" = "Libya",
    .default = stringr::str_squish(country)
  )
}

read_opec_pdf_text_pages <- function(pdf_path, pdftotext_path) {
  if (!file.exists(pdf_path)) {
    stop(sprintf("OPEC PDF does not exist: %s", pdf_path), call. = FALSE)
  }
  if (!nzchar(pdftotext_path)) {
    stop("The `pdftotext` executable is required for OPEC page discovery.", call. = FALSE)
  }

  text_path <- tempfile(fileext = ".txt")
  on.exit(unlink(text_path), add = TRUE)
  status <- system2(
    command = pdftotext_path,
    args = c("-layout", shQuote(normalizePath(pdf_path, winslash = "/")), shQuote(text_path)),
    stdout = TRUE,
    stderr = TRUE
  )

  if (!file.exists(text_path)) {
    stop(
      sprintf("OPEC text extraction failed: %s", paste(status, collapse = " ")),
      call. = FALSE
    )
  }

  readr::read_file(text_path) %>%
    stringr::str_split("\\f", simplify = FALSE) %>%
    purrr::pluck(1L)
}

read_opec_pdf_page_data <- function(pdf_path, page_number) {
  split_dir <- tempfile(pattern = "opec_pdf_pages_")
  dir.create(split_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(split_dir, recursive = TRUE, force = TRUE), add = TRUE)

  split_pages <- qpdf::pdf_split(input = pdf_path, output = split_dir)

  if (page_number < 1L || page_number > length(split_pages)) {
    stop(sprintf("OPEC PDF page %s is outside the document range.", page_number), call. = FALSE)
  }

  pdftools::pdf_data(split_pages[[page_number]])[[1]]
}

find_opec_production_page_in_pdf <- function(pdf_path, source_type, pdftotext_path) {
  pdf_key <- normalizePath(pdf_path, winslash = "/")

  if (exists(pdf_key, envir = opec_pdf_text_cache, inherits = FALSE)) {
    pdf_text_pages <- get(pdf_key, envir = opec_pdf_text_cache, inherits = FALSE)
  } else {
    split_dir <- tempfile(pattern = "opec_pdf_text_pages_")
    dir.create(split_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(split_dir, recursive = TRUE, force = TRUE), add = TRUE)

    split_pages <- qpdf::pdf_split(input = pdf_path, output = split_dir)
    pdf_text_pages <- purrr::map_chr(split_pages, function(page_path) {
      system2(
        command = pdftotext_path,
        args = c("-layout", shQuote(normalizePath(page_path, winslash = "/")), "-"),
        stdout = TRUE,
        stderr = TRUE
      ) %>%
        stringr::str_c(collapse = "\n")
    })

    assign(pdf_key, pdf_text_pages, envir = opec_pdf_text_cache)
  }

  find_opec_production_page(pdf_text_pages, source_type)
}

find_opec_production_page <- function(pdf_text_pages, source_type) {
  if (!source_type %in% c("secondary", "direct")) {
    stop("`source_type` must be either 'secondary' or 'direct'.", call. = FALSE)
  }

  source_pattern <- dplyr::if_else(
    source_type == "secondary",
    "crude oil production,? based on secondary sources|production,? as reported by selected secondary sources",
    "crude oil production,? based on direct communication"
  )
  page_hits <- which(stringr::str_detect(
    pdf_text_pages,
    stringr::regex(source_pattern, ignore_case = TRUE)
  ))

  table_hits <- page_hits[stringr::str_detect(
    pdf_text_pages[page_hits],
    stringr::regex(
      sprintf("Table[^\\r\\n]*crude oil production[^\\r\\n]*%s", ifelse(source_type == "secondary", "secondary sources", "direct communication")),
      ignore_case = TRUE
    )
  )]

  if (length(table_hits) == 1L) {
    return(table_hits[[1]])
  }

  if (length(page_hits) != 1L) {
    stop(
      sprintf(
        "Expected one %s production table page; found %s.",
        source_type,
        length(page_hits)
      ),
      call. = FALSE
    )
  }

  page_hits[[1]]
}

reconstruct_opec_pdf_rows <- function(pdf_page_data, y_tolerance) {
  if (y_tolerance <= 0) {
    stop("`y_tolerance` must be positive.", call. = FALSE)
  }

  y_groups <- pdf_page_data %>%
    dplyr::distinct(.data$y) %>%
    dplyr::arrange(.data$y) %>%
    dplyr::mutate(
      row_id = cumsum(c(TRUE, diff(.data$y) > y_tolerance)[seq_along(.data$y)])
    )

  pdf_page_data %>%
    dplyr::inner_join(y_groups, by = "y") %>%
    dplyr::arrange(.data$row_id, .data$x) %>%
    dplyr::group_by(.data$row_id) %>%
    dplyr::summarise(
      row_y = min(.data$y),
      row_text = stringr::str_c(.data$text, collapse = " "),
      words = list(dplyr::pick(dplyr::everything())),
      .groups = "drop"
    )
}

extract_opec_production <- function(pdf_path, report_date, source_type, y_tolerance, country_max_x, monthly_min_x, monthly_max_x) {
  if (!file.exists(pdf_path)) {
    stop(sprintf("OPEC PDF does not exist: %s", pdf_path), call. = FALSE)
  }
  if (!source_type %in% c("secondary", "direct")) {
    stop("`source_type` must be either 'secondary' or 'direct'.", call. = FALSE)
  }

  pdf_page <- find_opec_production_page_in_pdf(pdf_path, source_type, Sys.which("pdftotext"))
  pdf_page_data <- read_opec_pdf_page_data(pdf_path, pdf_page)
  table_rows <- reconstruct_opec_pdf_rows(pdf_page_data, y_tolerance)
  source_title_pattern <- dplyr::if_else(source_type == "secondary", "secondary sources", "direct communication")
  title_row <- which(
    stringr::str_detect(table_rows$row_text, "^Table") &
      stringr::str_detect(table_rows$row_text, stringr::regex("production", ignore_case = TRUE)) &
      stringr::str_detect(table_rows$row_text, stringr::regex(source_title_pattern, ignore_case = TRUE))
  )

  if (length(title_row) != 1L) {
    stop("Could not identify the OPEC production table title row.", call. = FALSE)
  }

  table_rows <- table_rows %>%
    dplyr::slice((title_row[[1]] + 1L):dplyr::n()) %>%
    dplyr::mutate(
      country = purrr::map_chr(.data$words, function(words) {
        words %>%
          dplyr::filter(.data$x < country_max_x) %>%
          dplyr::pull(.data$text) %>%
          stringr::str_c(collapse = " ") %>%
          normalise_opec_country()
      }),
      monthly_values = purrr::map(.data$words, function(words) {
        words %>%
          dplyr::filter(.data$x >= monthly_min_x, .data$x < monthly_max_x) %>%
          dplyr::pull(.data$text) %>%
          parse_opec_number()
      })
    ) %>%
    dplyr::filter(
      stringr::str_detect(.data$country, "^[A-Za-z]"),
      !stringr::str_detect(.data$country, "^(Note|Source|Secondary sources|Direct communication)$")
    )

  total_opec_row <- which(table_rows$country == "Total OPEC")
  if (length(total_opec_row) != 1L) {
    stop("Could not identify exactly one 'Total OPEC' row.", call. = FALSE)
  }

  table_rows <- table_rows %>%
    dplyr::slice(seq_len(total_opec_row[[1]])) %>%
    dplyr::filter(purrr::map_int(.data$monthly_values, length) == 3L)

  if (nrow(table_rows) == 0L) {
    stop("No OPEC production rows contained three monthly values.", call. = FALSE)
  }

  observation_dates <- seq(
    as.Date(report_date),
    length.out = 3L,
    by = "-1 month"
  ) %>%
    rev()

  table_rows %>%
    dplyr::select(country, monthly_values) %>%
    tidyr::unnest_longer(monthly_values, indices_to = "month_position") %>%
    dplyr::mutate(
      date = observation_dates[.data$month_position],
      production_tbd = .data$monthly_values,
      source_type = source_type,
      report_date = as.Date(report_date),
      source_table_page = pdf_page,
      pdf_file = pdf_path
    ) %>%
    dplyr::select(date, country, production_tbd, source_type, report_date, source_table_page, pdf_file)
}

audit_opec_production <- function(production_data, expected_observations, require_total_opec) {
  if (expected_observations < 1L) {
    stop("`expected_observations` must be at least one.", call. = FALSE)
  }

  required_columns <- c("date", "country", "production_tbd", "source_type", "source_table_page", "pdf_file")
  missing_columns <- setdiff(required_columns, names(production_data))

  if (length(missing_columns) > 0L) {
    stop(
      sprintf("Production data are missing columns: %s", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  if (require_total_opec) {
    total_opec_checks <- production_data %>%
      dplyr::filter(.data$country == "Total OPEC") %>%
      dplyr::count(.data$source_type, .data$source_table_page, .data$pdf_file, name = "n_total_opec")

    if (nrow(total_opec_checks) == 0L || any(total_opec_checks$n_total_opec != expected_observations)) {
      stop(
        sprintf(
          "Each production table must contain %s Total OPEC observation(s).",
          expected_observations
        ),
        call. = FALSE
      )
    }
  }
  if (nrow(production_data) == 0L) {
    stop("Production extraction returned no observations.", call. = FALSE)
  }
  if (any(!is.na(production_data$production_tbd) & production_data$production_tbd < 0)) {
    stop("Production values cannot be negative.", call. = FALSE)
  }

  tibble::tibble(
    audit_name = "opec_production",
    status = "passed",
    observations = nrow(production_data)
  )
}

extract_opec_legacy_production <- function(pdf_path, report_date, source_type, y_tolerance, country_max_x, value_match_tolerance) {
  if (!file.exists(pdf_path)) {
    stop(sprintf("OPEC PDF does not exist: %s", pdf_path), call. = FALSE)
  }
  if (!source_type %in% c("secondary", "direct")) {
    stop("`source_type` must be either 'secondary' or 'direct'.", call. = FALSE)
  }

  pdf_page <- find_opec_production_page_in_pdf(pdf_path, source_type, Sys.which("pdftotext"))
  pdf_page_data <- read_opec_pdf_page_data(pdf_path, pdf_page)
  table_rows <- reconstruct_opec_pdf_rows(pdf_page_data, y_tolerance)
  title_pattern <- dplyr::if_else(
    source_type == "secondary",
    "production,? based on secondary sources",
    "production,? based on direct communication"
  )
  title_row <- which(stringr::str_detect(
    table_rows$row_text,
    stringr::regex(title_pattern, ignore_case = TRUE)
  ))

  if (length(title_row) != 1L) {
    stop(sprintf("Could not identify the legacy OPEC %s-production table title.", source_type), call. = FALSE)
  }

  table_rows <- table_rows %>%
    dplyr::slice((title_row[[1]] + 1L):dplyr::n())
  next_table_row <- which(stringr::str_detect(table_rows$row_text, "^Table"))

  if (length(next_table_row) > 0L) {
    table_rows <- table_rows %>%
      dplyr::slice(seq_len(next_table_row[[1]] - 1L))
  }

  target_month <- stringr::str_to_lower(format(as.Date(report_date), "%b%y"))
  header_words <- table_rows %>%
    dplyr::slice_head(n = 5L) %>%
    dplyr::pull(.data$words) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      token = stringr::str_to_lower(stringr::str_remove_all(.data$text, "[^A-Za-z0-9]")),
      token = stringr::str_replace(.data$token, "^sept", "sep")
    )
  target_header <- header_words %>%
    dplyr::filter(.data$token == target_month) %>%
    dplyr::arrange(dplyr::desc(.data$y)) %>%
    dplyr::slice_head(n = 1L)

  if (nrow(target_header) == 0L) {
    target_month_name <- stringr::str_to_lower(format(as.Date(report_date), "%b"))
    target_year <- format(as.Date(report_date), "%y")
    target_header <- header_words %>%
      dplyr::filter(.data$token == target_month_name) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        has_year_token = any(
          header_words$token == target_year &
            abs(header_words$y - .data$y) <= y_tolerance &
            header_words$x > .data$x &
            header_words$x - .data$x <= value_match_tolerance
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$has_year_token) %>%
      dplyr::arrange(dplyr::desc(.data$y)) %>%
      dplyr::slice_head(n = 1L)
  }

  if (nrow(target_header) == 0L) {
    stop(
      sprintf("Could not identify exactly one legacy current-month header: %s", target_month),
      call. = FALSE
    )
  }

  current_month_x <- target_header$x[[1]]
  header_row_id <- target_header$y[[1]]
  table_rows <- table_rows %>%
    dplyr::filter(.data$row_y > header_row_id) %>%
    dplyr::mutate(
      country = purrr::map_chr(.data$words, function(words) {
        words %>%
          dplyr::filter(.data$x < country_max_x) %>%
          dplyr::pull(.data$text) %>%
          stringr::str_c(collapse = " ") %>%
          normalise_opec_country()
      }),
      production_tbd = purrr::map_dbl(.data$words, function(words) {
        candidates <- words %>%
          dplyr::filter(abs(.data$x - current_month_x) <= value_match_tolerance) %>%
          dplyr::mutate(distance_to_current_month = abs(.data$x - current_month_x)) %>%
          dplyr::arrange(.data$distance_to_current_month) %>%
          dplyr::slice_head(n = 1L) %>%
          dplyr::pull(.data$text)

        if (length(candidates) == 0L) {
          return(NA_real_)
        }

        parse_opec_number(candidates[[1]])
      })
    ) %>%
    dplyr::filter(stringr::str_detect(.data$country, "^[A-Za-z]"))

  total_opec_row <- which(table_rows$country == "Total OPEC")
  if (length(total_opec_row) != 1L) {
    stop("Could not identify exactly one 'Total OPEC' row in the legacy table.", call. = FALSE)
  }

  table_rows %>%
    dplyr::slice(seq_len(total_opec_row[[1]])) %>%
    dplyr::filter(!is.na(.data$production_tbd)) %>%
    dplyr::transmute(
      date = as.Date(report_date),
      country = .data$country,
      production_tbd = .data$production_tbd,
      source_type = source_type,
      report_date = as.Date(report_date),
      source_table_page = pdf_page,
      pdf_file = pdf_path
    )
}
