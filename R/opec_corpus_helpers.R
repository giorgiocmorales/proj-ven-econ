# Extract monthly observations from MOMR text tables using printed month headers.
# Preserve report vintages and raw row evidence; reject ambiguous column alignment.

opec_month_pattern <- "(?i)\\b(?:Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:t(?:ember)?)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\.?[ ]{0,2}(?:[0-9]{4}|[0-9]{2})?\\*?"
opec_value_pattern <- "(?<![A-Za-z0-9-])(?:[-+\u2212\u2013]?[0-9][0-9,]*(?:\\.[0-9]+)?[a-z*%]*|\\.\\.|n\\.a\\.|n/a|na|NA|\u2026|[-\u2013])(?=\\s|$)"

# Parse month names independently of the host locale; missing years refer to the report.
opec_header_date <- function(label, observation_month) {
  month_id <- match(tolower(substr(label, 1, 3)), tolower(month.abb))
  year_text <- stringr::str_extract(label, "[0-9]{2,4}")
  year_id <- ifelse(is.na(year_text), lubridate::year(observation_month), as.integer(year_text))
  year_id <- ifelse(year_id < 100, 2000L + year_id, year_id)
  year_id <- ifelse(is.na(year_text) & month_id > lubridate::month(observation_month), year_id - 1L, year_id)
  as.Date(sprintf("%04d-%02d-01", year_id, month_id))
}

# Select a bounded table, then match printed monthly headers to numeric columns.
extract_opec_monthly_table <- function(pages, report_month, kind) {
  if (!kind %in% c("secondary", "direct", "price")) {
    rlang::abort("Unknown OPEC table kind", class = "opec_table_error")
  }
  # Some PDFs put each row label one line above its numbers; merge without moving columns.
  pages <- purrr::map_chr(pages, function(page_text) {
    if (!stringr::str_detect(page_text, "based on|spot quotations|Reference Basket and")) return(page_text)
    page_lines <- stringr::str_split(page_text, "\\n")[[1]]
    if (length(page_lines) > 1L) for (i in seq_len(length(page_lines) - 1L)) {
      label <- stringr::str_trim(page_lines[[i]])
      next_line <- page_lines[[i + 1L]]
      if (stringr::str_detect(label, "^[A-Za-z][A-Za-z /()'-]{1,32}$") &&
          stringr::str_detect(next_line, "^\\s+[-0-9]") &&
          !stringr::str_detect(next_line, "[A-Za-z]")) {
        left <- stringr::str_locate(page_lines[[i]], "[A-Za-z]")[1, 1]
        prefix <- paste0(strrep(" ", left - 1L), label)
        page_lines[[i + 1L]] <- paste0(prefix, substr(next_line, nchar(prefix) + 1L, nchar(next_line)))
        page_lines[[i]] <- ""
      }
    }
    paste(page_lines, collapse = "\n")
  })
  lines <- tibble(page = rep(seq_along(pages), lengths(stringr::str_split(pages, "\n"))),
                  text = unlist(stringr::str_split(pages, "\n"))) %>%
    mutate(line_id = row_number())
  title_pattern <- switch(kind,
    secondary = "(?:crude oil )?production,? based on secondary sources",
    direct = "(?:crude oil )?production,? based on direct communication",
    price = "(?:monthly average spot quotations|OPEC Reference Basket and selected crudes|OPEC Reference Basket and other crudes|Table[^\\n]*Crude spot prices)"
  )
  titles <- lines %>% filter(str_detect(.data$text, regex(title_pattern, ignore_case = TRUE)))
  if (kind != "price") titles <- titles %>% filter(str_detect(.data$text, "^\\s*(Table|OPEC|DoC)"),
                                                  !str_detect(.data$text, "is shown|shown in"))
  if (nrow(titles) == 0L) {
    rlang::abort(paste("Table not present:", kind), class = "opec_table_absent")
  }
  # A real table has an Algeria row or a numeric basket row close to its heading.
  first_row_pattern <- if (kind == "price") "^\\s*(?:Reference Basket|OPEC Basket|OPEC Reference Basket|OPEC Reference|Basket|ORB|North Sea Dated|Brent)\\*?\\s+[-0-9]" else "^\\s*Algeria\\s+[0-9]"
  candidates <- titles %>% mutate(first_row = map_int(.data$line_id, function(id) {
    hits <- lines %>% filter(.data$line_id > id, .data$line_id <= id + 22L,
                             str_detect(.data$text, first_row_pattern))
    if (nrow(hits) == 0L) return(NA_integer_)
    hits$line_id[[1]]
  })) %>% filter(!is.na(.data$first_row)) %>% distinct(.data$first_row, .keep_all = TRUE)
  if (nrow(candidates) != 1L) {
    rlang::abort(sprintf("Expected one %s table; found %s", kind, nrow(candidates)), class = "opec_table_error")
  }
  first_id <- candidates$first_row[[1]]
  table_page <- lines$page[first_id]
  header_lines <- lines %>% filter(.data$line_id >= candidates$line_id[[1]], .data$line_id < first_id)
  # Refuse an unrecognized unit rather than silently applying today's scale to a new layout.
  unit_pattern <- if (kind == "price") "\\$/b(?:arrel)?" else "(?:tb/d|1[, ]?000 b/d)"
  printed_unit <- any(str_detect(header_lines$text, regex(unit_pattern, ignore_case = TRUE)))
  # These three tables omit a unit caption; their levels match adjacent 1,000 b/d tables
  # and the accompanying OPEC total expressed in millions of barrels per day.
  reviewed_unit <- kind == "secondary" && report_month %in% as.Date(c("2005-07-01", "2005-08-01", "2005-09-01"))
  if (!printed_unit && !reviewed_unit) {
    rlang::abort(paste("Unrecognized printed unit:", kind), class = "opec_unit_error")
  }
  first_text <- lines$text[first_id]
  first_tokens <- str_locate_all(first_text, opec_value_pattern)[[1]]
  if (nrow(first_tokens) < 3L) rlang::abort("Too few numeric table columns", class = "opec_table_error")
  # Missing-value glyphs can be shifted within a cell; use several rows to anchor columns.
  column_ends <- lines %>% filter(.data$line_id >= first_id, .data$line_id < first_id + 10L) %>%
    pull(.data$text) %>% map(function(text) str_locate_all(text, opec_value_pattern)[[1]]) %>%
    keep(function(positions) nrow(positions) == nrow(first_tokens)) %>%
    map(function(positions) positions[, 2]) %>% do.call(rbind, .) %>% apply(2, median)

  # Header positions are inferred from the table, not hard-coded PDF coordinates.
  headers <- map_dfr(seq_len(nrow(header_lines)), function(i) {
    locations <- str_locate_all(header_lines$text[[i]], opec_month_pattern)[[1]]
    if (nrow(locations) == 0L) return(tibble())
    tibble(label = str_sub(header_lines$text[[i]], locations[, 1], locations[, 2]),
           start = locations[, 1], end = locations[, 2], header_line = header_lines$line_id[[i]])
  })
  if (nrow(headers) == 0L) rlang::abort("No printed monthly headers", class = "opec_table_error")
  headers <- headers %>% mutate(
    column = map_int(.data$end, function(x) which.min(abs(column_ends - x))),
    distance = abs(.data$end - column_ends[.data$column]),
    date = opec_header_date(.data$label, report_month %m-% months(1))
  ) %>%
    filter(.data$date < report_month, .data$date >= report_month %m-% months(4)) %>%
    arrange(.data$column, desc(.data$header_line), .data$distance) %>%
    distinct(.data$column, .keep_all = TRUE)
  # The last numeric column is a change or annual/YTD statistic, never a monthly level.
  headers <- headers %>% filter(.data$column < nrow(first_tokens)) %>%
    arrange(.data$column) %>% distinct(.data$date, .keep_all = TRUE)
  if (nrow(headers) < 1L || !max(headers$date) == report_month %m-% months(1)) {
    rlang::abort(paste("Monthly columns do not include the preceding observation month:",
                       paste(headers$label, headers$column, collapse = "; ")), class = "opec_table_error")
  }

  table_lines <- lines %>% filter(.data$line_id >= first_id, .data$page == table_page)
  end_hits <- which(str_detect(table_lines$text, "^\\s*(?:Notes?:|Sources?:|Sources |Totals may|Differentials|Table [0-9]|Graph [0-9])"))
  if (length(end_hits) > 0L) table_lines <- table_lines %>% slice_head(n = end_hits[[1]] - 1L)
  rows <- map_dfr(seq_len(nrow(table_lines)), function(i) {
    row_text <- table_lines$text[[i]]
    positions <- str_locate_all(row_text, opec_value_pattern)[[1]]
    if (nrow(positions) == 0L) return(tibble())
    label <- str_squish(str_sub(row_text, 1L, positions[1, 1] - 1L))
    if (str_detect(label, paste0("^(?:", paste(month.name, collapse = "|"), ")$"))) return(tibble())
    if (!str_detect(label, "^[A-Za-z]") || str_detect(label, "^(?:Note|Source|Other|OPEC Monthly|Monthly Oil|Crude Oil)")) return(tibble())
    if (nrow(positions) != nrow(first_tokens)) {
      rlang::abort(paste("Column count mismatch:", label, row_text), class = "opec_alignment_error")
    }
    tokens <- str_sub(row_text, positions[, 1], positions[, 2])
    selected <- tokens[headers$column]
    numeric_text <- str_remove_all(str_replace_all(selected, "[\u2212\u2013]", "-"), "[, a-z*%]")
    missing <- str_to_lower(selected) %in% c("..", "n.a.", "n/a", "na", "\u2026", "-", "\u2013")
    values <- as.double(replace(numeric_text, missing, NA_character_))
    if (any(!missing & !is.finite(values))) rlang::abort("Invalid numeric token", class = "opec_value_error")
    tibble(date = headers$date, label = label, value = values,
           value_status = if_else(missing, "not_reported", "reported"),
           footnote_marker = if_else(missing, NA_character_, str_extract(selected, "[a-z*]+$")),
           source_row_line = table_lines$line_id[[i]],
           source_table_page = table_page, source_row = row_text,
           source_table_title = str_squish(candidates$text[[1]]),
           unit_basis = if_else(printed_unit, "printed_header", "reviewed_2005_caption_omission"),
           source_column = headers$label, source_type = kind,
           unit = if_else(kind == "price", "USD/b", "tb/d"))
  })
  if (nrow(rows) == 0L || anyDuplicated(rows[c("label", "date")])) {
    rlang::abort("Empty or duplicated table observations", class = "opec_table_error")
  }
  if (kind != "price") {
    if (!"Venezuela" %in% rows$label || (kind == "secondary" && !"Total OPEC" %in% rows$label)) {
      rlang::abort("Missing Venezuela or secondary-source OPEC total", class = "opec_table_error")
    }
    if (any(rows$value < 0, na.rm = TRUE)) rlang::abort("Negative production", class = "opec_value_error")
  }
  # Asterisks can denote scope changes, not provisional figures (e.g. Saudi supply).
  # Preserve the notes after the last data row, stopping before another table or graph.
  notes <- lines %>% filter(.data$page == table_page, .data$line_id > max(rows$source_row_line))
  next_section <- which(str_detect(notes$text, "^\\s*(?:Table [0-9]|Graph [0-9]|OPEC Monthly Oil Market Report)"))
  if (length(next_section) > 0L) notes <- notes %>% slice_head(n = next_section[[1]] - 1L)
  rows %>% mutate(source_table_notes = str_trim(paste(notes$text, collapse = "\n")))
}
