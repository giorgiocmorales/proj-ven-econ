# Profile the user-supplied workbook and reconcile oil exports without changing report series.
# The workbook mixes source observations, estimates, derived values and forecasts.
# These terminal audit outputs identify candidates; they never feed official-series values.
library(tidyverse)
library(readxl)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for workbook labels", class = "economia_locale_error")
  }
}
source("R/source_helpers.R")
ensure_project_dirs()
path <- "data/raw/Economia_A_C.xlsx"
if (!file.exists(path)) {
  rlang::abort("Economia_A_C raw workbook is missing", class = "economia_input_error")
}
input_hash <- unname(tools::md5sum(path))

# Cache each sheet once; preserve duplicate/blank headers using column positions.
sheets <- excel_sheets(path)
tables <- set_names(sheets) %>% map(function(sheet) {
  read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal")
})
inventory <- imap_dfr(tables, function(table, sheet) {
  headers <- as.character(unlist(table[1, ], use.names = FALSE))
  date_column <- which(headers %in% c("Fecha", "DATE"))
  values <- table[-1, ]
  dates <- rep(as.Date(NA), nrow(values))
  if (length(date_column) > 1L) {
    rlang::abort(paste("Ambiguous date columns in", sheet), class = "economia_schema_error")
  }
  if (length(date_column) == 1L) {
    date_text <- values[[date_column]]
    serial_rows <- which(!is.na(date_text) & str_detect(date_text, "^[0-9]{5}$"))
    iso_rows <- which(!is.na(date_text) & str_detect(date_text, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))
    dates[serial_rows] <- as.Date(as.double(date_text[serial_rows]), origin = "1899-12-30")
    dates[iso_rows] <- as.Date(date_text[iso_rows])
  }
  map_dfr(seq_len(ncol(values)), function(column) {
    numeric_rows <- which(!is.na(values[[column]]) &
      str_detect(values[[column]], "^[+-]?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?$"))
    dated_rows <- numeric_rows[!is.na(dates[numeric_rows])]
    tibble(source_file = path, source_md5 = input_hash, sheet = sheet,
      column_index = column, header = headers[column], sheet_rows = nrow(values),
      populated_cells = sum(!is.na(values[[column]])), dated_numeric_cells = length(dated_rows),
      first_date = if (length(dated_rows)) min(dates[dated_rows]) else as.Date(NA),
      last_date = if (length(dated_rows)) max(dates[dated_rows]) else as.Date(NA),
      review_status = if_else(length(date_column) == 1L, "units_and_source_review_required", "non_time_series_layout"))
  })
})

# The explicit mb/d header anchors the unit conversion; both export copies must agree.
oil_sheet <- "4-producci\u00f3n petrolera"
if (!all(c(oil_sheet, "Precio petroleo") %in% sheets)) {
  rlang::abort("Expected oil worksheets are missing", class = "economia_schema_error")
}
oil <- tables[[oil_sheet]]
prices <- tables[["Precio petroleo"]]
if (oil[[6]][1] != "Exportaciones" || prices[[2]][1] != "Exportaciones (mb/d)") {
  rlang::abort("Oil export headers changed; review units and columns", class = "economia_schema_error")
}
exports <- tibble(excel_row = seq_len(nrow(oil))[-1],
  date_original = as.Date(as.double(oil[[1]][-1]), origin = "1899-12-30"),
  raw_value = oil[[6]][-1], raw_unit = "million_barrels_per_day") %>%
  filter(!is.na(.data$raw_value)) %>%
  mutate(date = lubridate::floor_date(.data$date_original, "month"),
    exports_bpd_workbook = as.double(.data$raw_value) * 1e6,
    source_file = path, source_md5 = input_hash, source_sheet = oil_sheet,
    source_cell = paste0("F", .data$excel_row))
price_exports <- tibble(date_original = as.Date(as.double(prices[[1]][-1]), origin = "1899-12-30"),
  duplicate_exports_bpd = as.double(prices[[2]][-1]) * 1e6) %>%
  filter(!is.na(.data$duplicate_exports_bpd)) %>%
  mutate(date = lubridate::floor_date(.data$date_original, "month"))
if (anyNA(exports$date) || any(!is.finite(exports$exports_bpd_workbook)) ||
    any(exports$exports_bpd_workbook <= 0) || anyDuplicated(exports$date) > 0L ||
    anyDuplicated(price_exports$date) > 0L ||
    !setequal(exports$date, price_exports$date) ||
    any(abs(exports$exports_bpd_workbook - price_exports$duplicate_exports_bpd[match(exports$date, price_exports$date)]) > 1e-6)) {
  rlang::abort("Workbook export copies contain invalid or conflicting observations", class = "economia_reconciliation_error")
}

# Normalize month-end Excel dates to the pipeline's month-start key before joining.
reuters <- read_csv("data/final/oil_15_exports_reuters_monthly.csv", show_col_types = FALSE)
comparison <- exports %>% left_join(reuters %>%
  select(date, exports_bpd_reuters = exports_bpd, reuters_status = status, reuters_url = source_url), by = "date") %>%
  mutate(difference_bpd = .data$exports_bpd_workbook - .data$exports_bpd_reuters,
    disposition = case_when(is.na(.data$exports_bpd_reuters) ~ "candidate_requires_source_review",
      abs(.data$difference_bpd) <= 1e-6 ~ "overlap_agrees", TRUE ~ "overlap_differs_keep_reuters"))
gaps <- reuters %>% filter(is.na(.data$exports_bpd)) %>% select(date, month, notes) %>%
  left_join(exports, by = "date") %>%
  mutate(disposition = if_else(is.na(.data$exports_bpd_workbook), "not_in_workbook", "candidate_requires_source_review"))

# Save inspectable evidence and verify the immutable raw workbook was untouched.
write_csv(inventory, "data/interim/economia_a_c_inventory.csv", na = "")
write_csv(comparison, "data/interim/economia_a_c_oil_exports_comparison.csv", na = "")
write_csv(gaps, "data/interim/economia_a_c_oil_gap_candidates.csv", na = "")
if (!identical(input_hash, unname(tools::md5sum(path)))) {
  rlang::abort("Raw workbook changed during the audit", class = "economia_input_error")
}
print(comparison %>% count(.data$disposition))
print(gaps %>% select(month, disposition))
