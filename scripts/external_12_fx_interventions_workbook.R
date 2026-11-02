# Complement the BCV baseline with the supplied workbook through September 2026.
# Preserve cached amounts and formulas; audit their referenced bank detail independently.
# May uses dated event sheets. June-September retain the workbook's daily summary.
library(tidyverse)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()

# Verify the immutable source and the reviewed cutoff before reading any workbook cells.
raw_dir <- "data/raw/fx_interventions_workbook"
metadata_path <- file.path(raw_dir, "snapshot.json")
if (!file.exists(metadata_path)) {
  rlang::abort("Intervention workbook metadata is missing", class = "fx_workbook_source_error")
}
snapshot <- jsonlite::fromJSON(metadata_path)
workbook <- file.path(raw_dir, snapshot$source_file)
if (!file.exists(workbook) || !identical(unname(tools::md5sum(workbook)), snapshot$md5)) {
  rlang::abort("Intervention workbook differs from the archived source", class = "fx_workbook_source_error")
}
cutoff <- as.Date(snapshot$cutoff)
sheets <- readxl::excel_sheets(workbook)
if (!identical(sheets[2], snapshot$summary_sheet)) {
  rlang::abort("Intervention summary worksheet mapping changed", class = "fx_workbook_schema_error")
}

# Read the explicit daily calendar, preserving numeric zeros and the October source row.
summary <- readxl::read_excel(workbook, sheet = snapshot$summary_sheet,
  range = snapshot$summary_range, col_types = c("date", "numeric")) %>%
  transmute(date = as.Date(.data$fecha), value_usd = .data$intervencion_usd,
    source_row = row_number() + 1L, source_cell = paste0("C", .data$source_row))
if (anyNA(summary) || any(!is.finite(summary$value_usd)) || any(summary$value_usd < 0) ||
    nrow(summary) != 123L || any(summary$date != seq.Date(as.Date(snapshot$summary_first_date),
      as.Date(snapshot$summary_last_date), by = "day"))) {
  rlang::abort("Intervention summary has invalid dates, amounts or calendar coverage", class = "fx_workbook_observation_error")
}

# Inspect original OOXML formulas without modifying or recalculating the source workbook.
xml_connection <- unz(workbook, snapshot$summary_xml, open = "rb")
summary_xml <- xml2::read_xml(xml_connection)
close(xml_connection)
namespace <- c(d = "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
formula_nodes <- xml2::xml_find_all(summary_xml, "//d:c[d:f]", ns = namespace)
formulas <- tibble(source_cell = xml2::xml_attr(formula_nodes, "r"),
  source_formula = xml2::xml_text(xml2::xml_find_first(formula_nodes, "d:f", ns = namespace))) %>%
  filter(str_detect(.data$source_cell, "^C[0-9]+$"))
summary <- summary %>% left_join(formulas, by = "source_cell")
supported_sum <- "^SUM\\('[^']+'![A-Z]+[0-9]+:[A-Z]+[0-9]+(?:,'[^']+'![A-Z]+[0-9]+:[A-Z]+[0-9]+)*\\)$"
if (any(!str_detect(formulas$source_formula, supported_sum)) ||
    any(is.na(summary$source_formula) & summary$value_usd != 0)) {
  rlang::abort("Intervention summary contains an unsupported amount formula", class = "fx_workbook_formula_error")
}

# Recompute each SUM argument from its own stored bank cells, including multi-range formulas.
references <- formulas %>% mutate(reference = str_extract_all(.data$source_formula,
  "'[^']+'![A-Z]+[0-9]+:[A-Z]+[0-9]+")) %>% unnest_longer(reference) %>%
  mutate(detail_sheet = str_match(.data$reference, "^'([^']+)'!")[, 2],
    detail_range = str_extract(.data$reference, "[A-Z]+[0-9]+:[A-Z]+[0-9]+$"))
reference_audit <- purrr::pmap_dfr(references, function(source_cell, source_formula,
                                                       reference, detail_sheet, detail_range) {
  if (!detail_sheet %in% sheets) {
    rlang::abort(paste("Referenced intervention sheet is missing:", detail_sheet), class = "fx_workbook_formula_error")
  }
  raw_values <- readxl::read_excel(workbook, sheet = detail_sheet, range = detail_range,
    col_names = FALSE, col_types = "text", .name_repair = "unique_quiet") %>% unlist(use.names = FALSE)
  values <- readr::parse_double(raw_values)
  readr::stop_for_problems(values)
  if (any(is.na(values) & !is.na(raw_values)) || any(!is.na(values) & (!is.finite(values) | values < 0))) {
    rlang::abort(paste("Invalid bank amount in:", reference), class = "fx_workbook_observation_error")
  }
  tibble(source_cell, source_formula, reference, detail_sheet, detail_range,
    recomputed_usd = sum(values, na.rm = TRUE), blank_cells_ignored_by_sum = sum(is.na(values)))
})
daily_audit <- reference_audit %>% group_by(.data$source_cell) %>%
  summarise(recomputed_usd = sum(.data$recomputed_usd),
    detail_sheets = paste(unique(.data$detail_sheet), collapse = ";"),
    detail_ranges = paste(.data$detail_range, collapse = ";"), .groups = "drop")
summary <- summary %>% left_join(daily_audit, by = "source_cell") %>%
  mutate(recomputed_usd = if_else(is.na(.data$source_formula), .data$value_usd, .data$recomputed_usd),
    difference_usd = .data$value_usd - .data$recomputed_usd,
    within_requested_cutoff = .data$date <= cutoff)
readr::write_csv(reference_audit, "data/interim/external_12_fx_workbook_formula_references.csv")
readr::write_csv(summary, "data/interim/external_12_fx_workbook_daily_audit.csv")
if (anyNA(summary$recomputed_usd) || any(abs(summary$difference_usd) > 0.01)) {
  rlang::abort("Intervention summary does not reconcile with its referenced cells; see daily audit", class = "fx_workbook_reconciliation_error")
}

# Extract May's separate sale directions; never add sales back to the BCV to client sales.
may_sheets <- sheets[str_detect(sheets, "^[0-9]{2}052026$")]
if (length(may_sheets) != 13L) {
  rlang::abort("Unexpected May intervention-sheet coverage", class = "fx_workbook_coverage_error")
}
may_components <- purrr::map_dfr(may_sheets, function(sheet) {
  cells <- readxl::read_excel(workbook, sheet = sheet, range = "A1:J30",
    col_names = FALSE, col_types = "text", .name_repair = "unique_quiet")
  if (!identical(cells[[1]][29], "Total USD")) {
    rlang::abort(paste("May intervention sheet lacks a USD total:", sheet), class = "fx_workbook_unit_error")
  }
  header_date <- str_extract(cells[[1]][1], "[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
    as.Date(format = "%d/%m/%Y")
  components <- cells %>% slice(4:28) %>% select(4:6) %>%
    set_names(c("D", "E", "F")) %>%
    pivot_longer(everything(), names_to = "column", values_to = "raw_value") %>%
    mutate(value_usd = readr::parse_double(.data$raw_value))
  readr::stop_for_problems(components$value_usd)
  if (any(is.na(components$value_usd) & !is.na(components$raw_value)) ||
      any(!is.na(components$value_usd) & (!is.finite(components$value_usd) | components$value_usd < 0))) {
    rlang::abort(paste("Invalid May detail amounts:", sheet), class = "fx_workbook_observation_error")
  }
  components %>% group_by(.data$column) %>%
    summarise(blank_cells_ignored_by_sum = sum(is.na(.data$value_usd)),
      value_usd = sum(.data$value_usd, na.rm = TRUE), .groups = "drop") %>%
    mutate(source_sheet = sheet, date = as.Date(sheet, format = "%d%m%Y"), header_date,
      header_date_conflict = .data$date != .data$header_date,
      component = unlist(cells[2, 4:6], use.names = FALSE),
      direction = unlist(cells[3, 4:6], use.names = FALSE),
      source_range = paste0(.data$column, "4:", .data$column, "28"),
      published_column_total = as.numeric(unlist(cells[29, 4:6], use.names = FALSE)),
      difference_usd = .data$value_usd - .data$published_column_total)
})
readr::write_csv(may_components, "data/interim/external_12_fx_workbook_may_components.csv")
if (anyNA(may_components) || any(abs(may_components$difference_usd) > 0.01) ||
    any(!may_components$direction %in% c("Venta a Clientes", "Venta a BCV"))) {
  rlang::abort("May intervention component directions, dates or totals require review", class = "fx_workbook_reconciliation_error")
}
may <- may_components %>% filter(.data$direction == "Venta a Clientes") %>%
  group_by(.data$date, .data$source_sheet, .data$header_date_conflict) %>%
  summarise(value_usd = sum(.data$value_usd), source_range = paste(.data$source_range, collapse = ";"),
    .groups = "drop") %>% mutate(source_cell = NA_character_, source_formula = NA_character_,
      coverage = "recorded_event_sheets_only", method = "sum_columns_labelled_sales_to_clients")

# Produce the requested daily series with explicit source zeros and original date conflicts.
daily <- bind_rows(may, summary %>% filter(.data$within_requested_cutoff) %>%
  transmute(date, value_usd, source_sheet = snapshot$summary_sheet, source_cell, source_formula,
    source_range = .data$detail_ranges, header_date_conflict = FALSE,
    coverage = "explicit_daily_calendar", method = "supplied_daily_summary_verified_against_detail")) %>%
  mutate(value = .data$value_usd / 1e6, unit = "million_USD", source_id = snapshot$source_id,
    source_file = workbook, source_md5 = snapshot$md5, cutoff,
    measure = "workbook_intervention_and_client_fx_sales", provenance = snapshot$provenance) %>%
  arrange(.data$date)
if (nrow(daily) != 135L || anyDuplicated(daily$date) > 0L || any(daily$date > cutoff)) {
  rlang::abort("Unexpected requested daily intervention coverage", class = "fx_workbook_coverage_error")
}
monthly <- daily %>% mutate(month = floor_date(.data$date, "month")) %>%
  group_by(.data$month, .data$source_id, .data$coverage, .data$method, .data$measure) %>%
  summarise(recorded_days = n(), source_zero_days = sum(.data$value_usd == 0),
    header_date_conflicts = sum(.data$header_date_conflict), value_usd = sum(.data$value_usd),
    .groups = "drop") %>%
  transmute(date = .data$month, period_end = ceiling_date(.data$month, "month") - days(1),
    value = .data$value_usd / 1e6, value_usd, unit = "million_USD", source_id, coverage, method,
    measure, recorded_days, source_zero_days, header_date_conflicts, source_file = workbook,
    source_md5 = snapshot$md5,
    source_note = if_else(.data$coverage == "recorded_event_sheets_only",
      paste(snapshot$scope, "May is a subtotal of 13 dated event sheets; full-month completeness is unverified. A worksheet/header date conflict is retained."),
      snapshot$scope))

# Match all four monthly controls printed in the workbook's summary sheet.
controls <- readxl::read_excel(workbook, sheet = snapshot$summary_sheet, range = "D2:E5",
  col_names = c("month", "published_total_usd"), col_types = c("numeric", "numeric")) %>%
  mutate(date = make_date(2026L, .data$month, 1L)) %>%
  left_join(monthly %>% select(date, computed_total_usd = value_usd), by = "date") %>%
  mutate(difference_usd = .data$computed_total_usd - .data$published_total_usd)
readr::write_csv(controls, "data/interim/external_12_fx_workbook_monthly_controls.csv")
if (anyNA(controls) || any(abs(controls$difference_usd) > 0.01)) {
  rlang::abort("Daily intervention sums differ from workbook monthly controls", class = "fx_workbook_reconciliation_error")
}
readr::write_csv(daily, "data/final/external_12_fx_interventions_daily.csv")
readr::write_csv(monthly, "data/final/external_12_fx_interventions_monthly.csv")

print(monthly %>% select(date, value, recorded_days, coverage, header_date_conflicts), n = Inf)
if (any(daily$header_date_conflict)) {
  rlang::warn("May 7's worksheet is labelled May 6 inside the source. The worksheet date is retained and the conflict is flagged.",
    class = "fx_workbook_header_date_conflict")
}
