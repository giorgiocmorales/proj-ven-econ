# Reconcile the workbook's oil-price copies with OPEC without modifying either source.
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for workbook labels", class = "economia_locale_error")
  }
}
source("R/source_helpers.R")
ensure_project_dirs()
workbook_path <- "data/raw/Economia_A_C.xlsx"
opec_path <- "data/final/oil_02_price_opec_latest.csv"
workbook_md5 <- unname(tools::md5sum(workbook_path))
opec_md5 <- unname(tools::md5sum(opec_path))
exact_tolerance <- 1e-8
cent_tolerance <- 0.010001

# Read cached source values only; retain cells and normalize month-end dates to month-start.
columns <- tibble::tribble(
  ~sheet_name, ~variable_id, ~column_index, ~column_letter, ~expected_header,
  "4-producci\u00f3n petrolera", "merey", 10L, "J", "Merey",
  "4-producci\u00f3n petrolera", "north_sea_dated", 8L, "H", "Brent",
  "Precio petroleo", "merey", 3L, "C", "Precio Merey")
tables <- setNames(lapply(unique(columns$sheet_name), function(sheet_name) {
  read_excel(workbook_path, sheet = sheet_name, col_names = FALSE, col_types = "text",
    .name_repair = "minimal")
}), unique(columns$sheet_name))
workbook_prices <- list()
for (i in seq_len(nrow(columns))) {
  setting <- columns[i, ]
  table <- tables[[setting$sheet_name]]
  if (!identical(table[[1]][1], "Fecha") ||
      !identical(table[[setting$column_index]][1], setting$expected_header)) {
    rlang::abort("Workbook oil-price headers changed", class = "economia_price_schema_error")
  }
  workbook_prices[[i]] <- tibble(excel_row = seq_len(nrow(table))[-1],
    date_original = as.Date(as.double(table[[1]][-1]), origin = "1899-12-30"),
    workbook_price = as.double(table[[setting$column_index]][-1])) %>%
    filter(!is.na(workbook_price)) %>%
    mutate(date = floor_date(date_original, "month"), source_sheet = setting$sheet_name,
      variable_id = setting$variable_id, source_cell = paste0(setting$column_letter, excel_row),
      workbook_file = workbook_path, workbook_md5 = workbook_md5)
}
workbook_prices <- bind_rows(workbook_prices)
if (anyNA(workbook_prices$date) || any(!is.finite(workbook_prices$workbook_price)) ||
    anyDuplicated(workbook_prices[c("source_sheet", "variable_id", "date")])) {
  rlang::abort("Invalid or duplicate workbook oil prices", class = "economia_price_data_error")
}

# Keep OPEC's original benchmark identity and evidence beside every comparison.
opec <- read_csv(opec_path, show_col_types = FALSE,
  col_types = cols(footnote_marker = col_character())) %>%
  filter(period_type == "monthly", variable_id %in% c("merey", "north_sea_dated"))
stop_for_problems(opec)
if (anyDuplicated(opec[c("variable_id", "date")]) || any(opec$unit != "USD/b")) {
  rlang::abort("Invalid OPEC price keys or units", class = "economia_price_opec_error")
}
comparison <- workbook_prices %>% left_join(opec %>%
  select(date, variable_id, opec_price = value, opec_source_label = source_label,
    report_month, source_pdf_url, pdf_file, pdf_md5, source_table_page),
  by = c("date", "variable_id"), relationship = "many-to-one") %>%
  mutate(difference_usd_per_barrel = workbook_price - opec_price,
    comparison_status = case_when(is.na(opec_price) ~ "workbook_only_unverified",
      abs(difference_usd_per_barrel) <= exact_tolerance ~ "exact_match",
      abs(difference_usd_per_barrel) <= cent_tolerance ~ "within_one_cent",
      TRUE ~ "differs_over_one_cent"), opec_input_md5 = opec_md5) %>%
  arrange(source_sheet, variable_id, date)

# Separate older OPEC releases from discrepancies not supported by any archived vintage.
vintage_path <- "data/final/oil_02_price_opec.csv"
vintages <- read_csv(vintage_path, show_col_types = FALSE,
  col_types = cols(footnote_marker = col_character())) %>%
  filter(period_type == "monthly", variable_id %in% c("merey", "north_sea_dated"))
stop_for_problems(vintages)
vintage_matches <- workbook_prices %>% select(source_sheet, source_cell, date, variable_id, workbook_price) %>%
  inner_join(vintages %>% select(date, variable_id, report_month, vintage_price = value),
    by = c("date", "variable_id"), relationship = "many-to-many") %>%
  filter(!is.na(vintage_price), abs(workbook_price - vintage_price) <= cent_tolerance) %>%
  group_by(source_sheet, source_cell, date, variable_id) %>%
  summarise(matching_report_months = paste(sort(unique(report_month)), collapse = "; "),
    .groups = "drop")
comparison <- comparison %>% left_join(vintage_matches,
  by = c("source_sheet", "source_cell", "date", "variable_id"), relationship = "one-to-one") %>%
  mutate(matches_archived_opec_vintage = !is.na(matching_report_months),
    vintage_input_md5 = unname(tools::md5sum(vintage_path)))
summary <- comparison %>% group_by(source_sheet, variable_id) %>%
  summarise(first_date = min(date), last_date = max(date), workbook_months = n(),
    overlapping_months = sum(!is.na(opec_price)),
    exact_matches = sum(comparison_status == "exact_match"),
    matches_within_one_cent = sum(comparison_status %in% c("exact_match", "within_one_cent")),
    differences_over_one_cent = sum(comparison_status == "differs_over_one_cent"),
    differences_matching_older_vintage = sum(comparison_status == "differs_over_one_cent" & matches_archived_opec_vintage),
    workbook_only_months = sum(comparison_status == "workbook_only_unverified"),
    largest_absolute_difference = max(abs(difference_usd_per_barrel), na.rm = TRUE), .groups = "drop")

# Audit the workbook's own duplicated Merey series independently of OPEC.
internal <- workbook_prices %>% filter(variable_id == "merey", source_sheet != "Precio petroleo") %>%
  select(date, main_sheet_price = workbook_price, main_sheet_cell = source_cell) %>%
  inner_join(workbook_prices %>% filter(source_sheet == "Precio petroleo") %>%
    select(date, price_sheet_price = workbook_price, price_sheet_cell = source_cell),
    by = "date", relationship = "one-to-one") %>%
  mutate(difference_usd_per_barrel = price_sheet_price - main_sheet_price,
    agrees_within_one_cent = abs(difference_usd_per_barrel) <= cent_tolerance)

# Verify archived OPEC report bytes for both benchmark histories, not just URL labels.
provenance <- opec %>% distinct(pdf_file, pdf_md5) %>%
  mutate(file_exists = file.exists(pdf_file), actual_md5 = unname(tools::md5sum(pdf_file)),
    checksum_matches = !is.na(actual_md5) & actual_md5 == pdf_md5)
if (any(!provenance$file_exists | !provenance$checksum_matches)) {
  rlang::abort("OPEC oil-price archive files are missing or changed", class = "opec_price_provenance_error")
}
if (!identical(workbook_md5, unname(tools::md5sum(workbook_path))) ||
    !identical(opec_md5, unname(tools::md5sum(opec_path)))) {
  rlang::abort("A source changed during oil-price validation", class = "economia_price_input_error")
}
write_csv(comparison, "data/interim/economia_a_c_oil_prices_comparison.csv")
write_csv(summary, "data/interim/economia_a_c_oil_prices_summary.csv")
write_csv(internal, "data/interim/economia_a_c_merey_internal_comparison.csv")
write_csv(provenance, "data/interim/oil_price_opec_provenance_audit.csv")
print(summary, width = Inf)
print(internal %>% filter(!agrees_within_one_cent))
