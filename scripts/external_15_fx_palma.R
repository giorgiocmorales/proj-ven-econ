# Archive reviewed Palma workbooks and extract daily nominal FX quotes from 1990.
# Original workbook bytes and cells remain intact; date anomalies are quarantined,
# not guessed or silently repaired. Monthly-only anchors never become daily quotes.
library(tidyverse)
source("R/source_helpers.R")
ensure_project_dirs()

# Content-addressed cached sources allow offline replay without the Ecoanalitica folder.
dataset_id <- "external_15_fx_palma"
raw_dir <- "data/raw/palma_fx"
dir.create(file.path(raw_dir, "workbooks"), recursive = TRUE, showWarnings = FALSE)
workbooks <- read_csv("registry/palma_fx_workbooks.csv", show_col_types = FALSE) %>%
  mutate(raw_file = file.path(raw_dir, "workbooks", paste0(workbook_id, "_", source_sha256, ".xlsx")))
columns <- read_csv("registry/palma_fx_columns.csv", show_col_types = FALSE)
if (anyDuplicated(workbooks$workbook_id) || anyNA(workbooks) || anyNA(columns) ||
    any(!columns$workbook_id %in% workbooks$workbook_id) ||
    anyDuplicated(columns[c("workbook_id", "sheet", "value_column")])) {
  rlang::abort("Invalid Palma workbook or column registry", class = "palma_fx_registry_error")
}
for (i in seq_len(nrow(workbooks))) {
  record <- workbooks[i, ]
  if (!file.exists(record$raw_file)) {
    if (!file.exists(record$source_path) ||
        digest::digest(file = record$source_path, algo = "sha256") != record$source_sha256) {
      rlang::abort(paste("Missing or revised Palma source:", record$source_path), class = "palma_fx_source_error")
    }
    if (!file.copy(record$source_path, record$raw_file, overwrite = FALSE, copy.date = TRUE)) {
      rlang::abort("Could not archive Palma workbook", class = "palma_fx_archive_error")
    }
  }
  if (digest::digest(file = record$raw_file, algo = "sha256") != record$source_sha256) {
    rlang::abort("Archived Palma workbook checksum mismatch", class = "palma_fx_archive_error")
  }
}

# Read each worksheet once as original text; explicit mappings define series and units.
cells <- columns %>% distinct(workbook_id, sheet) %>%
  pmap_dfr(function(workbook_id, sheet) {
    mapping <- columns %>% filter(.data$workbook_id == .env$workbook_id, .data$sheet == .env$sheet)
    record <- workbooks %>% filter(.data$workbook_id == .env$workbook_id)
    # An explicit A1 origin preserves physical row numbers in sheets with blank top rows.
    source_range <- paste0("A1:", LETTERS[max(c(mapping$date_column, mapping$value_column))], max(mapping$last_row))
    table <- readxl::read_excel(record$raw_file, sheet = sheet, range = source_range, col_names = FALSE,
      col_types = "text", .name_repair = "minimal")
    names(table) <- paste0("col_", seq_len(ncol(table)))
    date_column <- unique(mapping$date_column)
    if (length(date_column) != 1L || ncol(table) < max(mapping$value_column) ||
        nrow(table) < max(mapping$last_row)) {
      rlang::abort(paste("Palma worksheet dimensions changed:", sheet), class = "palma_fx_schema_error")
    }
    table %>% mutate(source_row = row_number(), date_cell_raw = .data[[paste0("col_", date_column)]]) %>%
      select(source_row, date_cell_raw, all_of(paste0("col_", mapping$value_column))) %>%
      pivot_longer(starts_with("col_"), names_to = "column_id", values_to = "value_cell_raw") %>%
      mutate(value_column = as.integer(str_remove(column_id, "col_"))) %>%
      left_join(mapping, by = "value_column", relationship = "many-to-one") %>%
      filter(source_row >= first_row, source_row <= last_row) %>%
      mutate(source_file = record$raw_file, source_path = record$source_path,
        source_sha256 = record$source_sha256, source_cell = paste0(value_letter, source_row),
        source_date_cell = paste0(LETTERS[date_column], source_row)) %>%
      select(-column_id)
  })

# Parse Excel serial/text dates using the existing helper; blank/error/holiday cells
# remain distinct. The raw cell extract is retained even for quarantined observations.
cells <- cells %>% mutate(date = parse_bcv_date(date_cell_raw),
  value_text = na_if(str_squish(value_cell_raw), ""),
  rate_raw = suppressWarnings(as.numeric(value_text)),
  value_status = case_when(is.na(value_text) ~ "blank",
    value_text %in% c("Feriado", "J. Sto.", "V. Sto.", "Carnaval") ~ "holiday",
    value_text == "Inactivo" ~ "inactive",
    str_detect(value_text, "^#") ~ "source_excel_error",
    str_detect(value_text, regex("^en ", ignore_case = TRUE)) ~ "currency_heading",
    value_text %in% c("Sicad I", "Sicad II", "Simadi", "$ Today", "C\u00facuta", "Caracas", "nuevas") ~ "series_heading",
    !is.na(rate_raw) ~ "numeric", TRUE ~ "unrecognized"))
if (any(cells$value_status == "unrecognized")) {
  write_interim_dataset(cells %>% filter(value_status == "unrecognized"), paste0(dataset_id, "_parse_errors"))
  rlang::abort("Unrecognized Palma rate cells; see parse error extract", class = "palma_fx_parse_error")
}

# Examine neighboring dates in worksheet order, independently of each rate column.
# Only an isolated date outside two correctly ordered nearby dates is quarantined.
dates <- cells %>% distinct(workbook_id, sheet, source_row, date) %>%
  arrange(workbook_id, sheet, source_row) %>% group_by(workbook_id, sheet) %>%
  mutate(previous_date = lag(date), next_date = lead(date),
    source_date_anomaly = coalesce(next_date >= previous_date &
      as.integer(next_date - previous_date) <= 10L & (date < previous_date | date > next_date), FALSE)) %>%
  ungroup() %>% select(workbook_id, sheet, source_row, source_date_anomaly)
observations <- cells %>% left_join(dates, by = c("workbook_id", "sheet", "source_row"),
    relationship = "many-to-one") %>% filter(value_status == "numeric" | value_status == "source_excel_error") %>%
  mutate(frequency = if_else(workbook_id == "palma_master" & date < as.Date("1990-11-05"),
      "monthly_anchor", frequency),
    quality_status = case_when(is.na(date) ~ "invalid_date", date < as.Date("1990-01-01") ~ "before_requested_start",
      source_date_anomaly ~ "date_out_of_sequence", value_status == "source_excel_error" ~ "source_excel_error",
      !is.finite(rate_raw) | rate_raw <= 0 ~ "invalid_rate", TRUE ~ "usable"))

# Preserve conflicting same-source quotes; identical repeats can supply one observation.
observations <- observations %>% group_by(workbook_id, sheet, variable_id, date) %>%
  mutate(source_key_rows = n(), source_key_values = n_distinct(rate_raw, na.rm = TRUE),
    quality_status = if_else(quality_status == "usable" & source_key_values > 1L,
      "conflicting_source_key", quality_status)) %>% ungroup()
scales <- read_csv("registry/venezuela_currency_scales.csv", show_col_types = FALSE)
observations <- observations %>% crossing(scales) %>%
  filter((!is.na(date) & date >= effective_start & (is.na(effective_end) | date <= effective_end)) |
    (is.na(date) & currency_code == "VES_current")) %>%
  rename(currency_nominal = currency_code, nominal_conversion_factor = current_ves_per_currency_unit) %>%
  mutate(source_currency = case_when(is.na(date) ~ NA_character_,
      source_currency_rule == "contemporaneous" ~ currency_nominal,
      source_currency_rule == "vef_until_2018" & date < as.Date("2018-08-20") ~ "VEF",
      source_currency_rule == "vef_until_2018" ~ "VES_2018", TRUE ~ source_currency_rule)) %>%
  left_join(scales %>% select(source_currency = currency_code,
      source_conversion_factor = current_ves_per_currency_unit), by = "source_currency", relationship = "many-to-one") %>%
  mutate(rate_adjusted = rate_raw * source_conversion_factor,
    rate_nominal = if_else(source_currency == currency_nominal, rate_raw,
      rate_adjusted / nominal_conversion_factor),
    dataset_id = dataset_id, source_id = "ecoanalitica_palma", currency_adjusted = "VES_current",
    unit_adjusted = paste0("current_VES_per_", quote_currency),
    observation_id = paste(workbook_id, sheet, source_cell, sep = "|")) %>%
  select(dataset_id, source_id, observation_id, date, variable_id, market, quote_currency, frequency,
    rate_raw, source_currency, source_conversion_factor, rate_nominal, currency_nominal,
    nominal_conversion_factor, rate_adjusted, currency_adjusted, unit_adjusted, quality_status,
    source_key_rows, source_key_values, source_date_anomaly, priority, carry_days,
    workbook_id, sheet, source_row, source_cell, source_date_cell, date_cell_raw, value_cell_raw,
    source_path, source_file, source_sha256) %>% arrange(workbook_id, sheet, source_row, variable_id)
if (any(is.na(observations$source_conversion_factor) & observations$quality_status == "usable") ||
    anyDuplicated(observations$observation_id)) {
  rlang::abort("Palma conversion mapping failed", class = "palma_fx_currency_error")
}

# Publish raw cells, provenance and parsed observations only after validation succeeds.
write_csv(workbooks, file.path(raw_dir, "manifest.csv"))
write_interim_dataset(cells, paste0(dataset_id, "_cells"))
write_interim_dataset(observations, dataset_id)
# Publish only usable records; retain excluded cells and their blocking dates for consolidation.
write_final_dataset(observations %>% filter(quality_status == "usable"), dataset_id)
write_interim_dataset(observations %>% filter(quality_status != "usable"), paste0(dataset_id, "_quarantine"))
write_interim_dataset(observations %>% group_by(workbook_id, sheet, variable_id, frequency, quality_status) %>%
  summarise(rows = n(), first_date = min(date), last_date = max(date), .groups = "drop"),
  paste0(dataset_id, "_coverage"))
# The current parse-error extract must not describe a previous failed build after success.
error_path <- file.path("data/interim", paste0(dataset_id, "_parse_errors.csv"))
if (file.exists(error_path) && unlink(error_path) != 0L) {
  rlang::abort("Could not remove resolved Palma parse diagnostic", class = "palma_fx_write_error")
}
message("Archived ", nrow(workbooks), " Palma workbooks; extracted ", nrow(observations), " rate cells")
