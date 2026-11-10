# Reconcile the workbook's nominal collection history with reviewed SENIAT evidence.
# Preserve original posts, workbook cells, published aggregates and unresolved differences.
library(tidyverse)
library(readxl)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()
dataset_id <- "fiscal_08_seniat_consolidated"
workbook <- "data/raw/Economia_A_C.xlsx"
sheet <- "13 - Recaudaci\u00f3n"
review_dir <- "data/raw/seniat_revenue/consolidation_2026-10-23"
snapshot <- jsonlite::read_json(file.path(review_dir, "snapshot.json"), simplifyVector = TRUE)

# Require the reviewed workbook vintage and verify the supplementary evidence archive.
if (!file.exists(workbook) || !identical(unname(tools::md5sum(workbook)), snapshot$workbook_md5)) {
  rlang::abort("The reviewed Economia workbook vintage is missing or changed", class = "seniat_workbook_source_error")
}
manifest <- read_csv(file.path(review_dir, "manifest.csv"),
  col_types = cols(file = col_character(), md5 = col_character()), show_col_types = FALSE)
if (anyDuplicated(manifest$file) || !all(c("reviewed_evidence.csv", "snapshot.json") %in% manifest$file) ||
    !all(file.exists(file.path(review_dir, manifest$file))) ||
    any(unname(tools::md5sum(file.path(review_dir, manifest$file))) != manifest$md5)) {
  rlang::abort("Supplementary SENIAT evidence failed integrity checks", class = "seniat_review_source_error")
}
baseline <- read_csv("data/final/fiscal_07_seniat_revenue_monthly.csv",
  col_types = cols(date = col_date(), .default = col_guess()), show_col_types = FALSE)
controls <- read_csv("data/interim/fiscal_07_seniat_revenue_observations.csv",
  show_col_types = FALSE) %>% filter(.data$eligible, .data$frequency != "monthly",
    .data$measure == "total_collection", .data$normalized_currency == "VES_2021")
review <- read_csv(file.path(review_dir, "reviewed_evidence.csv"),
  col_types = cols(review_id = col_character(), date = col_date(), measure = col_character(),
    value_bolivares_2021 = col_double(), reported_increment = col_double(), source_rank = col_integer(),
    publication_date = col_date(), source_url = col_character(), evidence_type = col_character(),
    selection_reason = col_character()), show_col_types = FALSE)
readr::stop_for_problems(review)
direct_components <- read_csv("data/interim/fiscal_09_seniat_supplements_direct.csv",
  show_col_types = FALSE)
supplied_cells <- read_csv("data/interim/fiscal_09_seniat_supplements_workbook_cells.csv",
  show_col_types = FALSE) %>% filter(.data$eligible_for_consolidation,
    .data$date <= as.Date("2026-09-01"))
if (anyDuplicated(review[c("date", "measure")]) || anyNA(review) ||
    any(!is.finite(review$value_bolivares_2021) | review$value_bolivares_2021 < 0)) {
  rlang::abort("Supplementary SENIAT observations are invalid", class = "seniat_review_observation_error")
}

# Read only dates and nominal amounts; workbook USD formulas and rolling sums are excluded.
raw <- read_excel(workbook, sheet = sheet, col_names = FALSE,
  col_types = "text", .name_repair = "minimal")
expected_headers <- c("Mes", "Trimestre", "A\u00f1o", "Fecha", "TOTAL SENIAT",
  "ISLR", "IVA", "Renta Aduanera", "Otras Rentas")
if (ncol(raw) != 15L || !identical(str_squish(as.character(unlist(raw[1, 1:9], use.names = FALSE))),
    expected_headers)) {
  rlang::abort("Recaudacion sheet schema changed", class = "seniat_workbook_schema_error")
}
amount_map <- tribble(~column_index, ~measure, ~column_letter,
  5L, "total_collection", "E", 6L, "islr", "F", 7L, "iva", "G",
  8L, "customs", "H", 9L, "other_taxes", "I")
dates <- tibble(excel_row = seq_len(nrow(raw))[-1],
  excel_date_text = raw[[4]][-1], date_serial = parse_double(raw[[4]][-1]),
  recorded_year = parse_integer(raw[[3]][-1]), recorded_month = parse_integer(raw[[1]][-1])) %>%
  mutate(date_original = as.Date(.data$date_serial, origin = "1899-12-30"),
    date = floor_date(.data$date_original, "month"))
if (anyNA(dates) || anyDuplicated(dates$date) ||
    any(year(dates$date) != dates$recorded_year | month(dates$date) != dates$recorded_month) ||
    any(dates$date_original != ceiling_date(dates$date, "month") - days(1))) {
  rlang::abort("Workbook collection dates are invalid or duplicated", class = "seniat_workbook_date_error")
}
cells <- amount_map %>% pmap_dfr(function(column_index, measure, column_letter) {
  # Preserve the native text and exact cell address before parsing each nominal series.
  value_text <- raw[[column_index]][-1]
  amount <- parse_double(value_text)
  readr::stop_for_problems(amount)
  dates %>% mutate(measure = .env$measure, raw_value = value_text, value_bolivares_2021 = amount,
    source_cell = paste0(.env$column_letter, .data$excel_row))
}) %>% mutate(source_file = workbook, source_sheet = sheet, source_md5 = snapshot$workbook_md5,
  workbook_amount_unit = "VES_2021", monetary_divisor = 1,
  unit_basis = "inferred_from_nominal_overlap_and_workbook_conversion_formulas")
if (any(!is.finite(cells$value_bolivares_2021[!is.na(cells$value_bolivares_2021)])) ||
    any(cells$value_bolivares_2021 < 0, na.rm = TRUE)) {
  rlang::abort("Workbook nominal amounts are invalid", class = "seniat_workbook_amount_error")
}
# Exclude workbook scaffolding beyond its reviewed window while allowing newer releases.
workbook_cutoff <- as.Date("2026-09-01")
release_cutoff <- max(baseline$date, workbook_cutoff)
workbook_totals <- cells %>% filter(.data$measure == "total_collection", .data$date <= workbook_cutoff) %>%
  select(date, workbook_amount = value_bolivares_2021, source_file, source_sheet, source_cell, source_md5)
supplied_totals <- supplied_cells %>% filter(.data$measure == "total_collection") %>%
  transmute(date, supplied_amount = value_bolivares_2021, supplied_source_file = source_file,
    supplied_source_sheet = source_sheet, supplied_source_cell = source_cell, supplied_source_md5 = source_md5)
# Confirm the supplied nominal scale across both sides of the 2021 monetary change.
unit_anchors <- supplied_totals %>% inner_join(workbook_totals, by = "date") %>%
  filter(.data$date %in% as.Date(c("2019-01-01", "2020-01-01", "2021-11-01", "2026-06-01")))
if (nrow(unit_anchors) != 4L || any(abs(unit_anchors$supplied_amount - unit_anchors$workbook_amount) >
    .Machine$double.eps * pmax(abs(unit_anchors$workbook_amount), 1) * 16)) {
  rlang::abort("Supplied workbook nominal currency scale is not confirmed", class = "seniat_supplement_unit_error")
}
supplied_comparison <- supplied_totals %>% full_join(workbook_totals, by = "date") %>%
  filter(.data$date <= as.Date("2026-09-01")) %>%
  mutate(difference_bolivares_2021 = .data$supplied_amount - .data$workbook_amount)

# Establish the uncorrected overlap first so every original discrepancy remains inspectable.
comparison <- baseline %>% transmute(date, baseline_amount = value * 1e6,
    baseline_increment = normalized_increment, baseline_observation_id = observation_id,
    baseline_source_url = source_url, baseline_status = status) %>%
  full_join(workbook_totals, by = "date") %>% filter(.data$date <= release_cutoff) %>%
  mutate(difference_bolivares_2021 = .data$workbook_amount - .data$baseline_amount,
    numerical_allowance = .Machine$double.eps * pmax(abs(.data$workbook_amount),
      abs(.data$baseline_amount), 1, na.rm = TRUE) * 8,
    comparison_status = case_when(is.na(.data$baseline_amount) & is.na(.data$workbook_amount) ~ "missing_both",
      is.na(.data$baseline_amount) ~ "workbook_only", is.na(.data$workbook_amount) ~ "seniat_only",
      abs(.data$difference_bolivares_2021) <= .data$numerical_allowance ~ "numeric_match",
      abs(.data$difference_bolivares_2021) <= .data$baseline_increment + .data$numerical_allowance ~
        "within_published_increment", TRUE ~ "different")) %>% arrange(.data$date)

# Supplementary reviewed corrections precede the baseline; the workbook only fills missing totals.
monthly_evidence <- baseline %>% filter(!is.na(.data$value)) %>%
  transmute(date, measure = "total_collection", value_bolivares_2021 = value * 1e6,
    reported_increment = normalized_increment, source_rank, publication_date, source_url,
    evidence_type, evidence_id = observation_id, evidence_status = status,
    selection_reason = quality_note, priority = 2L) %>%
  bind_rows(review %>% filter(.data$measure == "total_collection") %>%
    mutate(evidence_id = review_id, evidence_status = "reviewed_release_correction", priority = 1L)) %>%
  arrange(.data$priority) %>% group_by(.data$date) %>% slice_head(n = 1) %>% ungroup()
calendar <- tibble(date = seq(min(cells$date), release_cutoff, by = "month"))
monthly <- calendar %>% left_join(workbook_totals, by = "date") %>%
  left_join(supplied_totals, by = "date") %>%
  left_join(monthly_evidence %>% select(-measure, -review_id), by = "date") %>%
  left_join(comparison %>% select(date, baseline_amount, comparison_status), by = "date") %>%
  left_join(baseline %>% select(date, baseline_source_conflict = source_conflict), by = "date") %>%
  mutate(value_bolivares_2021 = coalesce(.data$value_bolivares_2021, .data$workbook_amount, .data$supplied_amount),
    value = .data$value_bolivares_2021 / 1e6, unit = "million_VES_2021",
    source_id = case_when(!is.na(.data$evidence_id) ~ "seniat",
      !is.na(.data$workbook_amount) ~ "economia_a_c_workbook",
      !is.na(.data$supplied_amount) ~ "user_supplied_seniat_workbook", TRUE ~ NA_character_),
    status = coalesce(.data$evidence_status, if_else(is.na(.data$value_bolivares_2021),
      "not_located", "workbook_history_or_fill")),
    is_estimated = if_else(.data$source_id == "seniat", FALSE, NA),
    estimation_status = case_when(is.na(.data$value) ~ "unavailable",
      .data$source_id == "seniat" ~ "reported_amount", TRUE ~ "not_documented_in_workbook"),
    workbook_fill_after_2019 = .data$source_id %in% c("economia_a_c_workbook", "user_supplied_seniat_workbook") &
      .data$date >= as.Date("2019-01-01"),
    paired_equal_total_unverified = .data$workbook_fill_after_2019 &
      .data$date %in% as.Date(c("2020-03-01", "2020-04-01", "2021-09-01", "2021-10-01")),
    source_conflict = .data$comparison_status == "different" | coalesce(.data$baseline_source_conflict, FALSE),
    conflict_status = case_when(.data$evidence_status == "reviewed_release_correction" ~
      "resolved_by_corroborated_release", .data$source_conflict ~ "alternative_values_retained",
      TRUE ~ "no_conflict_recorded"),
    period_end = ceiling_date(.data$date, "month") - days(1),
    research_vintage = max(baseline$research_vintage))

# Retain workbook components at their reported levels; never force them to match a chosen total.
# Prefer a matching original formula over its pasted numeric copy so estimation provenance survives.
supplied_components <- supplied_cells %>% filter(.data$measure != "total_collection") %>%
  left_join(cells %>% filter(.data$measure != "total_collection") %>%
    select(date, measure, economia_amount = value_bolivares_2021), by = c("date", "measure")) %>%
  mutate(matches_economia_amount = !is.na(.data$economia_amount) &
      abs(.data$value_bolivares_2021 - .data$economia_amount) <=
        .Machine$double.eps * pmax(abs(.data$value_bolivares_2021), abs(.data$economia_amount), 1) * 16,
    priority = if_else(.data$matches_economia_amount & !is.na(.data$source_formula), 2.5, 4))
component_candidates <- cells %>% filter(.data$measure != "total_collection", !is.na(.data$value_bolivares_2021),
    .data$date <= workbook_cutoff) %>%
  transmute(date, measure, value_bolivares_2021, source_id = "economia_a_c_workbook",
    source_file, source_sheet, source_cell, source_md5, source_url = NA_character_,
    evidence_type = "workbook_numeric_cell", evidence_id = paste0("WB-", .data$source_cell),
    reported_increment = NA_real_, classification = "islr_iva_customs_other", priority = 3,
    is_estimated = NA, estimation_status = "numeric_cell_status_not_documented", source_formula = NA_character_) %>%
  bind_rows(read_csv("data/interim/fiscal_07_seniat_revenue_components.csv", show_col_types = FALSE) %>%
    transmute(date = period_start, measure, value_bolivares_2021 = normalized_amount,
      source_id = "seniat", source_url, evidence_type, evidence_id = observation_id,
      reported_increment = normalized_increment, classification = "domestic_customs_igtf", priority = 2,
      is_estimated = FALSE, estimation_status = "reported_component", source_formula = NA_character_),
    review %>% filter(.data$measure != "total_collection") %>% transmute(date, measure,
      value_bolivares_2021, source_id = "seniat", source_url, evidence_type, evidence_id = review_id,
      reported_increment, classification = "islr_iva_customs_other", priority = 1,
      is_estimated = FALSE, estimation_status = "reported_component", source_formula = NA_character_),
    direct_components %>% select(date, measure, value_bolivares_2021, source_id, source_url,
      evidence_type, evidence_id, reported_increment, classification, is_estimated, estimation_status,
      source_formula, total_post_url, source_relation) %>% mutate(priority = 0),
    supplied_components %>% select(date, measure, value_bolivares_2021, source_id, source_url,
      evidence_type, evidence_id, reported_increment, classification, source_file, source_sheet,
      source_cell, source_md5, is_estimated, estimation_status, source_formula,
      formula_anchor_cell, matches_economia_amount, priority))
components <- component_candidates %>%
  arrange(.data$priority) %>% group_by(.data$date, .data$measure) %>% slice_head(n = 1) %>% ungroup() %>%
  mutate(value = .data$value_bolivares_2021 / 1e6, unit = "million_VES_2021")
component_checks <- components %>% group_by(.data$date) %>% summarise(
  components_available = n(), components_classification = first(.data$classification),
  components_sum_bolivares_2021 = sum(.data$value_bolivares_2021),
  components_direct_count = sum(.data$estimation_status == "direct_reported_component"),
  components_estimated_count = sum(.data$is_estimated, na.rm = TRUE),
  components_derived_count = sum(.data$estimation_status == "workbook_derived_residual"),
  components_increment_allowance = sum(.data$reported_increment, na.rm = TRUE), .groups = "drop")
monthly <- monthly %>% left_join(component_checks, by = "date") %>%
  mutate(components_minus_total_bolivares_2021 = .data$components_sum_bolivares_2021 - .data$value_bolivares_2021,
    components_difference_percent = 100 * .data$components_minus_total_bolivares_2021 /
      na_if(.data$value_bolivares_2021, 0),
    components_reconciliation_status = case_when(is.na(.data$components_available) ~ "no_breakdown",
      is.na(.data$value) ~ "missing_total", .data$components_available !=
        if_else(.data$components_classification == "domestic_customs_igtf", 3L, 4L) ~ "partial_breakdown",
      abs(.data$components_minus_total_bolivares_2021) <=
        coalesce(.data$reported_increment, 0) + .data$components_increment_allowance +
        .Machine$double.eps * pmax(abs(.data$value_bolivares_2021),
          abs(.data$components_sum_bolivares_2021), 1) * 16 ~ "within_recorded_precision",
      TRUE ~ "unresolved_component_difference")) %>%
  left_join(components %>% select(date, measure, value) %>%
    pivot_wider(names_from = measure, values_from = value, names_glue = "{measure}_million_ves_2021"), by = "date") %>%
  rename(workbook_source_file = source_file, workbook_source_sheet = source_sheet,
    workbook_source_cell = source_cell, workbook_source_md5 = source_md5) %>%
  relocate(date, period_end, value, unit, value_bolivares_2021, source_id, status,
    estimation_status, is_estimated, source_conflict, conflict_status,
    ends_with("_million_ves_2021"), components_classification, components_available)

# Check the original workbook and consolidated series against every separately reported control vintage.
nominal_values <- monthly %>% select(date, consolidated = value_bolivares_2021, workbook = workbook_amount,
    reported_increment, workbook_fill_after_2019) %>%
  pivot_longer(cols = c("consolidated", "workbook"), names_to = "basis", values_to = "amount")
reconciliation <- controls %>% select(observation_id, period_start, period_end, frequency,
    control_amount = normalized_amount, control_increment = normalized_increment, source_url, source_rank) %>%
  cross_join(nominal_values) %>% filter(.data$date >= .data$period_start, .data$date <= .data$period_end) %>%
  group_by(.data$observation_id, .data$period_start, .data$period_end, .data$frequency,
    .data$control_amount, .data$control_increment, .data$source_url, .data$source_rank, .data$basis) %>%
  summarise(expected_months = n(), observed_months = sum(!is.na(.data$amount)),
    partial_sum_bolivares_2021 = sum(.data$amount, na.rm = TRUE),
    monthly_increment_allowance = if_else(first(.data$basis) == "consolidated",
      sum(.data$reported_increment, na.rm = TRUE), 0),
    workbook_fill_months = sum(.data$workbook_fill_after_2019, na.rm = TRUE), .groups = "drop") %>%
  mutate(difference_bolivares_2021 = if_else(.data$observed_months == .data$expected_months,
      .data$partial_sum_bolivares_2021 - .data$control_amount, NA_real_),
    status = case_when(.data$observed_months != .data$expected_months ~ "incomplete_monthly_coverage",
      abs(.data$difference_bolivares_2021) <= .data$control_increment + .data$monthly_increment_allowance +
        .Machine$double.eps * pmax(abs(.data$control_amount), 1) * 16 ~ "within_recorded_precision",
      TRUE ~ "unresolved_difference"))
coverage <- monthly %>% group_by(year = year(.data$date)) %>% summarise(
  expected_months = n(), observed_months = sum(!is.na(.data$value)),
  seniat_months = sum(.data$source_id == "seniat", na.rm = TRUE),
  workbook_months = sum(.data$source_id %in% c("economia_a_c_workbook", "user_supplied_seniat_workbook")),
  supplied_workbook_months = sum(.data$source_id == "user_supplied_seniat_workbook", na.rm = TRUE),
  months_with_breakdown = sum(!is.na(.data$components_available)),
  months_with_direct_components = sum(.data$components_direct_count > 0, na.rm = TRUE),
  months_with_estimated_components = sum(.data$components_estimated_count > 0, na.rm = TRUE), .groups = "drop")
if (anyDuplicated(monthly$date) || anyDuplicated(components[c("date", "measure")]) ||
    nrow(monthly) != nrow(calendar) || any(monthly$value < 0, na.rm = TRUE) ||
    !identical(unname(tools::md5sum(workbook)), snapshot$workbook_md5)) {
  rlang::abort("Consolidated SENIAT output or raw workbook integrity failed", class = "seniat_consolidation_error")
}

# Save nominal totals, component provenance, gap fills and reconciliation evidence as distinct layers.
write_csv(cells, paste0("data/interim/", dataset_id, "_workbook_cells.csv"), na = "")
write_csv(comparison, paste0("data/interim/", dataset_id, "_comparison.csv"), na = "")
write_csv(supplied_comparison, paste0("data/interim/", dataset_id, "_supplied_totals_comparison.csv"), na = "")
write_csv(component_candidates %>% left_join(components %>%
    select(date, measure, selected_evidence_id = evidence_id,
      selected_amount = value_bolivares_2021), by = c("date", "measure")) %>%
    filter(.data$evidence_id != .data$selected_evidence_id) %>%
    mutate(difference_bolivares_2021 = .data$value_bolivares_2021 - .data$selected_amount),
  paste0("data/interim/", dataset_id, "_component_alternatives.csv"), na = "")
write_csv(reconciliation, paste0("data/interim/", dataset_id, "_reconciliation.csv"), na = "")
write_csv(coverage, paste0("data/interim/", dataset_id, "_coverage.csv"), na = "")
write_csv(monthly %>% filter(!is.na(.data$components_available)) %>%
  select(date, value_bolivares_2021, components_classification, components_available,
    components_sum_bolivares_2021, components_minus_total_bolivares_2021,
    components_difference_percent, components_direct_count, components_estimated_count,
    components_derived_count, components_reconciliation_status),
  paste0("data/interim/", dataset_id, "_component_checks.csv"), na = "")
write_csv(monthly %>% filter(.data$workbook_fill_after_2019),
  paste0("data/interim/", dataset_id, "_gap_fills.csv"), na = "")
write_csv(monthly %>% filter(is.na(.data$value)), paste0("data/interim/", dataset_id, "_gaps.csv"), na = "")
write_csv(components, paste0("data/final/", dataset_id, "_components.csv"), na = "")
write_csv(monthly, paste0("data/final/", dataset_id, "_monthly.csv"), na = "")
print(comparison %>% count(.data$comparison_status))
print(coverage %>% filter(.data$year >= 2019))
print(reconciliation %>% filter(.data$frequency == "annual", .data$basis == "consolidated") %>%
  select(period_end, difference_bolivares_2021, status))
