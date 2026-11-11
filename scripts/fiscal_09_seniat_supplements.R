# Prepare exact-post component transcriptions and the supplied nominal workbook.
# Source formulas identify estimates; no values are recalculated or balanced here.
library(tidyverse)
library(readxl)
library(lubridate)
source("R/source_helpers.R")
ensure_project_dirs()
dataset_id <- "fiscal_09_seniat_supplements"
direct_dir <- "data/raw/seniat_revenue/direct_posts_2026-10-23"
workbook_dir <- "data/raw/seniat_revenue/user_workbook"
snapshot <- jsonlite::fromJSON(file.path(workbook_dir, "snapshot.json"))
workbook <- file.path(workbook_dir, snapshot$source_file)

# Verify source integrity and require precisely the original set of total-post URLs.
manifest <- read_csv(file.path(direct_dir, "manifest.csv"),
  col_types = cols(file = col_character(), md5 = col_character()), show_col_types = FALSE)
if (!file.exists(workbook) || !identical(unname(tools::md5sum(workbook)), snapshot$md5) ||
    anyDuplicated(manifest$file) || !all(file.exists(file.path(direct_dir, manifest$file))) ||
    any(unname(tools::md5sum(file.path(direct_dir, manifest$file))) != manifest$md5)) {
  rlang::abort("SENIAT supplementary source integrity failed", class = "seniat_supplement_source_error")
}
audit <- read_csv(file.path(direct_dir, "post_review.csv"), show_col_types = FALSE)
original <- read_csv("data/raw/seniat_revenue/research_2026-10-23/reviewed_primary_monthly.csv",
  show_col_types = FALSE)
direct <- read_csv(file.path(direct_dir, "reviewed_components.csv"),
  col_types = cols(.default = col_character(), date = col_date(), publication_date = col_date(),
    reported_increment = col_double()), show_col_types = FALSE)
readr::stop_for_problems(direct)
if (nrow(audit) != nrow(original) || anyDuplicated(audit$source_url) ||
    !setequal(audit$source_url, original$source_url) ||
    any(!direct$total_post_url %in% original$source_url) || anyNA(direct) ||
    anyDuplicated(direct[c("date", "measure")])) {
  rlang::abort("Direct component review is not linked to the original totals", class = "seniat_direct_scope_error")
}
direct <- direct %>% mutate(reported_amount = parse_double(.data$reported_value),
    monetary_divisor = case_when(.data$currency_regime == "VES_2018" ~ 1e6,
      .data$currency_regime == "VES_2021" ~ 1, TRUE ~ NA_real_),
    value_bolivares_2021 = .data$reported_amount / .data$monetary_divisor,
    reported_increment = .data$reported_increment / .data$monetary_divisor,
    source_id = "seniat", evidence_type = "visible_original_total_post_or_thread",
    classification = "islr_iva_customs_other", is_estimated = FALSE,
    estimation_status = "direct_reported_component", source_formula = NA_character_)
if (anyNA(direct$value_bolivares_2021) || any(!is.finite(direct$value_bolivares_2021)) ||
    any(direct$value_bolivares_2021 < 0 | direct$reported_increment <= 0)) {
  rlang::abort("Direct component amounts or units are invalid", class = "seniat_direct_amount_error")
}

# Read only the nominal sheet, retaining within-month source dates and exact cell addresses.
if (!identical(excel_sheets(workbook)[3], snapshot$sheet)) {
  rlang::abort("Supplied nominal worksheet mapping changed", class = "seniat_supplement_schema_error")
}
headers <- read_excel(workbook, sheet = snapshot$sheet, range = "A3:O3",
  col_names = FALSE, col_types = "text", .name_repair = "minimal")
if (!identical(str_squish(as.character(unlist(headers[1, 4:9], use.names = FALSE))),
    c("Fecha", "TOTAL SENIAT", "ISLR", "IVA", "Renta Aduanera", "Otras Rentas"))) {
  rlang::abort("Supplied nominal headers changed", class = "seniat_supplement_schema_error")
}
raw <- read_excel(workbook, sheet = snapshot$sheet, range = snapshot$data_range,
  col_names = FALSE, col_types = "text", .name_repair = "minimal")
dates <- tibble(source_row = seq_len(nrow(raw)) + 3L,
  source_date = as.Date(parse_double(raw[[4]]), origin = "1899-12-30"),
  recorded_month = parse_integer(raw[[1]]), recorded_year = parse_integer(raw[[3]])) %>%
  mutate(date = floor_date(.data$source_date, "month"),
    redundant_date_labels_missing = is.na(.data$recorded_month) | is.na(.data$recorded_year))
if (anyNA(dates[c("source_date", "date")]) || anyDuplicated(dates$date) ||
    any(month(dates$date) != dates$recorded_month, na.rm = TRUE) ||
    any(year(dates$date) != dates$recorded_year, na.rm = TRUE)) {
  rlang::abort("Supplied nominal dates are invalid", class = "seniat_supplement_date_error")
}
amount_map <- tibble(column_index = 5:15, column_letter = LETTERS[5:15],
  measure = c("total_collection", "islr", "iva", "customs", "other_taxes", "igtf_subcategory",
    "liquor_subcategory", "cigarette_subcategory", "inheritance_subcategory",
    "gambling_subcategory", "other_subcategory"))
cells <- amount_map %>% pmap_dfr(function(column_index, column_letter, measure) {
  # Preserve the cached numeric value without executing any workbook formula.
  amounts <- parse_double(raw[[column_index]])
  readr::stop_for_problems(amounts)
  dates %>% mutate(measure = .env$measure, raw_value = raw[[column_index]],
    value_bolivares_2021 = amounts, source_cell = paste0(.env$column_letter, .data$source_row),
    is_top_level = .env$column_index <= 9L)
})

# Reuse the repository's OOXML inspection pattern to preserve original formulas.
xml_connection <- unz(workbook, snapshot$worksheet_xml, open = "rb")
worksheet_xml <- xml2::read_xml(xml_connection)
close(xml_connection)
namespace <- c(d = "http://schemas.openxmlformats.org/spreadsheetml/2006/main")
# Preserve the full stored numeric text; text-mode Excel reads can round large fractional amounts.
numeric_nodes <- xml2::xml_find_all(worksheet_xml, "//d:c[d:v][not(@t) or @t='n']", ns = namespace)
numeric_cache <- tibble(source_cell = xml2::xml_attr(numeric_nodes, "r"),
  cached_numeric_text = xml2::xml_text(xml2::xml_find_first(numeric_nodes, "d:v", ns = namespace))) %>%
  mutate(cached_numeric_amount = parse_double(.data$cached_numeric_text))
readr::stop_for_problems(numeric_cache$cached_numeric_amount)
cells <- cells %>% left_join(numeric_cache, by = "source_cell") %>% mutate(
  raw_value = coalesce(.data$cached_numeric_text, .data$raw_value),
  value_bolivares_2021 = coalesce(.data$cached_numeric_amount, .data$value_bolivares_2021))
formula_nodes <- xml2::xml_find_all(worksheet_xml, "//d:c[d:f]", ns = namespace)
formulas <- tibble(source_cell = xml2::xml_attr(formula_nodes, "r"),
  source_formula_raw = xml2::xml_text(xml2::xml_find_first(formula_nodes, "d:f", ns = namespace)),
  shared_formula_id = xml2::xml_attr(xml2::xml_find_first(formula_nodes, "d:f", ns = namespace), "si"))
# Shared formulas store their expression only at an anchor; use that template for classification.
# Keep the anchor address explicit rather than pretending its references belong to the child cell.
anchors <- formulas %>% filter(!is.na(.data$shared_formula_id), nzchar(.data$source_formula_raw)) %>%
  transmute(shared_formula_id, formula_anchor_cell = source_cell, formula_anchor_expression = source_formula_raw)
formulas <- formulas %>% left_join(anchors, by = "shared_formula_id") %>% mutate(
  source_formula = if_else(nzchar(.data$source_formula_raw), .data$source_formula_raw,
    paste0("Shared formula anchored at ", .data$formula_anchor_cell, ": ", .data$formula_anchor_expression)))
cells <- cells %>% left_join(formulas, by = "source_cell") %>% mutate(
  source_cached_amount = .data$value_bolivares_2021,
  unit_status = if_else(.data$is_top_level, "current_bolivar_scale_checked_by_overlap",
    "subcategory_scale_not_verified"),
  value_bolivares_2021 = if_else(.data$is_top_level, .data$source_cached_amount, NA_real_),
  source_id = "user_supplied_seniat_workbook", source_file = workbook, source_sheet = snapshot$sheet,
  source_md5 = snapshot$md5, evidence_id = paste0("USER-", .data$source_cell),
  source_url = NA_character_, reported_increment = NA_real_, classification = "islr_iva_customs_other",
  estimation_status = case_when(is.na(.data$source_formula) ~ "numeric_cell_status_not_documented",
    str_detect(.data$source_formula, "INDEX|\\*") ~ "workbook_historical_share_estimate",
    str_detect(.data$source_formula, "E[0-9]+-.*[FGH]") ~ "workbook_derived_residual",
    TRUE ~ "unsupported_formula"),
  is_estimated = if_else(.data$estimation_status == "workbook_historical_share_estimate", TRUE, NA),
  evidence_type = if_else(is.na(.data$source_formula), "supplied_workbook_numeric_cell",
    "supplied_workbook_formula_cached_value"),
  eligible_for_consolidation = .data$is_top_level & !is.na(.data$value_bolivares_2021),
  subcategory_zero_status = if_else(!.data$is_top_level & .data$source_cached_amount == 0,
    "placeholder_or_reported_zero_not_distinguished", NA_character_))
if (any(cells$estimation_status == "unsupported_formula" & cells$is_top_level) ||
    any(!is.finite(cells$value_bolivares_2021[cells$eligible_for_consolidation])) ||
    any(cells$value_bolivares_2021[cells$eligible_for_consolidation] < 0) ||
    !identical(unname(tools::md5sum(workbook)), snapshot$md5)) {
  rlang::abort("Supplied nominal amounts or formulas require review", class = "seniat_supplement_formula_error")
}

# Retain subcategories as unverified candidates; missing-data zeros never become confirmed receipts.
write_csv(direct, paste0("data/interim/", dataset_id, "_direct.csv"), na = "")
write_csv(cells, paste0("data/interim/", dataset_id, "_workbook_cells.csv"), na = "")
write_csv(cells %>% filter(!.data$is_top_level),
  paste0("data/interim/", dataset_id, "_subcategory_candidates.csv"), na = "")
print(direct %>% count(.data$date))
print(cells %>% filter(.data$is_top_level) %>% count(.data$measure, .data$estimation_status))
