# Import the separate BCV Caracas CPI history, retaining immutable workbook vintages.
library(dplyr)
library(tidyr)
library(stringr)
source("R/source_helpers.R")
ensure_project_dirs()

# The Caracas historical workbook is 4_1_7.xls; 4_5_7.xls contains national CPI.
dataset_id <- "prices_07_cpi_caracas_bcv"
source_url <- "https://www.bcv.org.ve/sites/default/files/precios_consumidor/4_1_7.xls"
raw_path <- "data/raw/bcv_cpi_caracas.xls"
sheet_name <- "Base Diciembre 2007"
offline <- identical(Sys.getenv("BCV_CPI_CARACAS_OFFLINE"), "true") ||
  (source_acquisition_mode() != "update" && file.exists(raw_path))
candidate_path <- if (offline) raw_path else tempfile(tmpdir = "data/raw", fileext = ".xls")
if (!offline) {
  download_latest_file(source_url, candidate_path, max_attempts = 3, timeout_seconds = 120)
}

# Validate geography and base headings before interpreting the three source columns.
sheet <- read_bcv_sheet(candidate_path, sheet = sheet_name, col_names = FALSE,
  .name_repair = "unique_quiet")
if (ncol(sheet) != 3L ||
    !str_detect(clean_bcv_text(sheet[[1]][2]), fixed("METROPOLITANA DE CARACAS")) ||
    !identical(clean_bcv_text(sheet[[1]][3]), "Serie desde 1950") ||
    !str_detect(clean_bcv_text(sheet[[1]][4]), fixed("Diciembre 2007 = 100"))) {
  rlang::abort("Expected the Caracas CPI history with December 2007 base",
    class = "caracas_cpi_schema_error")
}

# Parentheses denote negative rates in some years; the initial (-) denotes missing.
# Preserve original cells and reject any other unparseable published numeric value.
cpi <- sheet %>% transmute(row_id = row_number(), label = clean_bcv_text(...1),
  year_marker = if_else(str_detect(coalesce(label, ""), "^\\d{4}"), label, NA_character_),
  cpi_index = clean_bcv_numeric(...2), mom_var_raw = clean_bcv_text(...3)) %>%
  fill(year_marker) %>% mutate(date = parse_bcv_month_date(label, year_marker))
if (any(!is.na(cpi$cpi_index) & is.na(cpi$date))) {
  rlang::abort("A numeric Caracas CPI row has no recognized monthly date",
    class = "caracas_cpi_date_error")
}
cpi <- cpi %>% filter(!is.na(date)) %>%
  mutate(mom_var_text = str_remove_all(mom_var_raw, "\\s+"),
    mom_var_text = na_if(mom_var_text, "(-)"),
    mom_var_text = str_replace(mom_var_text, "^\\((\\d+(?:[.,]\\d+)?)\\)$", "-\\1"),
    mom_var_text = str_replace_all(mom_var_text, fixed(","), "."),
    mom_var_pct = clean_bcv_numeric(mom_var_text),
    provisional = is_bcv_provisional(year_marker)) %>% arrange(date)
if (anyNA(cpi$date) || anyDuplicated(cpi$date) ||
    any(!is.finite(cpi$cpi_index) | cpi$cpi_index <= 0) ||
    any(!is.na(cpi$mom_var_text) & is.na(cpi$mom_var_pct)) ||
    !identical(cpi$date, seq.Date(as.Date("1950-01-01"), max(cpi$date), by = "month")) ||
    !identical(cpi$date[is.na(cpi$mom_var_pct)], as.Date("1950-01-01"))) {
  rlang::abort("Invalid, incomplete, or unparseable Caracas CPI history",
    class = "caracas_cpi_data_error")
}

# Check published rates against adjacent indices before promoting any new files.
rate_check <- cpi %>% mutate(calculated_mom = 100 * (cpi_index / lag(cpi_index) - 1))
if (any(abs(rate_check$calculated_mom - rate_check$mom_var_pct) > 0.2, na.rm = TRUE)) {
  rlang::abort("Caracas published monthly rates disagree with CPI levels by over 0.2 pp",
    class = "caracas_cpi_reconciliation_error")
}

# Archive both prior and new raw bytes; refresh the convenience copy only on success.
vintage_dir <- "data/raw/bcv_cpi_caracas/vintages"
dir.create(vintage_dir, recursive = TRUE, showWarnings = FALSE)
archive_paths <- unique(c(if (file.exists(raw_path)) raw_path, candidate_path))
for (path in archive_paths) {
  hash <- unname(tools::md5sum(path))
  archive_path <- file.path(vintage_dir, paste0(hash, ".xls"))
  if (file.exists(archive_path)) {
    if (!identical(unname(tools::md5sum(archive_path)), hash)) {
      rlang::abort("Archived Caracas CPI checksum mismatch", class = "caracas_cpi_archive_error")
    }
  } else if (!file.copy(path, archive_path, overwrite = FALSE, copy.date = TRUE)) {
    rlang::abort("Could not archive Caracas CPI workbook", class = "caracas_cpi_archive_error")
  }
}
source_md5 <- unname(tools::md5sum(candidate_path))
downloaded_at <- format(file.info(candidate_path)$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
if (!offline && !file.copy(candidate_path, raw_path, overwrite = TRUE, copy.date = TRUE)) {
  rlang::abort("Could not promote validated Caracas workbook", class = "caracas_cpi_write_error")
}
cpi <- cpi %>% mutate(dataset_id = dataset_id, source_id = "bcv", source_url = source_url,
  source_file = file.path(vintage_dir, paste0(source_md5, ".xls")), source_md5 = source_md5,
  sheet_name = sheet_name, downloaded_at = downloaded_at, frequency = "monthly",
  unit = "index_dec_2007_100", geography = "caracas_metropolitan_area",
  purpose = "historical_reference") %>% select(-mom_var_text)
write_final_dataset(cpi, dataset_id)
if (!offline) unlink(candidate_path)
message("Imported ", nrow(cpi), " Caracas CPI months: ", min(cpi$date), " to ", max(cpi$date))
