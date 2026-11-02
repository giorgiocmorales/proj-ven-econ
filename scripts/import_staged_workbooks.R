# Lossless workbook ingestion for retained references without a reviewed economic
# schema. This records every populated cell; it does not invent units or series.
library(dplyr)
library(tidyr)
library(purrr)
source("R/source_helpers.R")
source("R/reproducibility.R")

# The controller supplies the source identity explicitly through a process option.
source_id <- getOption("repro.source")
if (is.null(source_id)) rlang::abort("Run through run_reproducibility.R", class = "repro_config_error")
sources <- readr::read_csv("registry/reproducibility_sources.csv", show_col_types = FALSE,
  na = character(), col_types = readr::cols(.default = readr::col_character()))
profile <- sources %>% filter(.data$source_id == .env$source_id)
if (nrow(profile) != 1L) rlang::abort("Unknown workbook profile", class = "repro_config_error")
paths <- list.files("data/raw", full.names = TRUE, recursive = TRUE)
paths <- paths[grepl(profile$input_pattern, paths, perl = TRUE) & grepl("\\.xlsx?$", paths)]
if (!length(paths)) rlang::abort("No staged reference workbooks", class = "repro_input_error")

# Preserve physical row/column addresses and cached values as text. Empty cells
# remain implicit in their original workbook; formulas are not recalculated.
cells <- map_dfr(sort(paths), function(path) {
  sheets <- bcv_excel_sheets(path)
  if (!length(sheets)) rlang::abort(paste("Empty workbook", path), class = "repro_schema_error")
  map_dfr(sheets, function(sheet) {
    values <- read_bcv_sheet(path, sheet, col_names = FALSE, col_types = "text",
      range = readxl::cell_limits(ul = c(1L, 1L), lr = c(NA_integer_, NA_integer_)),
      .name_repair = "unique_quiet")
    # Blank worksheets are valid source structure; record them in the sheet ledger.
    if (ncol(values) == 0L) {
      message("Empty source worksheet retained in sheet inventory: ", path, " :: ", sheet)
      return(tibble::tibble())
    }
    names(values) <- sprintf("column_%05d", seq_len(ncol(values)))
    values %>% mutate(row_number = row_number()) %>%
      pivot_longer(-row_number, names_to = "column", values_to = "raw_cached_value",
        values_drop_na = TRUE) %>% mutate(source_file = path, source_sha256 = repro_hash(path),
        sheet_name = sheet, source_id = source_id, column_number = as.integer(sub("column_", "", column)),
        interpretation_status = "untyped_source_cell_units_unverified") %>% select(-column)
  })
})
if (!nrow(cells) || anyDuplicated(cells[c("source_file", "sheet_name", "row_number", "column_number")])) {
  rlang::abort("No populated cells or duplicate workbook addresses", class = "repro_schema_error")
}
write_interim_dataset(cells, paste0("reference_", source_id, "_cells"))
sheet_inventory <- map_dfr(sort(paths), function(path) {
  tibble::tibble(source_file = path, source_sha256 = repro_hash(path),
    sheet_name = bcv_excel_sheets(path))
}) %>% left_join(cells %>% count(source_file, sheet_name, name = "populated_cells"),
  by = c("source_file", "sheet_name")) %>%
  mutate(populated_cells = coalesce(populated_cells, 0L))
write_interim_dataset(sheet_inventory, paste0("reference_", source_id, "_sheets"))
message("Imported ", nrow(cells), " source cells without economic reinterpretation")
