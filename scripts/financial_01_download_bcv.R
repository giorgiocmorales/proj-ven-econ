# Download BCV financial-sector workbooks listed in the source registry.

library(tidyverse)

rm(list = ls())

source("R/source_helpers.R")

source_registry <- readr::read_csv(
  "registry/financial_bcv_sources.csv",
  show_col_types = FALSE
)

required_columns <- c("source_url", "raw_file")
missing_columns <- setdiff(required_columns, names(source_registry))

if (length(missing_columns) > 0) {
  stop(
    sprintf("Financial source registry is missing columns: %s", paste(missing_columns, collapse = ", ")),
    call. = FALSE
  )
}

purrr::pwalk(
  source_registry %>% dplyr::select(.data$source_url, .data$raw_file),
  function(source_url, raw_file) {
    download_latest_file(
      url = source_url,
      dest_path = raw_file,
      max_attempts = 3,
      timeout_seconds = 120
    )
  }
)

# Record workbook dimensions and declared unit profiles for future parser audits.
workbook_inventory <- purrr::pmap_dfr(source_registry, function(...) {
  source_row <- tibble::as_tibble(list(...))
  raw_file <- source_row$raw_file[[1]]

  purrr::map_dfr(bcv_excel_sheets(raw_file), function(sheet_name) {
    sheet <- read_bcv_sheet(
      file_path = raw_file,
      sheet = sheet_name,
      col_names = FALSE,
      .name_repair = "minimal"
    )

    tibble::tibble(
      dataset_family = source_row$dataset_family[[1]],
      parser_profile = source_row$parser_profile[[1]],
      frequency = source_row$frequency[[1]],
      raw_file = raw_file,
      source_url = source_row$source_url[[1]],
      source_vintage = source_row$vintage_label[[1]],
      unit_profile = source_row$unit_profile[[1]],
      sheet_name = sheet_name,
      row_count = nrow(sheet),
      column_count = ncol(sheet),
      file_size_bytes = file.info(raw_file)$size
    )
  })
})

if (nrow(workbook_inventory) == 0) {
  stop("BCV financial workbook inventory returned no sheets.", call. = FALSE)
}

write_interim_dataset(workbook_inventory, "financial_00_workbook_inventory_bcv")
