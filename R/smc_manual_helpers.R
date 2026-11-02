# Prepare the legacy BCV SMC workbooks that readxl cannot parse directly.

smc_manual_fix_registry <- function() {
  tibble::tribble(
    ~raw_path, ~filepath, ~database_id,
    "data/manual_fix/2_1_2d21_smc.xls", "data/manual_fix/2_1_2d21_smc.xlsx", "2021Q4",
    "data/manual_fix/2_1_2c22_smc.xls", "data/manual_fix/2_1_2c22_smc.xlsx", "2022Q3",
    "data/manual_fix/2_1_2d22_smc.xls", "data/manual_fix/2_1_2d22_smc.xlsx", "2022Q4",
    "data/manual_fix/2_1_2a23_smc.xls", "data/manual_fix/2_1_2a23_smc.xlsx", "2023Q1",
    "data/manual_fix/2_1_2b23_smc.xls", "data/manual_fix/2_1_2b23_smc.xlsx", "2023Q2",
    "data/manual_fix/2_1_2c23_smc_60.xls", "data/manual_fix/2_1_2c23_smc_60.xlsx", "2023Q3"
  )
}

convert_smc_manual_workbook <- function(raw_path, filepath, converter_path) {
  if (file.exists(filepath)) {
    converted_sheets <- readxl::excel_sheets(filepath)

    if (length(converted_sheets) == 0) {
      stop(sprintf("Converted SMC workbook has no worksheets: %s", filepath), call. = FALSE)
    }

    return(invisible(filepath))
  }

  if (.Platform$OS.type != "windows") {
    stop(
      sprintf("SMC workbook conversion requires Windows Excel: %s", raw_path),
      call. = FALSE
    )
  }

  if (!file.exists(raw_path)) {
    stop(sprintf("Legacy SMC workbook is missing: %s", raw_path), call. = FALSE)
  }

  converter_full_path <- normalizePath(converter_path, winslash = "/", mustWork = TRUE)
  raw_full_path <- normalizePath(raw_path, winslash = "/", mustWork = TRUE)
  output_full_path <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
  conversion_status <- system2(
    command = "powershell.exe",
    args = c(
      "-NoProfile",
      "-ExecutionPolicy",
      "Bypass",
      "-File",
      shQuote(converter_full_path),
      "-InputPath",
      shQuote(raw_full_path),
      "-OutputPath",
      shQuote(output_full_path)
    )
  )

  if (conversion_status != 0) {
    stop(sprintf("Excel conversion failed for legacy SMC workbook: %s", raw_path), call. = FALSE)
  }

  if (!file.exists(filepath)) {
    stop(sprintf("Excel conversion produced no workbook: %s", filepath), call. = FALSE)
  }

  converted_sheets <- readxl::excel_sheets(filepath)

  if (length(converted_sheets) == 0) {
    stop(sprintf("Excel conversion produced no worksheets: %s", filepath), call. = FALSE)
  }

  invisible(filepath)
}

prepare_smc_manual_workbooks <- function(manual_fixes, converter_path) {
  purrr::pwalk(
    manual_fixes,
    function(raw_path, filepath, database_id) {
      convert_smc_manual_workbook(
        raw_path = raw_path,
        filepath = filepath,
        converter_path = converter_path
      )
    }
  )
}
