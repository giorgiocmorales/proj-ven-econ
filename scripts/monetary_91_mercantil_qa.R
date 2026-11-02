# Compare the official-data multiplier with the user-provided Mercantil workbook.

library(tidyverse)
library(readxl)

rm(list = ls())

# Define the comparison inputs and output paths without adding the QA workbook
# to the official raw-to-final data lineage.
qa_workbook_path <- "data/raw/Mercantil QA/Liquidez VES.xlsx"
multiplier_path <- "data/final/monetary_03_multiplier_bcv.csv"
comparison_path <- "output/validation/monetary_mercantil_qa.csv"
summary_path <- "output/validation/monetary_mercantil_qa_summary.csv"
input_paths <- c(qa_workbook_path, multiplier_path)
missing_paths <- input_paths[!file.exists(input_paths)]

if (length(missing_paths) > 0) {
  stop(sprintf("Missing monetary QA inputs: %s", paste(missing_paths, collapse = ", ")), call. = FALSE)
}

# Extract the pre-calculated weekly multiplier from the comparison workbook.
qa_multiplier <- readxl::read_excel(
  path = qa_workbook_path,
  sheet = "Multiplicador"
) %>%
  dplyr::transmute(
    date = as.Date(.data$Fecha),
    qa_multiplier = as.numeric(.data$Multiplicador)
  ) %>%
  dplyr::filter(!is.na(.data$date), !is.na(.data$qa_multiplier))

if (anyDuplicated(qa_multiplier$date) > 0 || any(qa_multiplier$qa_multiplier <= 0)) {
  stop("Mercantil QA multiplier contains duplicate dates or non-positive values.", call. = FALSE)
}

# Join on exact weekly dates and preserve every discrepancy for review.
comparison <- readr::read_csv(multiplier_path, show_col_types = FALSE) %>%
  dplyr::select("date", bcv_pipeline_multiplier = "value") %>%
  dplyr::inner_join(qa_multiplier, by = "date") %>%
  dplyr::mutate(
    difference = .data$bcv_pipeline_multiplier - .data$qa_multiplier,
    relative_difference = .data$difference / .data$qa_multiplier,
    review_flag = dplyr::case_when(
      abs(.data$relative_difference) <= 1e-6 ~ "match",
      abs(.data$relative_difference) <= 0.01 ~ "minor_difference",
      TRUE ~ "review"
    )
  )

if (nrow(comparison) < 800) {
  stop("Mercantil QA comparison has insufficient overlapping weekly observations.", call. = FALSE)
}

# Summarize agreement while retaining the comparison as a non-authoritative QA check.
comparison_summary <- comparison %>%
  dplyr::summarise(
    observations = dplyr::n(),
    period_start = min(.data$date),
    period_end = max(.data$date),
    correlation = stats::cor(.data$bcv_pipeline_multiplier, .data$qa_multiplier),
    mean_absolute_error = mean(abs(.data$difference)),
    mean_absolute_relative_error = mean(abs(.data$relative_difference)),
    maximum_absolute_relative_error = max(abs(.data$relative_difference)),
    exact_scale_check_2010_h2 = max(
      abs(.data$relative_difference[
        .data$date >= as.Date("2010-07-01") & .data$date <= as.Date("2010-12-31")
      ])
    ),
    review_observations = sum(.data$review_flag == "review")
  )

dir.create(dirname(comparison_path), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(comparison, comparison_path)
readr::write_csv(comparison_summary, summary_path)
