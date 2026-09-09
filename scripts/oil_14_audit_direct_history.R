# Audit monthly official-production history against the supplied workbook and MOMR PDFs.
# This standalone audit writes evidence only; it does not splice sources into report graphs.
# JODI CRUDEOIL includes lease condensate and excludes NGL: retain its separate identity.
library(tidyverse)
source("R/source_helpers.R")
ensure_project_dirs()

# Read the reference without changing it; preserve exact worksheet and cell provenance.
reference_path <- Sys.getenv("OPEC_REFERENCE_WORKBOOK",
  "D:/Documents/3_Trabajos/Matar Tigres/Angel/Economia_A_C.xlsx")
reference_sheet <- "4-producci\u00f3n petrolera"
reference <- readxl::read_excel(reference_path, sheet = reference_sheet, col_names = FALSE,
                               col_types = "text", .name_repair = "minimal")
if (!identical(reference[[2]][[1]], "Comunicaci\u00f3n directa")) {
  rlang::abort("The reference workbook's direct-production column has changed", class = "opec_reference_error")
}
manual <- tibble(excel_row = seq_len(nrow(reference)), excel_date = reference[[1]],
                 reference_value = reference[[2]]) %>%
  filter(str_detect(.data$excel_date, "^[0-9]+$"), !is.na(.data$reference_value)) %>%
  mutate(date = floor_date(as.Date(as.double(.data$excel_date), origin = "1899-12-30"), "month"),
         reference_value = as.double(.data$reference_value),
         reference_cell = paste0("B", .data$excel_row), reference_sheet = reference_sheet,
         reference_file = reference_path, reference_md5 = unname(tools::md5sum(reference_path))) %>%
  select(-excel_date) %>% arrange(.data$date)
if (anyDuplicated(manual$date) || any(!is.finite(manual$reference_value))) {
  rlang::abort("Invalid or duplicate reference observations", class = "opec_reference_error")
}

# Compare every available MOMR vintage, so ordinary revisions are not mistaken for errors.
corpus <- read_csv("data/processed/oil_13_momr_corpus.csv", show_col_types = FALSE,
                   col_types = cols(footnote_marker = col_character()))
readr::stop_for_problems(corpus)
direct <- corpus %>% filter(.data$series_id == "venezuela", .data$source_type == "direct")
latest <- direct %>% group_by(.data$date) %>% slice_max(.data$report_month, n = 1L, with_ties = FALSE) %>%
  ungroup() %>% select(date, latest_value = value, latest_report = report_month)
vintages <- manual %>% inner_join(direct, by = "date", relationship = "many-to-many") %>%
  group_by(.data$date) %>% summarise(
    matches_any_vintage = any(.data$reference_value == .data$value, na.rm = TRUE),
    matching_reports = paste(format(.data$report_month[which(.data$reference_value == .data$value)], "%Y-%m"),
                             collapse = ";"), .groups = "drop")
comparison <- manual %>% left_join(latest, by = "date", relationship = "one-to-one") %>%
  left_join(vintages, by = "date", relationship = "one-to-one") %>%
  mutate(difference_from_latest = .data$reference_value - .data$latest_value)
write_csv(comparison, "data/interim/oil_14_manual_momr_comparison.csv")

# Independently scan all archived report text, including narrative and footnote contexts.
reports <- read_csv("registry/opec_momr_reports.csv", show_col_types = FALSE)
page_evidence <- list()
report_audit <- list()
for (i in seq_len(nrow(reports))) {
  month <- reports$report_month[[i]]
  key <- format(month, "%Y-%m")
  pdf <- file.path("data/raw/opec", key, "momr.pdf")
  cache_path <- file.path("data/interim/opec", paste0(key, "_text.rds"))
  if (!file.exists(cache_path)) {
    if (reports$status[[i]] != "unavailable") {
      rlang::abort(paste("Missing report text cache:", key), class = "opec_audit_input_error")
    }
    report_audit[[key]] <- tibble(report_month = month, status = "unavailable", pages = NA_integer_,
                                  direct_production_pages = NA_character_)
    next
  }
  cached <- readRDS(cache_path)
  if (!identical(cached$pdf_hash, unname(tools::md5sum(pdf)))) {
    rlang::abort(paste("Stale report text cache:", key), class = "opec_audit_input_error")
  }
  pages <- cached$pages
  production_pages <- which(str_detect(pages,
    regex("production[,\\s]+based on\\s+direct communication", ignore_case = TRUE)))
  # Search country and national-company references in all earlier reports.
  if (month < as.Date("2012-04-01")) {
    page_evidence[[key]] <- map_dfr(seq_along(pages), function(page) {
      lines <- str_split(pages[[page]], "\n")[[1]]
      hits <- which(str_detect(lines, regex("Venezuela|PDVSA", ignore_case = TRUE)))
      map_dfr(hits, function(line) {
        context <- paste(lines[seq(max(1L, line - 5L), min(length(lines), line + 5L))], collapse = "\n")
        if (!str_detect(context, regex("direct|official|communicat|inform|report|according|PDVSA", ignore_case = TRUE))) {
          return(tibble())
        }
        tibble(report_month = month, pdf_page = page, context = context, source_pdf_url = reports$pdf_url[[i]])
      })
    })
  }
  report_audit[[key]] <- tibble(report_month = month, status = "scanned", pages = length(pages),
                               direct_production_pages = paste(production_pages, collapse = ";"))
}
report_audit <- bind_rows(report_audit)
write_csv(report_audit, "data/interim/oil_14_report_text_audit.csv")
write_csv(bind_rows(page_evidence), "data/interim/oil_14_early_report_contexts.csv")

# Verify the 2002-2011 candidate history against official JODI annual CSV downloads.
# Dataset codes and units: https://www.jodidata.org/oil/database/data-downloads.aspx
dir.create("data/raw/jodi/oil/primary", recursive = TRUE, showWarnings = FALSE)
jodi <- map_dfr(2002:2011, function(year) {
  url <- paste0("https://www.jodidata.org/_resources/files/downloads/oil-data/annual-csv/primary/", year, ".csv")
  path <- file.path("data/raw/jodi/oil/primary", paste0(year, ".csv"))
  if (!file.exists(path)) {
    download_latest_file(url = url, dest_path = path, max_attempts = 2L, timeout_seconds = 60L)
  }
  data <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  readr::stop_for_problems(data)
  data %>% filter(.data$REF_AREA == "VE", .data$ENERGY_PRODUCT == "CRUDEOIL",
                  .data$FLOW_BREAKDOWN == "INDPROD", .data$UNIT_MEASURE == "KBD") %>%
    transmute(date = as.Date(paste0(.data$TIME_PERIOD, "-01")), jodi_value = as.double(.data$OBS_VALUE),
              assessment_code = .data$ASSESSMENT_CODE, unit = "tb/d", source_url = url,
              source_file = path, source_md5 = unname(tools::md5sum(path)))
})
if (nrow(jodi) != 120L || anyDuplicated(jodi$date) || any(!is.finite(jodi$jodi_value)) ||
    !setequal(jodi$date, seq(as.Date("2002-01-01"), as.Date("2011-12-01"), by = "month"))) {
  rlang::abort("Incomplete JODI reference window", class = "opec_audit_input_error")
}
jodi_comparison <- jodi %>% left_join(manual, by = "date", relationship = "one-to-one") %>%
  mutate(difference = .data$reference_value - .data$jodi_value,
         exact_match = .data$difference == 0)
write_csv(jodi_comparison, "data/interim/oil_14_manual_jodi_comparison.csv")
message(sprintf("Scanned %s reports. JODI comparison: %s of %s monthly reference values match exactly.",
                sum(report_audit$status == "scanned"), sum(jodi_comparison$exact_match), nrow(jodi_comparison)))
