# Build a candidate catalog of OPEC Monthly Oil Market Report archives.
#
# OPEC's year archive pages are available at
# https://www.opec.org/monthly-oil-market-report-YYYY.html. Local automated
# requests to those pages can be blocked, but their official PDF links use a
# stable month-year convention. This script adds unverified historical PDF
# candidates. The corpus audit, not registry status, determines extraction validity.

library(tidyverse)
library(lubridate)

rm(list = ls())

source("R/source_helpers.R")
ensure_project_dirs()

registry_path <- file.path("registry", "opec_momr_reports.csv")
start_month <- as.Date("2001-01-01")
# Include completed report months by default; the current month's issue may not be published yet.
end_month <- as.Date(Sys.getenv("OPEC_END_DATE", as.character(floor_date(Sys.Date(), "month") %m-% months(1))))

existing <- read_csv(
  registry_path,
  col_types = cols(
    report_month = col_date(),
    observation_month = col_date(),
    appendix_url = col_character(),
    pdf_url = col_character(),
    pdf_source = col_character(),
    page_price = col_integer(),
    page_production = col_integer(),
    status = col_character(),
    notes = col_character()
  )
)

report_months <- seq.Date(start_month, end_month, by = "month")

candidates <- tibble(report_month = report_months) %>%
  mutate(
    observation_month = report_month %m-% months(1),
    month_slug = str_to_lower(month.name[month(report_month)]),
    year = format(report_month, "%Y"),
    pdf_url = glue::glue("https://www.opec.org/assets/assetdb/momr-{month_slug}-{year}.pdf"),
    appendix_url = if_else(
      report_month >= as.Date("2019-02-01"),
      glue::glue("https://www.opec.org/assets/assetdb/momr-appendix-{month_slug}-{year}.xlsx"),
      NA_character_
    ),
    pdf_source = "official_candidate",
    page_price = NA_integer_,
    page_production = NA_integer_,
    status = "candidate",
    notes = "Generated from OPEC MOMR archive URL convention; verify parser compatibility before activation."
  ) %>%
  select(-month_slug, -year) %>%
  anti_join(existing, by = "report_month")

updated_registry <- bind_rows(existing, candidates) %>%
  arrange(desc(report_month))

# Discover real archive links for new or unresolved reports before relying on URL guesses.
# A blocked archive page is recorded explicitly; candidate downloads are still validated.
discovery_months <- bind_rows(candidates, existing %>% filter(.data$status == "unavailable")) %>%
  distinct(.data$report_month)
discovery_log <- list()
for (archive_year in unique(year(discovery_months$report_month))) {
  archive_url <- paste0("https://www.opec.org/monthly-oil-market-report-", archive_year, ".html")
  found <- tryCatch({
    response <- httr::GET(archive_url, httr::timeout(20))
    httr::stop_for_status(response)
    page <- xml2::read_html(httr::content(response, as = "raw"))
    rvest::html_elements(page, "a[href]") %>% rvest::html_attr("href") %>%
      xml2::url_absolute(archive_url) %>% unique()
  }, error = identity)
  if (inherits(found, "error")) {
    discovery_log[[as.character(archive_year)]] <- tibble(
      archive_url = archive_url, status = "error", details = conditionMessage(found))
    warning(paste("OPEC archive discovery failed:", archive_url, conditionMessage(found)), call. = FALSE)
    next
  }
  pdf_links <- found[str_detect(found, regex("\\.pdf(?:$|\\?)", ignore_case = TRUE)) &
                       !str_detect(found, regex("appendix", ignore_case = TRUE))]
  discovery_log[[as.character(archive_year)]] <- tibble(
    archive_url = archive_url, status = "ok", details = paste(length(pdf_links), "PDF links"))
  for (i in which(year(updated_registry$report_month) == archive_year &
                  updated_registry$report_month %in% discovery_months$report_month)) {
    month_name <- tolower(month.name[month(updated_registry$report_month[[i]])])
    matches <- pdf_links[str_detect(tolower(pdf_links), paste0(month_name, "[-_]", archive_year))]
    if (length(matches) == 1L) {
      updated_registry$pdf_url[[i]] <- matches[[1]]
      updated_registry$pdf_source[[i]] <- "official_archive"
    } else if (length(matches) > 1L) {
      rlang::abort(paste("Ambiguous OPEC archive links:", paste(matches, collapse = ", ")),
                   class = "opec_registry_error")
    }
  }
}
if (length(discovery_log) > 0L) write_csv(bind_rows(discovery_log), "data/raw/opec_archive_discovery.csv")
write_csv(updated_registry, registry_path)

message(glue::glue(
  "Added {nrow(candidates)} OPEC MOMR archive candidates; ",
  "{sum(updated_registry$status == 'active')} reports remain active."
))
