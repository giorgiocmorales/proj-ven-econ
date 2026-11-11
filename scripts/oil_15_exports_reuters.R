# Rebuild Venezuela's monthly crude-and-fuel exports from reviewed Reuters evidence.
# Inputs are a manually reviewed source/observation ledger and two downloaded Reuters
# chart payloads. This is an offline rebuild, not an automatic news scraper. Update
# the ledger and snapshot date after reviewing a new report; retain older vintages.
# Dates identify shipment months; source_date identifies the report/host publication.
# Latest eligible report wins. Differences can reflect revisions or rounding; neither
# missing months nor prior totals implied by percentage changes are reconstructed.
# Each month can come from a different report vintage, so Reuters' originally
# reported percentage changes need not reconcile with the selected monthly levels.
# Petrochemicals/byproducts reported separately in tonnes, production, imports,
# destination-only exports, forecasts and partial-month windows are excluded.
# Secondary Reuters citations and reposts remain explicit provenance limitations.
# The uncredited August 2021 reproduction and ambiguous February 2019 window remain
# in the evidence ledger but cannot supply the published series. No new dependencies.
# Explicit user-supplied values fill remaining gaps only, with separate provenance.
library(tidyverse)
library(lubridate)
# Use UTF-8 for shared source files on Windows, matching the existing graph scripts.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("A UTF-8 locale is required", class = "reuters_locale_error")
  }
}
source("R/source_helpers.R")
ensure_project_dirs()

# Fix the research vintage to the user-facing search cutoff, independent of host time.
raw_dir <- "data/raw/reuters_oil_exports"
snapshot_path <- file.path(raw_dir, "snapshot.json")
if (!file.exists(snapshot_path)) {
  rlang::abort("Reuters raw snapshot metadata is missing", class = "reuters_evidence_missing")
}
snapshot <- jsonlite::fromJSON(snapshot_path)
research_date <- as.Date(snapshot$reviewed_through)
if (length(research_date) != 1L || is.na(research_date)) {
  rlang::abort("Reuters snapshot requires one valid review date", class = "reuters_snapshot_error")
}
input_files <- file.path(raw_dir, c("reviewed_sources.json", "chart_2021_06_02.json",
                                  "chart_2020_05_04.js"))
if (!all(file.exists(input_files))) {
  rlang::abort(paste("Missing Reuters evidence:", paste(input_files[!file.exists(input_files)], collapse = ", ")),
               class = "reuters_evidence_missing")
}

# Keep the original numeric string and unit beside its normalized barrels/day value.
reviewed <- jsonlite::fromJSON(input_files[1], simplifyVector = FALSE)
article_rows <- map_dfr(reviewed, function(source_record) {
  map_dfr(source_record$observations, function(observation) {
    tibble(date = as.Date(paste0(observation[[1]], "-01")),
           raw_value = observation[[2]], raw_unit = observation[[3]],
           notes = observation[[4]], source_date = as.Date(source_record$date),
           source_url = source_record$url, source_title = source_record$title,
           access = source_record$access)
  })
})

# Extract exact numbers from Reuters' serialized 2021 Datawrapper table, not pixels.
chart_2021 <- jsonlite::fromJSON(input_files[2], simplifyVector = FALSE)
chart_2021_rows <- read_tsv(I(chart_2021$chartData), skip = 1,
  col_names = c("raw_month", "raw_value"), col_types = cols(.default = col_character()),
  show_col_types = FALSE) %>%
  mutate(date = as.Date(paste0("01-", .data$raw_month), format = "%d-%b-%y"),
         raw_unit = "bpd", source_date = as.Date(substr(chart_2021$chartJSON$publishedAt, 1, 10)),
         source_url = "https://graphics.reuters.com/VENEZUELA-OIL/EXPORTS/xegpbqdzjvq/",
         source_title = "Venezuelan exports fell amid outage, lack of diluents",
         access = "reuters_graphic_data", notes = "Exact underlying Reuters chart table; source: PDVSA / Refinitiv.") %>%
  select(-raw_month)

# The older chart stores a small array of date/value literals in its public script.
chart_2020_text <- read_file(input_files[3])
chart_2020_cells <- str_match_all(chart_2020_text,
  '\\{date:"([0-9/]+)","oil exports":"([0-9]+)"\\}')[[1]]
chart_2020_rows <- tibble(date = as.Date(chart_2020_cells[, 2], format = "%m/%d/%Y"),
  raw_value = chart_2020_cells[, 3], raw_unit = "bpd", source_date = as.Date("2020-05-04"),
  source_url = "https://fingfx.thomsonreuters.com/gfx/editorcharts/gjnpweajdpw/",
  source_title = "Venezuela oil exports hold up amid U.S. sanctions",
  access = "reuters_graphic_data", notes = "Exact underlying Reuters chart array linked in the 4 May 2020 report.")
if (nrow(chart_2020_rows) != 13L || nrow(chart_2021_rows) != 17L) {
  rlang::abort("Reuters chart schema or coverage changed", class = "reuters_chart_schema_error")
}

# Assign deterministic source IDs by URL; keep weaker or ambiguous candidates visible.
evidence <- bind_rows(article_rows, chart_2020_rows, chart_2021_rows) %>%
  mutate(exports_bpd = as.double(.data$raw_value) *
           case_when(.data$raw_unit == "bpd" ~ 1, .data$raw_unit == "million_bpd" ~ 1e6,
                     TRUE ~ NA_real_),
         eligible = !.data$access %in% c("uncredited_article_reproduction", "ambiguous_reporting_window"),
         weak_provenance = .data$access %in% c("secondary_citing_reuters", "reuters_text_reposted"),
         evidence_id = sprintf("E%03d", row_number()), retrieved_date = research_date) %>%
  left_join(bind_rows(article_rows, chart_2020_rows, chart_2021_rows) %>%
    distinct(.data$source_url, .data$source_date, .data$source_title, .data$access) %>%
    arrange(.data$source_date, .data$source_url) %>%
    mutate(source_id = sprintf("R%03d", row_number())) %>%
    select(source_url, source_id), by = "source_url")
# Unknown provenance labels must be reviewed before they can enter the series.
if (anyNA(evidence$access) || any(!evidence$access %in% c(
    "page_text", "search_text", "reuters_graphic_data", "secondary_citing_reuters", "reuters_text_reposted",
    "uncredited_article_reproduction", "ambiguous_reporting_window"))) {
  rlang::abort("Reuters evidence contains an unreviewed access category", class = "reuters_provenance_error")
}
if (anyNA(evidence$date) || anyNA(evidence$source_date) ||
    any(!is.finite(evidence$exports_bpd)) || any(evidence$exports_bpd <= 0) ||
    any(evidence$source_date < evidence$date) || any(evidence$source_date > research_date) ||
    anyDuplicated(evidence[c("source_id", "date")]) > 0L) {
  rlang::abort("Reuters evidence contains invalid values, dates, units or duplicate keys",
               class = "reuters_observation_error")
}

# Conflicting values on the same report date require review rather than a silent tie.
same_day_conflicts <- evidence %>% filter(.data$eligible) %>%
  group_by(.data$date, .data$source_date) %>%
  summarise(values = n_distinct(.data$exports_bpd), .groups = "drop") %>% filter(.data$values > 1L)
if (nrow(same_day_conflicts) > 0L) {
  rlang::abort("Different eligible Reuters values share a publication date", class = "reuters_vintage_conflict")
}

# Read explicit gap fills separately; source_date records when the user supplied them.
gap_path <- file.path(raw_dir, "user_gap_fills.csv")
if (!file.exists(gap_path)) {
  rlang::abort("User gap-fill ledger is missing", class = "reuters_evidence_missing")
}
user_fills <- read_csv(gap_path, show_col_types = FALSE,
  col_types = cols(date = col_date(), raw_value = col_character(), raw_unit = col_character(),
    source_date = col_date(), notes = col_character())) %>%
  mutate(exports_bpd = as.double(.data$raw_value) * 1000,
    source_url = NA_character_, source_title = "User-supplied monthly gap fills; original source unspecified",
    access = "user_supplied", eligible = TRUE, weak_provenance = TRUE,
    source_id = "U001", evidence_id = sprintf("UE%03d", row_number()),
    retrieved_date = .data$source_date)
if (anyNA(user_fills$date) || anyNA(user_fills$source_date) || anyNA(user_fills$raw_unit) ||
    any(user_fills$raw_unit != "thousand_bpd") ||
    any(!is.finite(user_fills$exports_bpd)) || any(user_fills$exports_bpd <= 0) ||
    any(day(user_fills$date) != 1L) || any(user_fills$source_date < user_fills$date) ||
    anyDuplicated(user_fills$date) > 0L) {
  rlang::abort("User gap fills require valid month keys, dates and positive kb/d values",
               class = "reuters_gap_fill_error")
}
evidence <- bind_rows(evidence, user_fills)

# Publish one row per month; eligible Reuters observations always take precedence.
eligible_rows <- evidence %>% filter(.data$eligible) %>% arrange(.data$source_date, .data$source_id)
vintage_summary <- eligible_rows %>% group_by(.data$date) %>%
  summarise(n_vintages = n(), n_distinct_values = n_distinct(.data$exports_bpd),
            first_reported_bpd = first(.data$exports_bpd),
            min_reported_bpd = min(.data$exports_bpd), max_reported_bpd = max(.data$exports_bpd),
            first_source_id = first(.data$source_id), .groups = "drop")
monthly <- eligible_rows %>% arrange(.data$access != "user_supplied", .data$source_date, .data$source_id) %>%
  group_by(.data$date) %>% slice_tail(n = 1L) %>% ungroup() %>%
  left_join(vintage_summary, by = "date") %>%
  complete(date = seq(min(.data$date), max(.data$date), by = "month")) %>%
  arrange(.data$date) %>%
  mutate(month = format(.data$date, "%Y-%m"), exports_thousand_bpd = .data$exports_bpd / 1000,
         status = case_when(is.na(.data$exports_bpd) ~ "not_verified",
                            .data$access == "user_supplied" ~ "user_supplied_gap_fill",
                            .data$weak_provenance ~ "reported_weaker_provenance",
                            TRUE ~ "reported"),
         different_vintages = !is.na(.data$n_distinct_values) & .data$n_distinct_values > 1L,
         latest_minus_first_bpd = .data$exports_bpd - .data$first_reported_bpd,
         retrieved_date = research_date,
         notes = case_when(.data$access == "user_supplied" ~ .data$notes,
                           is.na(.data$exports_bpd) & .data$month == "2019-02" ~ "Only a sanctions-window figure found; strict calendar-month average not verified.",
                           is.na(.data$exports_bpd) & .data$month == "2021-08" ~ "625840 bpd found in an uncredited reproduction; Reuters attribution not verified.",
                           is.na(.data$exports_bpd) & .data$month == "2022-12" ~ "Monthly figure not located in the reviewed Reuters material; annual averages are not substituted.",
                           TRUE ~ .data$notes)) %>%
  select(date, month, exports_bpd, exports_thousand_bpd, status,
         raw_value, raw_unit, source_date, source_id, evidence_id,
         access, source_url, source_title, notes,
         n_vintages, different_vintages, first_reported_bpd,
         latest_minus_first_bpd, min_reported_bpd, max_reported_bpd,
         first_source_id, retrieved_date)
evidence <- evidence %>% mutate(selected = .data$evidence_id %in% monthly$evidence_id) %>%
  arrange(desc(.data$date), desc(.data$source_date))

# Export source facts separately from the harmonized monthly and gap tables.
sources <- evidence %>% distinct(.data$source_id, .data$source_date, .data$source_title,
                                 .data$source_url, .data$access, .data$retrieved_date) %>%
  arrange(desc(.data$source_date), .data$source_id)
coverage <- monthly %>% mutate(year = year(.data$date)) %>% group_by(.data$year) %>%
  summarise(expected_months = n(), reported_months = sum(!is.na(.data$exports_bpd)),
            missing_months = paste(.data$month[is.na(.data$exports_bpd)], collapse = "; "),
            weaker_provenance_months = sum(.data$status == "reported_weaker_provenance"),
            user_supplied_months = sum(.data$status == "user_supplied_gap_fill"),
            differing_vintage_months = sum(.data$different_vintages), .groups = "drop")
write_csv(evidence, "data/interim/oil_15_exports_reuters_vintages.csv", na = "")
write_csv(sources, "data/interim/oil_15_exports_reuters_sources.csv", na = "")
write_csv(arrange(monthly, desc(.data$date)), "data/final/oil_15_exports_reuters_monthly.csv", na = "")
write_csv(filter(monthly, is.na(.data$exports_bpd)), "data/interim/oil_15_exports_reuters_gaps.csv", na = "")
write_csv(coverage, "data/interim/oil_15_exports_reuters_coverage.csv", na = "")
jsonlite::write_json(list(monthly = monthly %>% arrange(desc(.data$date)), evidence = evidence, sources = sources,
                coverage = coverage), "data/interim/oil_15_exports_reuters_review.json",
           dataframe = "rows", auto_unbox = TRUE, pretty = TRUE, na = "null")

# Report figures are built separately by oil_91_exports_graph.R using edition settings.
print(coverage)
message(sprintf("Published %d reported months of %d calendar months; %d sources; %d evidence rows.",
                sum(!is.na(monthly$exports_bpd)), nrow(monthly), nrow(sources), nrow(evidence)))
