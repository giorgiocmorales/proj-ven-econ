# Scrape the four Wikipedia wage chronologies, retaining source bytes and original cells.
# Set MINIMUM_WAGE_OFFLINE=true to rebuild from the retained HTML without downloading.
library(dplyr)
library(tidyr)
library(stringr)
source("R/source_helpers.R")
ensure_project_dirs()

dataset_id <- "real_13_minimum_wage_wikipedia"
source_url <- "https://es.wikipedia.org/wiki/Anexo:Salario_m%C3%ADnimo_en_Venezuela"
raw_path <- "data/raw/minimum_wage_wikipedia/latest.html"
offline <- identical(Sys.getenv("MINIMUM_WAGE_OFFLINE"), "true") ||
  (source_acquisition_mode() != "update" && file.exists(raw_path))
dir.create(dirname(raw_path), recursive = TRUE, showWarnings = FALSE)
candidate_path <- if (offline) raw_path else tempfile(tmpdir = dirname(raw_path), fileext = ".html")
if (!file.exists(candidate_path) && offline) {
  rlang::abort("Missing cached Wikipedia wage HTML", class = "minimum_wage_source_error")
}
if (!offline) download_latest_file(source_url, candidate_path, max_attempts = 3, timeout_seconds = 120)

# Identify chronologies by their headings; unrelated country comparisons are excluded.
# html_table expands rowspans, retaining the page's repeated wage/allowance values.
page <- xml2::read_html(candidate_path)
tables <- rvest::html_elements(page, "table.wikitable")
source_tables <- purrr::map_dfr(tables, function(table) {
  heading <- rvest::html_element(table, xpath = "preceding::h3[1]") %>% rvest::html_text2()
  if (is.na(heading) || !str_detect(heading, "^Cono monetario \\(V(?:EB|EF|ES|ED)\\)$")) {
    return(tibble::tibble())
  }
  cells <- rvest::html_table(table, header = FALSE, convert = FALSE)
  if (!identical(cells[[1]][1], "Vigencia")) return(tibble::tibble())
  if (ncol(cells) < 3L || !str_detect(cells[[2]][1], fixed("Salario")) ||
      !str_detect(cells[[3]][1], "alimentaci")) {
    rlang::abort("Wikipedia wage chronology columns changed", class = "minimum_wage_schema_error")
  }
  cells %>% transmute(source_table = heading, source_row = row_number(),
    date_cell_raw = .data$X1, minimum_wage_cell_raw = .data$X2,
    food_allowance_cell_raw = .data$X3) %>% slice(-1L) %>%
    filter(!if_all(ends_with("cell_raw"), ~ is.na(.x) | str_squish(.x) == ""))
})
expected_tables <- paste0("Cono monetario (", c("VEB", "VEF", "VES", "VED"), ")")
if (!setequal(unique(source_tables$source_table), expected_tables)) {
  rlang::abort("Expected all four Wikipedia currency chronologies", class = "minimum_wage_schema_error")
}

# Keep raw text separate from numeric interpretation; dates follow the listed chronology.
observations <- source_tables %>%
  mutate(date_text = str_remove_all(date_cell_raw, "\\[[^]]*\\]"),
    date = parse_bcv_date(str_remove_all(date_text, "\\bde\\b")),
    currency_raw = case_when(source_table == expected_tables[1] ~ "VEB",
      source_table == expected_tables[2] ~ "VEF", source_table == expected_tables[3] ~ "VES_2018",
      source_table == expected_tables[4] ~ "VES_current")) %>%
  pivot_longer(c(minimum_wage_cell_raw, food_allowance_cell_raw),
    names_to = "variable_id", names_pattern = "(.*)_cell_raw", values_to = "value_cell_raw") %>%
  mutate(value_text = str_remove_all(value_cell_raw, "\\[[^]]*\\]"),
    value_text = str_remove_all(value_text, "[\\s\\p{Z}]+"),
    value_text = na_if(value_text, ""),
    punctuation_normalized = !is.na(value_text) &
      str_detect(value_text, "^\\d{1,3}(?:\\.\\d{3})*\\.\\d{2}$"),
    value_text = if_else(punctuation_normalized,
      str_replace(value_text, "\\.(\\d{2})$", ",\\1"), value_text))
if (anyNA(observations$date) || anyDuplicated(observations[c("date", "variable_id")]) ||
    any(!is.na(observations$value_text) &
      !str_detect(observations$value_text, "^(?:\\d+|\\d{1,3}(?:\\.\\d{3})+)(?:,\\d{1,2})?$"))) {
  rlang::abort("Invalid chronology dates, duplicate keys or unrecognized amounts",
    class = "minimum_wage_parse_error")
}
observations <- observations %>% mutate(value_raw = as.numeric(
  str_replace(str_remove_all(value_text, fixed(".")), fixed(","), ".")))

# Reuse the project's dated reconversion registry, rather than the page's USD/real columns.
currency_scales <- readr::read_csv("registry/venezuela_currency_scales.csv", show_col_types = FALSE)
observations <- observations %>% left_join(currency_scales, by = c("currency_raw" = "currency_code"),
  relationship = "many-to-one") %>%
  mutate(conversion_factor = current_ves_per_currency_unit, value = value_raw * conversion_factor)
if (anyNA(observations$conversion_factor) ||
    any(observations$date < observations$effective_start |
      (!is.na(observations$effective_end) & observations$date > observations$effective_end)) ||
    any(!is.na(observations$value_raw) & (!is.finite(observations$value_raw) | observations$value_raw < 0))) {
  rlang::abort("Invalid source amount or currency regime", class = "minimum_wage_currency_error")
}

# Preserve an immutable HTML vintage and its permanent revision URL before publishing extracts.
source_sha256 <- digest::digest(file = candidate_path, algo = "sha256")
vintage_path <- file.path(dirname(raw_path), "vintages", paste0(source_sha256, ".html"))
dir.create(dirname(vintage_path), recursive = TRUE, showWarnings = FALSE)
if (file.exists(vintage_path)) {
  if (!identical(digest::digest(file = vintage_path, algo = "sha256"), source_sha256)) {
    rlang::abort("Wikipedia wage archive checksum mismatch", class = "minimum_wage_archive_error")
  }
} else if (!file.copy(candidate_path, vintage_path, overwrite = FALSE, copy.date = TRUE)) {
  rlang::abort("Could not archive Wikipedia wage HTML", class = "minimum_wage_archive_error")
}
revision_href <- rvest::html_element(page, "a[href*='oldid=']") %>% rvest::html_attr("href")
if (is.na(revision_href)) {
  rlang::abort("Wikipedia permanent revision link is missing", class = "minimum_wage_source_error")
}
revision_url <- xml2::url_absolute(revision_href, source_url)
downloaded_at <- format(file.info(candidate_path)$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
observations <- observations %>% mutate(dataset_id = dataset_id, source_id = "wikipedia",
  source_url = source_url, revision_url = revision_url,
  source_file = vintage_path, source_sha256 = source_sha256, downloaded_at = downloaded_at,
  currency = "VES_current", unit = "current_VES_per_month",
  date_basis = "source_listed_date_not_independently_verified",
  value_status = if_else(is.na(value_raw), "source_missing", "source_reported")) %>%
  select(dataset_id, source_id, date, variable_id, value_raw, currency_raw, conversion_factor,
    value, currency, unit, value_status, punctuation_normalized, source_table, source_row,
    date_cell_raw, value_cell_raw, date_basis, source_url, revision_url, source_file,
    source_sha256, downloaded_at) %>% arrange(date, variable_id)
if (!offline && !file.copy(candidate_path, raw_path, overwrite = TRUE, copy.date = TRUE)) {
  rlang::abort("Could not promote Wikipedia wage HTML", class = "minimum_wage_write_error")
}
write_raw_dataset(source_tables, dataset_id)
write_final_dataset(observations, dataset_id)
if (!offline) unlink(candidate_path)
message("Imported ", nrow(observations), " wage/food observations through ", max(observations$date))
