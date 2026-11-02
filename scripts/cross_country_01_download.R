# Download annual cross-country inputs for the one-time macroeconomic screen.
# Run from the repository root with Rscript scripts/cross_country_01_download.R.
# CROSS_COUNTRY_REFRESH=false rebuilds from cached API responses without network calls.
# CROSS_COUNTRY_REFRESH_WDI, _DATA360, and _IMF override the global setting per provider.
# CROSS_COUNTRY_RAW_ONLY=true saves the four requested inputs in raw without publishing final outputs.
# Keep WDI central-government debt separate from WEO general-government debt.
# Data360 fiscal-space primary and overall balances are distinct additional series.

## Setup ----
library(tidyverse)
library(xml2)
library(jsonlite)
source("R/source_helpers.R")
source("R/cross_country_api_helpers.R")

refresh <- cross_country_refresh_flag("CROSS_COUNTRY_REFRESH", TRUE)
refresh_wdi <- cross_country_refresh_flag("CROSS_COUNTRY_REFRESH_WDI", refresh)
refresh_data360 <- cross_country_refresh_flag("CROSS_COUNTRY_REFRESH_DATA360", refresh)
refresh_imf <- cross_country_refresh_flag("CROSS_COUNTRY_REFRESH_IMF", refresh)
raw_only <- cross_country_refresh_flag("CROSS_COUNTRY_RAW_ONLY", FALSE)
raw_dir <- "data/raw/cross_country_macro"
final_dir <- "data/final/cross_country_macro"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
if (!raw_only) dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
start_year <- 1950L
cutoff_year <- 2025L

## Country and indicator definitions ----
# Exclude World Bank aggregates by their official region classification.
wb_country_url <- "https://api.worldbank.org/v2/country?format=json&per_page=400"
wb_countries <- read_wb_pages(wb_country_url, "wb_countries", raw_dir, refresh_wdi) %>%
  filter(region.value != "Aggregates") %>%
  transmute(country_code = id, country = name, region = trimws(region.value))
wb_indicators <- tribble(
  ~indicator_id, ~variable, ~unit, ~government_scope,
  "NY.GDP.MKTP.KD.ZG", "gdp_growth", "annual_percent", NA_character_,
  "FP.CPI.TOTL.ZG", "inflation", "annual_percent", NA_character_,
  "GC.DOD.TOTL.GD.ZS", "debt_gdp", "percent_gdp", "central_government"
)
imf_indicators <- tribble(
  ~indicator_id, ~variable, ~unit, ~government_scope,
  "NGDP_RPCH", "gdp_growth", "annual_percent", NA_character_,
  "PCPIPCH", "inflation", "annual_percent", NA_character_,
  "GGXONLB_NGDP", "primary_balance_gdp", "percent_gdp", "general_government",
  "GGXWDG_NGDP", "debt_gdp", "percent_gdp", "general_government"
)
data360_indicators <- tribble(
  ~indicator_id, ~variable, ~government_scope,
  "WB_CCDFS_FBY", "overall_balance_gdp", "provider_fiscal_coverage",
  "WB_CCDFS_PBY", "primary_balance_gdp", "provider_fiscal_coverage",
  "WB_CCDFS_GGDY", "debt_gdp", "general_government"
)
if (raw_only) data360_indicators <- data360_indicators %>% filter(variable != "overall_balance_gdp")

## World Bank WDI ----
# Retain missing observations and source status; never replace missing values with zero.
wb_data <- pmap_dfr(wb_indicators, function(indicator_id, variable, unit, government_scope) {
  endpoint <- sprintf(
    "https://api.worldbank.org/v2/country/all/indicator/%s?source=2&date=1950:2025&format=json&per_page=20000",
    indicator_id
  )
  read_wb_pages(endpoint, paste0("wb_", indicator_id), raw_dir, refresh_wdi) %>%
    inner_join(wb_countries, by = c("countryiso3code" = "country_code")) %>%
    transmute(source = "WB_WDI", source_country_code = countryiso3code,
              country_code = countryiso3code, country = country,
              indicator_id = indicator_id, indicator = indicator.value,
              variable = .env$variable, unit = .env$unit, government_scope = .env$government_scope,
              year = as.integer(date), value = cross_country_numeric(value, .env$indicator_id),
              observation_status = obs_status, latest_actual_year = NA_integer_,
              latest_actual_label = NA_character_, period_class = "published_completed_year",
              source_updated_at, country_updated_at = NA_character_, downloaded_at,
              source_url = endpoint, source_version = "WDI_source_2")
})

## World Bank Data360 fiscal-space database ----
# Use the official indicator metadata to validate percent-of-GDP units and retain
# the provider's country catalog, including economies absent from WDI's catalog.
# OBS_STATUS=A is a provider status, not the WEO last-actual-year classification.
data360_data <- pmap_dfr(data360_indicators, function(indicator_id, variable, government_scope) {
  metadata_url <- sprintf(
    "https://data360files.worldbank.org/data360-data/metadata/WB_CCDFS/%s.json", indicator_id
  )
  metadata_path <- cache_response(metadata_url, paste0(indicator_id, "_metadata.json"), raw_dir, refresh_data360)
  metadata <- jsonlite::fromJSON(metadata_path)$series_description
  if (!identical(metadata$idno, indicator_id) || !identical(metadata$measurement_unit, "Percentage of GDP")) {
    rlang::abort("Unexpected Data360 indicator definition or units.", class = "macro_api_schema_error")
  }
  country_lookup <- as_tibble(metadata$ref_country) %>% transmute(REF_AREA = code, country = name)
  if (anyDuplicated(country_lookup$REF_AREA)) {
    rlang::abort("Data360 country metadata contains duplicates.", class = "macro_duplicate_error")
  }
  raw <- read_data360_pages(indicator_id, raw_dir, refresh_data360)
  if (any(!raw$REF_AREA %in% country_lookup$REF_AREA) || anyNA(raw$FREQ) || any(raw$FREQ != "A") ||
      anyNA(raw$UNIT_MEASURE) || any(raw$UNIT_MEASURE != "PT_GDP") ||
      anyNA(raw$UNIT_MULT) || any(raw$UNIT_MULT != 0)) {
    rlang::abort("Unexpected Data360 country, frequency, or unit.", class = "macro_api_schema_error")
  }
  raw %>% left_join(country_lookup, by = "REF_AREA") %>%
    transmute(source = "WB_CCDFS", source_country_code = REF_AREA, country_code = REF_AREA,
              country, indicator_id = INDICATOR, indicator = metadata$name,
              variable = .env$variable, unit = "percent_gdp", government_scope = .env$government_scope,
              year = as.integer(TIME_PERIOD), value = cross_country_numeric(OBS_VALUE, .env$indicator_id),
              observation_status = OBS_STATUS, latest_actual_year = NA_integer_,
              latest_actual_label = NA_character_, period_class = "published_completed_year",
              source_updated_at = NA_character_,
              metadata_updated_at = metadata$version_statement$version_date,
              country_updated_at = NA_character_, downloaded_at, source_url,
              source_version = metadata$version_statement$version)
})

## IMF WEO SDMX ----
# Resolve and pin the live flow version before requesting the four series together.
structure_url <- "https://api.imf.org/external/sdmx/2.1/dataflow/IMF.RES/WEO/latest?references=all"
structure_path <- cache_response(structure_url, "imf_structure.xml", raw_dir, refresh_imf)
structure <- read_xml(structure_path)
flow <- xml_find_first(structure, ".//*[local-name()='Dataflow' and @id='WEO']")
version <- xml_attr(flow, "version")
if (is.na(version)) {
  rlang::abort("IMF WEO version is missing.", class = "macro_api_schema_error")
}
key <- paste0(".", paste(imf_indicators$indicator_id, collapse = "+"), ".A")
imf_url <- sprintf("https://api.imf.org/external/sdmx/2.1/data/IMF.RES,WEO,%s/%s?startPeriod=1950", version, key)
imf_path <- cache_response(imf_url, "imf_data.xml", raw_dir, refresh_imf)
imf_xml <- read_xml(imf_path)
imf_dataset <- xml_find_first(imf_xml, ".//*[local-name()='DataSet']")

# The IMF country codelist includes groups and historic economies. Keep country codes,
# including Taiwan and Kosovo, while excluding the named three-letter regional groups.
country_nodes <- xml_find_all(structure, ".//*[local-name()='Codelist' and @id='CL_WEO_COUNTRY']/*[local-name()='Code']")
imf_countries <- tibble(
  country_code = xml_attr(country_nodes, "id"),
  country = xml_text(xml_find_first(country_nodes, "./*[local-name()='Name' and @*[local-name()='lang']='en']"))
) %>%
  filter(str_detect(country_code, "^[A-Z]{3}$"),
         !country_code %in% c("AFR", "APD", "EUR", "MCD", "WHD", "SDS"))

# Group metadata gives the last reported actual year separately for each country/series.
# Fiscal-year labels use the ending calendar year, retaining the original label for audit.
group_nodes <- xml_find_all(imf_xml, ".//*[local-name()='Group']")
imf_metadata <- map_dfr(group_nodes, ~ as_tibble_row(as.list(xml_attrs(.x)))) %>%
  rename(country_code = COUNTRY, indicator_id = INDICATOR,
         latest_actual_label = LATEST_ACTUAL_ANNUAL_DATA) %>%
  mutate(latest_actual_year = case_when(
    str_detect(latest_actual_label, "^[0-9]{4}$") ~ as.integer(str_extract(latest_actual_label, "^[0-9]{4}$")),
    str_detect(latest_actual_label, "^FY[0-9]{4}/[0-9]{2}$") ~
      as.integer(str_sub(latest_actual_label, 3, 6)) + 1L,
    TRUE ~ NA_integer_
  ))

# Parse the provider's SDMX document with the same package used by existing downloaders.
imf_data <- as_tibble(as.data.frame(rsdmx::readSDMX(file = imf_path, isURL = FALSE))) %>%
  rename(country_code = COUNTRY, indicator_id = INDICATOR) %>%
  inner_join(imf_countries, by = "country_code") %>%
  inner_join(imf_indicators, by = "indicator_id") %>%
  left_join(imf_metadata %>% select(country_code, indicator_id, latest_actual_label, latest_actual_year),
            by = c("country_code", "indicator_id")) %>%
  transmute(source = "IMF_WEO", source_country_code = country_code,
            country_code = case_when(country_code == "KOS" ~ "XKX",
                                     country_code == "WBG" ~ "PSE", TRUE ~ country_code),
            country, indicator_id,
            indicator = imf_indicators$variable[match(indicator_id, imf_indicators$indicator_id)],
            variable, unit, government_scope, year = as.integer(TIME_PERIOD),
            value = cross_country_numeric(OBS_VALUE, "IMF OBS_VALUE") * 10^cross_country_numeric(SCALE, "IMF SCALE"),
            observation_status = NA_character_, latest_actual_year, latest_actual_label,
            period_class = case_when(
              year > cutoff_year ~ "future_or_current_year_projection",
              is.na(latest_actual_year) ~ "actual_status_unknown",
              year <= latest_actual_year ~ "reported_actual",
              TRUE ~ "completed_year_estimate"
            ),
            source_updated_at = xml_attr(imf_dataset, "UPDATE_DATE"),
            country_updated_at = COUNTRY_UPDATE_DATE,
            downloaded_at = format(file.info(imf_path)$mtime, tz = "UTC", usetz = TRUE),
            source_url = imf_url, source_version = version)

## Validation and exports ----
# Validate country/indicator/year grain before building panels; keep forecasts separate.
all_data <- bind_rows(wb_data, imf_data, data360_data) %>%
  filter(year >= start_year) %>% arrange(source, country_code, variable, year)
if (anyDuplicated(all_data[c("source", "country_code", "indicator_id", "year")])) {
  rlang::abort("Duplicate country-indicator-year observations.", class = "macro_duplicate_error")
}
if (any(is.na(all_data$year)) || any(!is.finite(all_data$value) & !is.na(all_data$value))) {
  rlang::abort("Invalid year or non-finite observation.", class = "macro_value_error")
}
if (!setequal(unique(imf_data$indicator_id), imf_indicators$indicator_id) ||
    !setequal(unique(wb_data$indicator_id), wb_indicators$indicator_id) ||
    !setequal(unique(data360_data$indicator_id), data360_indicators$indicator_id)) {
  rlang::abort("One or more requested indicators are missing.", class = "macro_coverage_error")
}
if (raw_only) {
  # Keep source provenance and projection labels; leave model preparation and screens for later.
  requested_inputs <- all_data %>%
    filter(variable %in% c("gdp_growth", "inflation", "primary_balance_gdp", "debt_gdp"))
  write_csv(requested_inputs, file.path(raw_dir, "cross_country_inputs.csv"))
  write_csv(imf_metadata, file.path(raw_dir, "imf_series_metadata.csv"))
  message("Downloaded and validated the requested inputs in ", raw_dir)
} else {
  historical <- all_data %>% filter(year <= cutoff_year)
  write_csv(imf_metadata, file.path(final_dir, "imf_series_metadata.csv"))
  write_csv(all_data, file.path(final_dir, "all_source_observations.csv"))
  write_csv(historical, file.path(final_dir, "historical_observations.csv"))
  write_csv(historical %>% filter(source == "WB_CCDFS"),
            file.path(final_dir, "wb_fiscal_space_observations.csv"))
  write_csv(all_data %>% filter(year > cutoff_year), file.path(final_dir, "imf_projections.csv"))
  panel <- historical %>%
    select(source, country_code, country, year, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    arrange(source, country_code, year)
  write_csv(panel, file.path(final_dir, "country_year_panel.csv"))

  # Compare overlapping growth and CPI observations without conflating debt scopes.
  comparison <- historical %>% filter(variable %in% c("gdp_growth", "inflation")) %>%
    select(country_code, year, variable, source, value) %>%
    pivot_wider(names_from = source, values_from = value) %>%
    filter(!is.na(IMF_WEO), !is.na(WB_WDI)) %>%
    mutate(wdi_minus_weo_pp = WB_WDI - IMF_WEO) %>%
    arrange(variable, desc(abs(wdi_minus_weo_pp)))
  write_csv(comparison, file.path(final_dir, "overlapping_source_comparison.csv"))

  # CCDFS draws on multiple sources, including IMF inputs; it is a comparison source,
  # not independent evidence. Preserve any differences in its fiscal and debt values.
  fiscal_comparison <- historical %>%
    filter(source %in% c("IMF_WEO", "WB_CCDFS"), variable %in% c("primary_balance_gdp", "debt_gdp")) %>%
    select(country_code, year, variable, source, value) %>%
    pivot_wider(names_from = source, values_from = value) %>%
    filter(!is.na(IMF_WEO), !is.na(WB_CCDFS)) %>%
    mutate(ccdfs_minus_weo_pp = WB_CCDFS - IMF_WEO) %>%
    arrange(variable, desc(abs(ccdfs_minus_weo_pp)))
  write_csv(fiscal_comparison, file.path(final_dir, "overlapping_fiscal_source_comparison.csv"))

  # A country with no usable data still appears in coverage, with NA first/last years.
  coverage <- historical %>% group_by(source, country_code, country, variable) %>%
    summarise(rows = n(), observed_years = sum(!is.na(value)), missing_years = sum(is.na(value)),
              first_year = if (all(is.na(value))) NA_integer_ else min(year[!is.na(value)]),
              last_year = if (all(is.na(value))) NA_integer_ else max(year[!is.na(value)]),
              .groups = "drop")
  write_csv(coverage, file.path(final_dir, "coverage_by_country.csv"))
  write_csv(coverage %>% group_by(source, variable) %>%
              summarise(countries_with_data = sum(observed_years > 0),
                        observations = sum(observed_years),
                        first_year = min(first_year, na.rm = TRUE),
                        last_year = max(last_year, na.rm = TRUE), .groups = "drop"),
            file.path(final_dir, "coverage_summary.csv"))
  write_csv(bind_rows(wb_countries %>% mutate(source = "WB_WDI", source_country_code = country_code),
                      imf_countries %>% mutate(source = "IMF_WEO", source_country_code = country_code,
                        country_code = case_when(country_code == "KOS" ~ "XKX",
                                                 country_code == "WBG" ~ "PSE", TRUE ~ country_code)),
                      data360_data %>% distinct(source, source_country_code, country_code, country)),
            file.path(final_dir, "country_catalog.csv"))
  message("Wrote cross-country macro extracts to ", final_dir)
}
