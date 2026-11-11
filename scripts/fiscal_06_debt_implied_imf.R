# Reconstruct Venezuela's nominal gross debt from IMF debt/GDP and nominal GDP.
# Inputs are the exact-VEN API responses listed in the saved request manifest.
# OBS_VALUE already contains the source value: do not multiply it by SCALE.
# GDD/WEO combinations are sensitivity calculations with unmatched GDP vintages.
library(tidyverse)
library(xml2)

output_dir <- "outputs/imf_ven_debt"
requests <- read_csv(file.path(output_dir, "imf_debt_source_requests.csv"), show_col_types = FALSE)

# Extract source observations, including blanks, and retain publication provenance.
inputs <- pmap_dfr(requests, function(database, path, url, bytes) {
  document <- read_xml(path)
  dataset <- xml_find_first(document, ".//*[local-name()='DataSet']")
  if (inherits(dataset, "xml_missing")) rlang::abort(paste("Missing dataset:", database), class = "imf_debt_source_error")
  groups <- xml_find_all(dataset, "./*[local-name()='Group']")
  map_dfr(xml_find_all(dataset, "./*[local-name()='Series']"), function(series) {
    if (xml_attr(series, "COUNTRY") != "VEN" || xml_attr(series, "FREQUENCY") != "A") {
      rlang::abort("Source series is not annual Venezuela data.", class = "imf_debt_scope_error")
    }
    country_group <- groups[which(xml_attr(groups, "COUNTRY") == "VEN" &
      xml_attr(groups, "INDICATOR") == xml_attr(series, "INDICATOR"))]
    metadata <- if (length(country_group) == 1) xml_attrs(country_group[[1]]) else character()
    observations <- xml_find_all(series, "./*[local-name()='Obs']")
    tibble(database = database, country = "VEN", indicator = xml_attr(series, "INDICATOR"),
      year = as.integer(xml_attr(observations, "TIME_PERIOD")),
      source_value = xml_attr(observations, "OBS_VALUE"),
      value = parse_double(source_value, na = c("", "NaN", "NA")),
      scale_metadata = xml_attr(series, "SCALE"),
      series_update_date = xml_attr(series, "COUNTRY_UPDATE_DATE"),
      underlying_source = xml_attr(series, "DATA_SOURCE"),
      latest_actual_year = as.integer(metadata["LATEST_ACTUAL_ANNUAL_DATA"]),
      coverage_note = unname(metadata["METHODOLOGY_NOTES"]),
      debt_valuation = unname(metadata["FISCAL_SECTOR_VALUATION_OF_DEBT"]),
      publication_date = xml_attr(dataset, "PUBLICATION_DATE"), source_url = url)
  })
})
if (any(is.na(inputs$year))) rlang::abort("Invalid annual time period.", class = "imf_debt_period_error")
if (anyDuplicated(inputs[c("database", "indicator", "year")])) {
  rlang::abort("Duplicate source observations.", class = "imf_debt_duplicate_error")
}
write_csv(inputs, file.path(output_dir, "imf_ven_debt_inputs.csv"), na = "")

# Match the same fiscal period and expose all GDP vintages rather than splicing them.
gdp <- inputs %>% filter(indicator == "NGDPD") %>%
  transmute(gdp_database = database, year, nominal_gdp_usd = value,
    gdp_latest_actual_year = latest_actual_year,
    gdp_publication_date = publication_date, gdp_source_url = source_url)
ratios <- inputs %>%
  filter(indicator %in% c("FL_S1311MIXED_POGDP_PT", "FL_S13_POGDP_PT", "GGXWDG_NGDP"), !is.na(value)) %>%
  transmute(debt_database = database, debt_indicator = indicator, year,
    debt_scope = if_else(indicator == "FL_S1311MIXED_POGDP_PT", "central_government", "general_government"),
    debt_percent_gdp = value, debt_publication_date = publication_date,
    debt_latest_actual_year = latest_actual_year, debt_coverage_note = coverage_note,
    debt_valuation = debt_valuation,
    debt_underlying_source = underlying_source, debt_source_url = source_url)

# WEO debt and GDP use the same release; older GDD ratios are explicitly scenarios.
matched <- ratios %>% filter(debt_database != "GDD") %>%
  left_join(gdp, by = c("debt_database" = "gdp_database", "year")) %>%
  mutate(gdp_database = debt_database, vintage_match = "same_WEO_release")
mixed <- ratios %>% filter(debt_database == "GDD") %>%
  crossing(gdp_database = unique(gdp$gdp_database)) %>%
  left_join(gdp, by = c("gdp_database", "year")) %>%
  mutate(vintage_match = "mixed_sources_denominator_vintage_not_verified")
debt <- bind_rows(matched, mixed) %>%
  mutate(nominal_gdp_usd_billions = nominal_gdp_usd / 1e9,
    implied_debt_usd = debt_percent_gdp / 100 * nominal_gdp_usd,
    implied_debt_usd_billions = implied_debt_usd / 1e9,
    debt_observation_classification = case_when(is.na(debt_latest_actual_year) ~ "source_actual_cutoff_not_available",
      year > debt_latest_actual_year ~ "after_latest_actual_estimate_or_projection",
      TRUE ~ "at_or_before_latest_actual_year"),
    calculation_status = if_else(is.na(nominal_gdp_usd), "missing_nominal_gdp", "calculated"),
    interpretation = if_else(vintage_match == "same_WEO_release",
      "Same-release accounting reconstruction; not an independently observed debt stock.",
      "Illustrative only: GDP revisions or denominator differences change the implied debt stock.")) %>%
  arrange(debt_scope, debt_database, gdp_database, year)

# Check the accounting identity against WEO's separately published domestic-currency debt.
domestic <- inputs %>% filter(indicator %in% c("GGXWDG", "NGDP_FY")) %>%
  select(database, year, indicator, value) %>% pivot_wider(names_from = indicator, values_from = value)
debt <- debt %>% left_join(domestic, by = c("debt_database" = "database", "year")) %>%
  mutate(implied_debt_domestic = debt_percent_gdp / 100 * NGDP_FY,
    published_debt_domestic = GGXWDG,
    domestic_identity_relative_difference = if_else(!is.na(GGXWDG) & GGXWDG != 0,
      (implied_debt_domestic - GGXWDG) / GGXWDG, NA_real_),
    domestic_check = case_when(is.na(GGXWDG) ~ "no_published_domestic_debt_for_check",
      abs(domestic_identity_relative_difference) <= 1e-4 ~ "consistent_with_source_rounding",
      TRUE ~ "source_rounding_or_denominator_discrepancy")) %>% select(-GGXWDG, -NGDP_FY)
if (any(debt$debt_percent_gdp < 0) || any(debt$nominal_gdp_usd <= 0, na.rm = TRUE)) {
  rlang::abort("Invalid gross-debt ratio or GDP value.", class = "imf_debt_value_error")
}
stopifnot(all(abs(debt$implied_debt_usd - debt$debt_percent_gdp / 100 * debt$nominal_gdp_usd) < 0.01, na.rm = TRUE))
write_csv(debt, file.path(output_dir, "imf_ven_implied_debt.csv"), na = "")

# Export a compact comparison emphasizing recent years and the last central-government ratio.
comparison <- debt %>%
  filter((debt_database == "WEO_2026_APR_VINTAGE" & year %in% c(2019L, 2024L, 2025L)) |
    (debt_database == "GDD" & debt_scope == "central_government" & year == 2019L) |
    (debt_database == "GDD" & debt_scope == "general_government" & year == 2024L)) %>%
  select(debt_scope, year, debt_database, gdp_database, debt_percent_gdp,
    nominal_gdp_usd_billions, implied_debt_usd_billions, vintage_match, domestic_check)
write_csv(comparison, file.path(output_dir, "imf_ven_debt_comparison.csv"), na = "")
print(comparison, n = Inf, width = Inf)
message("Saved ", nrow(debt), " year/vintage combinations; ", sum(debt$calculation_status == "calculated"), " have a nominal-debt estimate.")

# Present one row per year using April 2026 GDP, preserving unavailable debt values.
preview <- gdp %>%
  filter(gdp_database == "WEO_2026_APR_VINTAGE", between(year, 1999L, 2026L)) %>%
  transmute(year, nominal_gdp_usd_billions = nominal_gdp_usd / 1e9) %>%
  left_join(debt %>%
    filter(gdp_database == "WEO_2026_APR_VINTAGE",
      (debt_scope == "central_government" & debt_database == "GDD") |
        (debt_scope == "general_government" & debt_database == "WEO_2026_APR_VINTAGE")) %>%
    select(year, debt_scope, debt_percent_gdp, implied_debt_usd_billions) %>%
    pivot_wider(names_from = debt_scope,
      values_from = c(debt_percent_gdp, implied_debt_usd_billions),
      names_glue = "{debt_scope}_{.value}"), by = "year") %>%
  select(year, nominal_gdp_usd_billions, central_government_debt_percent_gdp,
    central_government_implied_debt_usd_billions, general_government_debt_percent_gdp,
    general_government_implied_debt_usd_billions) %>% arrange(year)
stopifnot(nrow(preview) == 28L, identical(preview$year, 1999:2026))
write_csv(preview, file.path(output_dir, "imf_ven_debt_1999_2026.csv"), na = "")
