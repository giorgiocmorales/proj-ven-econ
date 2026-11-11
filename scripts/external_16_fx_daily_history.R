# Extend daily official/parallel FX in a new dataset; BCV/Yadio inputs are read-only.
# Run after external_95_fx_combine.R and external_15_fx_palma.R. No downloads occur.
# Distinct official regimes and EUR quotes remain separate. Historical source priority
# follows registry/palma_fx_columns.csv; canonical values always take precedence.
library(tidyverse)
source("R/source_helpers.R")
source("R/palma_fx_helpers.R")
ensure_project_dirs()
dataset_id <- "external_16_fx_daily_history"

# Capture all canonical input checksums and preserve the exact calendar vintage used.
protected_paths <- c("data/final/external_fx.csv", "data/final/external_01_fx_smc_bcv.csv",
  "data/final/external_02_fx_ind_bcv.csv", "data/final/external_03_fx_int_bcv.csv",
  "data/final/external_04_fx_yadio.csv")
if (any(!file.exists(protected_paths))) {
  rlang::abort("Missing canonical FX inputs", class = "palma_fx_canonical_error")
}
# Older acquisitions may not have an API JSON archive; protect existing raw files too.
protected_paths <- unique(c(protected_paths, list.files("data/raw", full.names = TRUE,
  pattern = "^(yadio|bcv_indx|2_1_2a).*\\.(json|html|xls|xlsx)$")))
protected <- tibble(path = protected_paths,
  sha256 = map_chr(protected_paths, digest::digest, algo = "sha256", file = TRUE))
canonical_sha256 <- protected$sha256[protected$path == "data/final/external_fx.csv"]
canonical_archive <- file.path("data/interim/palma_fx/canonical", paste0(canonical_sha256, ".csv"))
dir.create(dirname(canonical_archive), recursive = TRUE, showWarnings = FALSE)
if (!file.exists(canonical_archive) &&
    !file.copy("data/final/external_fx.csv", canonical_archive, overwrite = FALSE, copy.date = TRUE)) {
  rlang::abort("Could not archive the canonical FX vintage", class = "palma_fx_archive_error")
}
if (digest::digest(file = canonical_archive, algo = "sha256") != canonical_sha256) {
  rlang::abort("Canonical FX archive checksum mismatch", class = "palma_fx_archive_error")
}
canonical <- read_csv(canonical_archive, show_col_types = FALSE)
canonical_text <- read_csv(canonical_archive, col_types = cols(.default = col_character()), show_col_types = FALSE)
historical <- read_csv("data/final/external_15_fx_palma.csv", show_col_types = FALSE,
  col_types = cols(value_cell_raw = col_character(), date_cell_raw = col_character()))
# Quarantined dates still block carry-forwards; they never become published quotes.
historical <- bind_rows(historical,
  read_csv("data/interim/external_15_fx_palma_quarantine.csv", show_col_types = FALSE,
    col_types = cols(value_cell_raw = col_character(), date_cell_raw = col_character())))
history_candidates <- palma_fx_expand_history(historical)
canonical_candidates <- palma_fx_canonical(canonical, canonical_text, canonical_archive, canonical_sha256)

# Re-express the nominal value in the currency active on each output date. The source
# quote/date/denomination remain separate, including when carrying over a reconversion.
scales <- read_csv("registry/venezuela_currency_scales.csv", show_col_types = FALSE)
candidates <- bind_rows(history_candidates, canonical_candidates) %>%
  select(-any_of(c("currency_nominal", "nominal_conversion_factor", "rate_nominal"))) %>%
  crossing(scales) %>% filter(date >= effective_start, is.na(effective_end) | date <= effective_end) %>%
  mutate(currency_nominal = currency_code, nominal_conversion_factor = current_ves_per_currency_unit,
    rate_nominal = if_else(source_currency == currency_nominal, rate_raw,
      rate_adjusted / nominal_conversion_factor), currency_adjusted = "VES_current",
    unit_adjusted = paste0("current_VES_per_", quote_currency),
    canonical_denomination_differs_from_date = canonical & source_id == "bcv" & source_currency != currency_nominal) %>%
  select(-all_of(names(scales)))
selected <- palma_fx_select_daily(candidates) %>% mutate(dataset_id = .env$dataset_id) %>%
  select(dataset_id, date, variable_id, market, quote_currency, rate_nominal, currency_nominal,
    nominal_conversion_factor, rate_adjusted, currency_adjusted, unit_adjusted, canonical,
    source_id, source_series, rate_raw, source_currency, source_conversion_factor,
    observation_date, value_method, carried_days, observation_id, workbook_id, sheet,
    source_cell, source_row, source_key_rows, quality_status, value_cell_raw, adjusted_cell_raw,
    canonical_denomination_differs_from_date, source_file, source_path, source_sha256)

# Keep every candidate in overlap comparisons; no averaging or hidden source replacement.
comparison <- candidates %>% group_by(date, variable_id, quote_currency) %>%
  filter(n() > 1L) %>% ungroup() %>%
  left_join(selected %>% select(date, variable_id, quote_currency, selected_id = observation_id,
      selected_rate = rate_adjusted, selected_canonical = canonical),
    by = c("date", "variable_id", "quote_currency"), relationship = "many-to-one") %>%
  transmute(date, variable_id, quote_currency, source_id, workbook_id, sheet, observation_id,
    observation_date, value_method, rate_adjusted, canonical, priority,
    selected = observation_id == selected_id, selected_id, selected_rate, selected_canonical,
    difference_percent = 100 * (rate_adjusted / selected_rate - 1))
if (anyDuplicated(selected[c("date", "variable_id", "quote_currency")]) ||
    anyNA(selected[c("date", "rate_nominal", "rate_adjusted")]) ||
    any(abs(selected$rate_adjusted - selected$rate_nominal * selected$nominal_conversion_factor) >
      1e-12 * abs(selected$rate_adjusted))) {
  rlang::abort("Invalid consolidated FX keys or reconversion identity", class = "palma_fx_output_error")
}

# Verify exact canonical preservation in the new output, then prove no input bytes changed.
expected <- canonical_candidates %>% filter(variable_id != "official_headline") %>%
  select(date, variable_id, quote_currency, expected_raw = rate_raw, expected_adjusted = rate_adjusted)
check <- expected %>% left_join(selected, by = c("date", "variable_id", "quote_currency"), relationship = "one-to-one")
if (anyNA(check$rate_adjusted) || !all(check$canonical) ||
    !identical(check$expected_raw, check$rate_raw) || !identical(check$expected_adjusted, check$rate_adjusted) ||
    !identical(protected$sha256, map_chr(protected_paths, digest::digest, algo = "sha256", file = TRUE))) {
  rlang::abort("Canonical BCV/Yadio preservation failed", class = "palma_fx_canonical_error")
}
write_csv(protected, "data/interim/external_16_fx_daily_history_inputs.csv")
write_interim_dataset(comparison, paste0(dataset_id, "_overlap"))
# Retain original canonical decimal tokens in the CSV numeric fields. Ordinary CSV
# double formatting can change a final binary rounding bit even without any arithmetic.
serialized <- selected %>% mutate(
  rate_raw = if_else(canonical, value_cell_raw, sprintf("%.17g", rate_raw)),
  rate_adjusted = if_else(canonical, adjusted_cell_raw, sprintf("%.17g", rate_adjusted)))
write_final_dataset(serialized, dataset_id)
write_interim_dataset(selected %>% group_by(variable_id, market, quote_currency, source_id, canonical) %>%
  summarise(first_date = min(date), last_date = max(date), rows = n(),
    carried_rows = sum(value_method == "carried_forward"), .groups = "drop"), paste0(dataset_id, "_coverage"))
message("Consolidated ", nrow(selected), " daily FX rows across ", n_distinct(selected$variable_id),
  " series; canonical quotes and input checksums preserved")
