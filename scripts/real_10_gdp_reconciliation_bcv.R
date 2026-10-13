# Generate reconciled annual and quarterly GDP indices from BCV growth extracts.
# Relative levels use 2007 annual GDP = 100; they are inferred, not published levels.
# Both current workbook headings specify 2007 prices; older levels use 1997 prices.
# Run against the existing processed extracts without refreshing or replacing them.
library(tidyverse)
source("R/source_helpers.R")
ensure_project_dirs()

# Keep source families separate: activity and institutional-sector totals overlap.
source_pairs <- tribble(
  ~family, ~annual_file, ~quarterly_file,
  "activity", "data/processed/real_01_gdp_a_a_bcv.csv", "data/processed/real_03_gdp_q_a_bcv.csv",
  "institutional", "data/processed/real_02_gdp_a_s_bcv.csv", "data/processed/real_04_gdp_q_s_bcv.csv"
)
base_year <- 2007L
index_base_annual_total <- 100
price_base_year <- 2007L
tolerance_pp <- 1e-6
output_dir <- "data/interim"
generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
annual_results <- list()
quarterly_results <- list()
fit_results <- list()
ytd_results <- list()

# Solve the four initial quarter shares from all annual level restrictions.
# Eliminate Q4 to impose shares summing to one exactly. No seasonal profile is assumed.
for (pair_id in seq_len(nrow(source_pairs))) {
  pair <- source_pairs[pair_id, ]
  annual <- read_csv(pair$annual_file, show_col_types = FALSE)
  quarterly <- read_csv(pair$quarterly_file, show_col_types = FALSE)
  for (frequency_id in c("annual", "quarterly")) {
    input <- if (frequency_id == "annual") annual else quarterly
    required_columns <- c("series_label", "year", "value", "date", "frequency", "unit",
                          "provisional", "source_url", "downloaded_at")
    if (frequency_id == "quarterly") required_columns <- c(required_columns, "quarter")
    if (!all(required_columns %in% names(input))) {
      rlang::abort("Missing required BCV GDP columns", class = "gdp_reconciliation_input_error")
    }
    if (!nrow(input) || anyNA(input[required_columns]) ||
        any(!is.finite(input$value)) || any(input$value <= -100) ||
        any(input$unit != "percent_yoy") || any(input$frequency != frequency_id) ||
        any(input$year != as.integer(input$year)) || any(!nzchar(input$series_label)) ||
        !is.logical(input$provisional)) {
      rlang::abort("Invalid BCV growth observations", class = "gdp_reconciliation_input_error")
    }
  }
  if (!setequal(annual$series_label, quarterly$series_label)) {
    rlang::abort("Annual and quarterly series labels differ", class = "gdp_reconciliation_input_error")
  }
  for (label in unique(annual$series_label)) {
    a <- annual %>% filter(series_label == label) %>% arrange(year)
    q <- quarterly %>% filter(series_label == label) %>% arrange(year, quarter)
    if (anyDuplicated(a$year) || anyDuplicated(q[c("year", "quarter")]) ||
        anyNA(q$quarter) || any(!q$quarter %in% 1:4) ||
        any(a$date != lubridate::make_date(a$year, 1L, 1L)) ||
        any(q$date != lubridate::make_date(q$year, 3L * q$quarter - 2L, 1L)) ||
        !identical(as.integer(a$year), seq.int(base_year + 1L, max(a$year))) ||
        !identical(as.integer(4L * q$year + q$quarter),
                   seq.int(4L * (base_year + 1L) + 1L, max(4L * q$year + q$quarter)))) {
      rlang::abort("Duplicate or missing BCV GDP periods", class = "gdp_reconciliation_input_error")
    }

    # YoY rates determine four independent quarter chains relative to 2007.
    a <- a %>% mutate(annual_index = index_base_annual_total * cumprod(1 + value / 100))
    q <- q %>% group_by(quarter) %>%
      mutate(quarter_factor = cumprod(1 + value / 100)) %>% ungroup()
    factors <- q %>% select(year, quarter, quarter_factor) %>%
      pivot_wider(names_from = quarter, values_from = quarter_factor, names_prefix = "q") %>%
      arrange(year)
    fit_data <- a %>% left_join(factors, by = "year")
    if (anyNA(fit_data[c("q1", "q2", "q3", "q4")])) {
      rlang::abort("Annual benchmark lacks four quarters", class = "gdp_reconciliation_input_error")
    }
    design <- as.matrix(fit_data[c("q1", "q2", "q3")]) - fit_data$q4
    target <- fit_data$annual_index / index_base_annual_total - fit_data$q4
    if (qr(design)$rank != 3L) {
      rlang::abort("Starting quarter shares are not identified", class = "gdp_reconciliation_rank_error")
    }
    coefficients <- qr.solve(design, target)
    shares <- c(coefficients, 1 - sum(coefficients))
    if (any(!is.finite(shares)) || any(shares <= 0)) {
      rlang::abort("BCV rates imply non-positive starting quarter shares",
                   class = "gdp_reconciliation_share_error")
    }

    # Reconstruct levels and recover full previous-year weights even for incomplete years.
    previous_annual <- bind_rows(
      tibble(year = base_year, annual_index = index_base_annual_total),
      a %>% select(year, annual_index)
    ) %>% transmute(year = year + 1L, previous_year_annual_index = annual_index)
    q <- q %>% mutate(
      family = pair$family, inferred_index = index_base_annual_total * shares[quarter] * quarter_factor,
      initial_share = shares[quarter], annual_source_file = pair$annual_file,
      quarterly_source_file = pair$quarterly_file,
      previous_year_quarter_index = inferred_index / (1 + value / 100)
    ) %>% left_join(previous_annual, by = "year") %>% group_by(year) %>% mutate(
      previous_year_matched_period_weight = previous_year_quarter_index /
        sum(previous_year_quarter_index),
      previous_year_annual_weight = previous_year_quarter_index / previous_year_annual_index,
      inferred_ytd_growth_percent = 100 * (cumsum(inferred_index) /
        cumsum(previous_year_quarter_index) - 1)
    ) %>% ungroup() %>% arrange(year, quarter)
    if (anyNA(q$previous_year_annual_index)) {
      rlang::abort("Quarterly data extend beyond the available annual benchmarks",
                   class = "gdp_reconciliation_input_error")
    }

    # Include inferred 2007 quarters internally so growth uses valid base-period levels.
    # Adjacent-quarter growth retains seasonality; no seasonal adjustment is applied.
    quarterly_changes <- tibble(index = c(index_base_annual_total * shares, q$inferred_index)) %>% mutate(
      inferred_quarter_yoy_percent = 100 * (index / lag(index, n = 4L) - 1),
      inferred_qoq_unadjusted_percent = 100 * (index / lag(index, n = 1L) - 1),
      cumulative_index = cumsum(index),
      inferred_rolling_four_quarter_index = if_else(row_number() >= 4L,
        cumulative_index - lag(cumulative_index, n = 4L, default = 0), NA_real_),
      inferred_rolling_four_quarter_yoy_percent = 100 * (
        inferred_rolling_four_quarter_index /
          lag(inferred_rolling_four_quarter_index, n = 4L) - 1)
    ) %>% slice(-(1:4)) %>% select(starts_with("inferred_"))
    q <- bind_cols(q, quarterly_changes)

    # Every same-quarter YoY must match BCV; every Q4 rolling YoY must match annual GDP.
    q4_growth <- q %>% filter(quarter == 4L) %>%
      select(year, inferred_rolling_four_quarter_yoy_percent) %>%
      inner_join(a %>% select(year, value), by = "year")
    if (anyNA(q$inferred_quarter_yoy_percent) ||
        max(abs(q$inferred_quarter_yoy_percent - q$value)) > tolerance_pp ||
        nrow(q4_growth) != nrow(a) ||
        anyNA(q4_growth$inferred_rolling_four_quarter_yoy_percent) ||
        max(abs(q4_growth$inferred_rolling_four_quarter_yoy_percent - q4_growth$value)) > tolerance_pp) {
      rlang::abort("Derived quarterly growth fails BCV reconciliation",
                   class = "gdp_reconciliation_growth_error")
    }

    # For incomplete years, compare only the same observed quarters in both years.
    # These are YTD growth estimates, not full-year estimates or forecasts.
    ytd_results[[length(ytd_results) + 1L]] <- q %>% group_by(family, series_label, year) %>%
      summarise(n_quarters = n(), through_quarter = max(quarter),
        inferred_ytd_growth = 100 * (sum(inferred_index) /
          sum(previous_year_quarter_index) - 1), .groups = "drop") %>%
      filter(n_quarters < 4L)
    reconstructed <- q %>% group_by(year) %>% summarise(
      n_quarters = n(), inferred_annual_index = sum(inferred_index),
      simple_average_yoy = mean(value), minimum_yoy = min(value), maximum_yoy = max(value),
      .groups = "drop"
    ) %>% filter(n_quarters == 4L) %>% arrange(year) %>% mutate(
      inferred_annual_growth = 100 * (inferred_annual_index /
        lag(inferred_annual_index, default = index_base_annual_total) - 1)
    )
    reconciled <- a %>% left_join(reconstructed, by = "year") %>% mutate(
      family = pair$family, residual_pp = inferred_annual_growth - value,
      simple_average_gap_pp = simple_average_yoy - value,
      outside_quarter_range = value < minimum_yoy - tolerance_pp |
        value > maximum_yoy + tolerance_pp
    )
    # Require exact agreement before publishing any derived dataset.
    if (anyNA(reconciled$residual_pp) || max(abs(reconciled$residual_pp)) > tolerance_pp ||
        any(reconciled$outside_quarter_range)) {
      rlang::abort("Annual and quarterly BCV rates cannot be reconciled within tolerance",
                   class = "gdp_reconciliation_growth_error")
    }
    q <- q %>% mutate(annual_source_url = first(a$source_url),
      annual_downloaded_at = max(a$downloaded_at),
      reconstruction_uses_provisional_data = any(a$provisional) || any(q$provisional))
    reconciled <- reconciled %>% mutate(quarterly_source_url = first(q$source_url),
      quarterly_downloaded_at = max(q$downloaded_at),
      reconstruction_uses_provisional_data = any(a$provisional) || any(q$provisional))

    # Fit only 2008-2012, then evaluate later years as an independent restriction check.
    training <- fit_data$year <= 2012L
    if (qr(design[training, , drop = FALSE])$rank != 3L) {
      rlang::abort("Early-window shares are not identified", class = "gdp_reconciliation_rank_error")
    }
    early_coefficients <- qr.solve(design[training, , drop = FALSE], target[training])
    early_shares <- c(early_coefficients, 1 - sum(early_coefficients))
    early_levels <- as.vector(as.matrix(fit_data[c("q1", "q2", "q3", "q4")]) %*% early_shares)
    early_growth <- 100 * (early_levels / c(1, head(early_levels, -1L)) - 1)
    reconciled <- reconciled %>% mutate(early_fit_residual_pp = early_growth - value)
    fit_results[[length(fit_results) + 1L]] <- tibble(
      family = pair$family, series_label = label, base_year = base_year,
      first_year = min(a$year), last_year = max(a$year), n_annual = nrow(a),
      q1_share = shares[1], q2_share = shares[2], q3_share = shares[3], q4_share = shares[4],
      positive_shares = all(shares > 0), design_condition_number = kappa(design, exact = TRUE),
      max_residual_pp = max(abs(reconciled$residual_pp)),
      max_holdout_residual_pp = max(abs(reconciled$early_fit_residual_pp[!training])),
      max_share_change = max(abs(early_shares - shares)),
      exact_within_tolerance = all(shares > 0) && max(abs(reconciled$residual_pp)) <= tolerance_pp,
      max_simple_average_gap_pp = max(abs(reconciled$simple_average_gap_pp)),
      annual_source_file = pair$annual_file, quarterly_source_file = pair$quarterly_file
    )
    annual_results[[length(annual_results) + 1L]] <- reconciled
    quarterly_results[[length(quarterly_results) + 1L]] <- q
  }
}

# Retain published rates and source provenance alongside every inferred observation.
fits <- bind_rows(fit_results)
annual_audit <- bind_rows(annual_results)
quarterly_audit <- bind_rows(quarterly_results)
write_csv(fits, file.path(output_dir, "real_10_gdp_reconciliation_fits.csv"))
write_csv(annual_audit, file.path(output_dir, "real_10_gdp_reconciliation_annual.csv"))
write_csv(quarterly_audit, file.path(output_dir, "real_10_gdp_reconciliation_quarterly.csv"))
write_csv(bind_rows(ytd_results), file.path(output_dir, "real_10_gdp_reconciliation_ytd.csv"))

# Export one row per series/period with explicit measure names and index conventions.
# Quarter weights use the full previous year; YTD uses only matching observed quarters.
quarterly_data <- quarterly_audit %>% transmute(
  dataset_id = "real_10_gdp_quarterly_bcv", source_id = "bcv", family, series_label,
  date, year, quarter, frequency = "quarterly", gdp_index = inferred_index,
  published_yoy_percent = value, reconstructed_yoy_percent = inferred_quarter_yoy_percent,
  qoq_unadjusted_percent = inferred_qoq_unadjusted_percent,
  rolling_four_quarter_index = inferred_rolling_four_quarter_index,
  rolling_four_quarter_yoy_percent = inferred_rolling_four_quarter_yoy_percent,
  ytd_yoy_percent = inferred_ytd_growth_percent, previous_year_annual_weight,
  index_base_year = base_year, index_base_annual_total = index_base_annual_total,
  price_base_year = price_base_year, seasonal_adjustment = "unadjusted",
  level_status = "inferred_from_published_growth", published_provisional = provisional,
  reconstruction_uses_provisional_data, annual_source_file, quarterly_source_file,
  annual_source_url, quarterly_source_url = source_url,
  annual_downloaded_at, quarterly_downloaded_at = downloaded_at, generated_at = generated_at
) %>% arrange(family, series_label, date)
annual_data <- annual_audit %>% transmute(
  dataset_id = "real_10_gdp_annual_bcv", source_id = "bcv", family, series_label,
  date, year, frequency = "annual", gdp_index = inferred_annual_index,
  published_yoy_percent = value, reconstructed_yoy_percent = inferred_annual_growth,
  reconciliation_residual_pp = residual_pp,
  index_base_year = base_year, index_base_annual_total = index_base_annual_total,
  price_base_year = price_base_year, seasonal_adjustment = "unadjusted",
  level_status = "inferred_from_published_growth", published_provisional = provisional,
  reconstruction_uses_provisional_data,
  annual_source_file = source_pairs$annual_file[match(family, source_pairs$family)],
  quarterly_source_file = source_pairs$quarterly_file[match(family, source_pairs$family)],
  annual_source_url = source_url, quarterly_source_url,
  annual_downloaded_at = downloaded_at, quarterly_downloaded_at, generated_at = generated_at
) %>% arrange(family, series_label, date)
write_final_dataset(quarterly_data, "real_10_gdp_quarterly_bcv")
write_final_dataset(annual_data, "real_10_gdp_annual_bcv")

# Historical published levels have a different price base and vintage; do not splice.
historical_path <- "data/processed/real_09_gdp_historical_bcv.csv"
if (file.exists(historical_path)) {
  historical <- read_csv(historical_path, show_col_types = FALSE) %>%
    filter(base_year == 1997L) %>%
    select(year, historical_level = value, historical_growth = growth_percent)
  overlap <- annual_audit %>% filter(family == "activity", series_label == "Consolidado") %>%
    inner_join(historical, by = "year") %>%
    mutate(price_base_vintage_gap_pp = value - historical_growth)
  write_csv(overlap, file.path(output_dir, "real_10_gdp_reconciliation_historical_overlap.csv"))
} else {
  message("Historical overlap audit not run: optional 1997-price level extract is absent.")
}
message("Generated ", nrow(quarterly_data), " quarterly and ", nrow(annual_data),
        " annual observations across ", nrow(fits), " reconciled series; diagnostics in ", output_dir, ".")
