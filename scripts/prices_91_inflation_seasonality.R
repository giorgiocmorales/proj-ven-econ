# Describe annual inflation peaks and calendar patterns without pooling nominal inflation regimes.
# Run separately from source refreshes: Rscript scripts/prices_91_inflation_seasonality.R
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

# Use the labelled historical reference and the full native Caracas series as a sensitivity.
history_path <- "data/final/prices_08_inflation_historical_bcv.csv"
caracas_path <- "data/final/prices_08_inflation_caracas_bcv.csv"
output_dir <- "outputs/analysis/inflation_seasonality"
peak_tolerance_pp <- 1e-8
reference <- read_csv(history_path, show_col_types = FALSE) %>%
  mutate(sample_id = "historical_reference")
caracas <- read_csv(caracas_path, show_col_types = FALSE) %>%
  filter(variable_id == "inflation_mom") %>% mutate(sample_id = "caracas_only")
observations <- bind_rows(reference, caracas) %>% arrange(sample_id, date) %>%
  mutate(year = year(date), month_num = month(date), month_name = month.name[month_num])
if (anyNA(observations$date) || anyDuplicated(observations[c("sample_id", "date")]) ||
    any(!is.finite(observations$value) | observations$value <= -100) ||
    any(observations$variable_id != "inflation_mom" | observations$unit != "percent_mom")) {
  rlang::abort("Seasonality requires unique valid monthly inflation observations",
    class = "inflation_seasonality_input_error")
}

# Exclude incomplete years from every twelve-month comparison; preserve their coverage.
coverage <- observations %>% group_by(sample_id, year) %>%
  summarise(months = n_distinct(month_num), first_month = min(month_num),
    last_month = max(month_num), complete = months == 12L, .groups = "drop")
complete <- observations %>% inner_join(coverage %>% filter(complete) %>% select(sample_id, year),
  by = c("sample_id", "year"), relationship = "many-to-one") %>%
  group_by(sample_id, year) %>% mutate(
    within_year_rank = rank(value, ties.method = "average"),
    is_peak = abs(value - max(value)) <= peak_tolerance_pp,
    peak_ties = sum(is_peak), fractional_peak = as.numeric(is_peak) / peak_ties,
    log_change = 100 * log1p(value / 100)) %>% ungroup()
if (!nrow(complete) || any((complete %>% count(sample_id, year))$n != 12L)) {
  rlang::abort("No valid complete-year panels for seasonality",
    class = "inflation_seasonality_coverage_error")
}

# Distinguish how often a month is the maximum from how high it typically ranks.
# Full counts include tied peaks; fractional counts allocate one peak across ties.
periods <- tibble::tribble(
  ~period_id, ~period_label, ~sample_id, ~first_year, ~last_year,
  "full_history", "Historical reference 1951-2025", "historical_reference", 1951L, 2025L,
  "caracas_pre_2008", "Caracas 1951-2007 (57 years)", "historical_reference", 1951L, 2007L,
  "national", "National 2008-2025", "historical_reference", 2008L, 2025L,
  "national_early", "National 2008-2016 (9 years)", "historical_reference", 2008L, 2016L,
  "national_2017_2021", "National 2017-2021 (5 years)", "historical_reference", 2017L, 2021L,
  "national_recent", "National 2022-2025 (4 years)", "historical_reference", 2022L, 2025L,
  "caracas_sensitivity", "Caracas only 1951-2025", "caracas_only", 1951L, 2025L)
summaries <- list()
tests <- list()
for (i in seq_len(nrow(periods))) {
  setting <- periods[i, ]
  panel <- complete %>% filter(sample_id == setting$sample_id,
    between(year, setting$first_year, setting$last_year))
  n_years <- n_distinct(panel$year)
  if (n_years != setting$last_year - setting$first_year + 1L) {
    rlang::abort(paste("Incomplete analysis period:", setting$period_id),
      class = "inflation_seasonality_coverage_error")
  }
  summaries[[setting$period_id]] <- panel %>% group_by(month_num, month_name) %>%
    summarise(peak_years_including_ties = sum(is_peak), fractional_peak_years = sum(fractional_peak),
      mean_within_year_rank = mean(within_year_rank), median_inflation_pct = median(value),
      mean_inflation_pct = mean(value), .groups = "drop") %>%
    mutate(period_id = setting$period_id, period_label = setting$period_label,
      sample_id = setting$sample_id, n_years = n_years,
      peak_years_pct = 100 * peak_years_including_ties / n_years,
      fractional_peak_pct = 100 * fractional_peak_years / n_years, .before = 1)

  # Friedman tests calendar ranks with years as blocks, avoiding inflation-level pooling.
  # These are exploratory associations, not proof of forecast value or causal seasonality.
  friedman <- stats::friedman.test(panel$value, panel$month_num, panel$year)
  # A sensitivity adds calendar effects after year-specific levels and linear trends.
  # Its conventional OLS F test assumes independent homoskedastic errors.
  trend_model <- lm(log_change ~ factor(year) * month_num, data = panel)
  calendar_model <- update(trend_model, . ~ . + factor(month_num))
  trend_test <- anova(trend_model, calendar_model)
  tests[[setting$period_id]] <- tibble(period_id = setting$period_id, sample_id = setting$sample_id,
    n_years = n_years, friedman_chisq = as.numeric(friedman$statistic),
    friedman_df = as.numeric(friedman$parameter), friedman_p = friedman$p.value,
    kendall_w = as.numeric(friedman$statistic) / (n_years * 11),
    trend_adjusted_f = trend_test$F[2], trend_adjusted_df = trend_test$Df[2],
    trend_adjusted_p = trend_test$`Pr(>F)`[2])
}
monthly_summary <- bind_rows(summaries)
seasonality_tests <- bind_rows(tests)
annual_peaks <- complete %>% filter(sample_id == "historical_reference", is_peak) %>%
  arrange(year, month_num) %>% group_by(year) %>% summarise(
    peak_months = paste(month_name, collapse = " / "), inflation_pct = max(value),
    tied_months = n(), geography = first(geography), .groups = "drop")

# Reconcile counting denominators and fractional ties before publishing the analysis.
count_checks <- monthly_summary %>% group_by(period_id) %>%
  summarise(months = n(), fractional_peaks = sum(fractional_peak_years), years = first(n_years),
    fractional_pct = sum(fractional_peak_pct), .groups = "drop")
if (any(count_checks$months != 12L) || any(abs(count_checks$fractional_peaks - count_checks$years) > 1e-10) ||
    any(abs(count_checks$fractional_pct - 100) > 1e-10) || nrow(annual_peaks) != 75L ||
    any(!between(monthly_summary$mean_within_year_rank, 1, 12))) {
  rlang::abort("Seasonality counts, ranks or tie allocations failed validation",
    class = "inflation_seasonality_output_error")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(annual_peaks, file.path(output_dir, "annual_peak_months.csv"))
write_csv(monthly_summary, file.path(output_dir, "monthly_summary.csv"))
write_csv(seasonality_tests, file.path(output_dir, "seasonality_tests.csv"))
write_csv(coverage, file.path(output_dir, "year_coverage.csv"))
write_csv(complete, file.path(output_dir, "within_year_observations.csv"))
jsonlite::write_json(list(source_paths = c(history_path, caracas_path),
  source_md5 = unname(tools::md5sum(c(history_path, caracas_path))),
  included_years = "1951-2025: 75 complete years", excluded_years = c("1950: January missing", "2026: incomplete"),
  geography = "Caracas before 2008; national from 2008; Caracas-only sensitivity retained",
  peak_definition = "Largest monthly CPI percentage change within each complete year",
  peak_tolerance_pp = peak_tolerance_pp,
  ties = "Full counts include each tied month; fractional counts divide one peak among ties",
  seasonality = "Friedman ranks with year blocks; Kendall W measures rank concordance, not variance explained",
  caveats = c("Calendar associations do not establish out-of-sample forecast gains",
    "Annual inflation trends and serial dependence can affect exploratory test inference",
    "Recent 2022-2025 results have only four independent annual blocks",
    "Trend sensitivity uses conventional independent-error OLS inference")),
  file.path(output_dir, "analysis_metadata.json"), auto_unbox = TRUE, pretty = TRUE)

# Plot ranks so a hyperinflation year cannot dominate a period's seasonal profile.
plot_periods <- c("caracas_pre_2008", "national_early", "national_2017_2021", "national_recent")
plot_data <- monthly_summary %>% filter(period_id %in% plot_periods) %>%
  mutate(period_label = factor(period_label, levels = periods$period_label[match(plot_periods, periods$period_id)]))
figure <- ggplot(plot_data, aes(month_num, mean_within_year_rank)) +
  geom_hline(yintercept = 6.5, color = "#687179", linewidth = 0.3, linetype = "dashed") +
  geom_line(color = "#17365d", linewidth = 0.8) +
  facet_wrap(~period_label, ncol = 2L) +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(1, 12), breaks = c(1, 3, 6.5, 9, 12), expand = expansion(mult = 0)) +
  labs(title = "Inflation calendar patterns change across periods",
    subtitle = "Average rank within each year: 1 = lowest monthly inflation, 12 = highest; dashed line = 6.5",
    x = NULL, y = "Average within-year rank",
    caption = "Source: BCV Caracas and national CPI histories. Complete years only; 1950 and 2026 excluded.\nRanks show relative timing, not inflation rates or guaranteed seasonal forecasting effects.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), axis.ticks = element_blank(),
    axis.line = element_line(color = "#2b3033", linewidth = 0.35),
    plot.caption = element_text(hjust = 0), panel.spacing = grid::unit(1.3, "lines"))
ggsave(file.path(output_dir, "monthly_rank_patterns.png"), figure, width = 12, height = 7,
  units = "in", dpi = 200, device = ragg::agg_png, bg = "white")
ggsave(file.path(output_dir, "monthly_rank_patterns.pdf"), figure, width = 12, height = 7,
  units = "in", device = grDevices::cairo_pdf, bg = "white")
print(monthly_summary %>% filter(period_id == "full_history") %>%
  arrange(desc(peak_years_including_ties)) %>% select(month_name, peak_years_including_ties, peak_years_pct))
print(seasonality_tests)
