# Standalone monetary-base and liquidity charts from harmonized BCV outputs.
# Preserve weekly stocks; use the compounded daily CPI estimate on each observation date.
library(tidyverse)
Sys.setlocale("LC_CTYPE", ".UTF-8")
source("R/report_figures.R", encoding = "UTF-8")
source("R/report_inputs.R")
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
cutoff <- as.Date(config$params$data_cutoff)
history_start <- as.Date("2020-01-01")

base <- readr::read_csv("data/final/monetary_01_mb_bcv.csv", show_col_types = FALSE) %>%
  filter(variable_id == "monetary_base") %>%
  select(date, variable_id, value, unit, currency, provisional, source_url, downloaded_at)
liquidity <- readr::read_csv("data/final/monetary_02_liq_bcv.csv", show_col_types = FALSE) %>%
  filter(variable_id == "liquidez_monetaria") %>%
  select(date, variable_id, value, unit, currency, provisional, source_url, downloaded_at)
weekly <- bind_rows(base, liquidity) %>% filter(date <= cutoff)
if (!setequal(unique(weekly$variable_id), c("monetary_base", "liquidez_monetaria")) ||
    anyDuplicated(weekly[c("variable_id", "date")]) || anyNA(weekly$date) ||
    any(!is.finite(weekly$value) | weekly$value <= 0) ||
    anyNA(weekly$unit) || any(weekly$unit != "current_VES") ||
    anyNA(weekly$currency) || any(weekly$currency != "VES_current")) {
  stop("Monetary charts require unique positive stocks in current-denomination VES")
}

# Estimated daily values preserve published monthly changes; no extrapolation.
cpi_path <- "data/final/prices_05_daily_cpi_bcv.csv"
cpi <- readr::read_csv(cpi_path, show_col_types = FALSE) %>% filter(date <= cutoff)
if (anyDuplicated(cpi$date) || anyNA(cpi$date) ||
    any(!is.finite(cpi$daily_cpi_estimated) | cpi$daily_cpi_estimated <= 0)) {
  stop("Invalid estimated daily CPI deflator; run prices_05_daily_cpi_bcv.R")
}

weekly <- weekly %>% rename(nominal_ves = value) %>%
  mutate(month = floor_date(date, "month"), week = floor_date(date, "week", week_start = 1)) %>%
  left_join(cpi %>% select(date, cpi_index = daily_cpi_estimated, cpi_provisional = provisional),
    by = "date") %>% arrange(variable_id, date) %>% group_by(variable_id) %>%
  mutate(previous_observation_date = lag(date),
    interval_days = as.integer(date - previous_observation_date),
    inflation_since_previous_pct = 100 * (cpi_index / lag(cpi_index) - 1)) %>% ungroup()
common_dates <- weekly %>% filter(is.finite(cpi_index)) %>%
  count(date) %>% filter(n == 2L)
if (!nrow(common_dates)) stop("No weekly dates shared by both monetary series and CPI")
last_date <- max(common_dates$date)
reference_date <- floor_date(last_date, "month")
reference_cpi <- cpi %>% filter(month == reference_date) %>% slice_head(n = 1) %>% pull(cpi_index)
eligible <- weekly %>% filter(date <= last_date)
# Match dates 364 days apart, retaining holiday and additional closing observations.
# If the target date was not reported, use only a preceding observation within seven days.
prior_year <- weekly %>% transmute(variable_id,
  prior_year_nominal_ves = nominal_ves, prior_year_observation_date = date)
aggregates <- eligible %>% mutate(comparison_target_date = date - days(364)) %>%
  left_join(prior_year, by = join_by(variable_id, closest(comparison_target_date >= prior_year_observation_date)),
    relationship = "many-to-one") %>%
  mutate(comparison_offset_days = as.integer(comparison_target_date - prior_year_observation_date),
    prior_year_nominal_ves = if_else(comparison_offset_days <= 7L, prior_year_nominal_ves, NA_real_)) %>%
  mutate(real_ves = nominal_ves * reference_cpi / cpi_index,
    nominal_growth_yoy = 100 * (nominal_ves / prior_year_nominal_ves - 1),
    cpi_reference_date = reference_date, cpi_reference_value = reference_cpi,
    cpi_source_file = cpi_path, source_id = "bcv", frequency = "weekly",
    series = factor(variable_id, levels = c("monetary_base", "liquidez_monetaria"),
                    labels = c("Base monetaria", "Liquidez monetaria")))
chart_data <- aggregates %>% filter(date >= history_start)
for (id in c("monetary_base", "liquidez_monetaria")) {
  dates <- sort(chart_data$date[chart_data$variable_id == id])
  if (any(as.integer(diff(dates)) > 14L)) {
    stop("Displayed monetary history has a gap longer than two weeks")
  }
}
if (any(!is.finite(chart_data$real_ves)) || any(!is.finite(chart_data$nominal_growth_yoy))) {
  stop("Charts require complete CPI and prior-year monetary comparisons")
}
readr::write_csv(aggregates, "data/final/monetary_92_aggregates.csv")
readr::write_csv(weekly %>% select(variable_id, date, week, month, cpi_index,
  previous_observation_date, interval_days, inflation_since_previous_pct),
  "data/final/monetary_92_weekly_coverage.csv")
# Count the days contributed by each month to every displayed observation interval.
# The interval is (previous observation, current observation], avoiding double counting.
weekly_inflation_parts <- chart_data %>%
  select(variable_id, observation_date = date, previous_observation_date, interval_days,
         inflation_since_previous_pct) %>%
  inner_join(cpi %>% select(day = date, month, daily_log_factor),
    by = join_by(previous_observation_date < day, observation_date >= day),
    relationship = "many-to-many") %>%
  group_by(variable_id, observation_date, previous_observation_date, interval_days,
           inflation_since_previous_pct, month) %>%
  summarise(days_from_month = n(), month_log_contribution = sum(daily_log_factor), .groups = "drop")
weekly_inflation_check <- weekly_inflation_parts %>%
  group_by(variable_id, observation_date, previous_observation_date, interval_days, inflation_since_previous_pct) %>%
  summarise(days_counted = sum(days_from_month), months_spanned = n(),
    compounded_inflation_pct = 100 * expm1(sum(month_log_contribution)), .groups = "drop")
if (nrow(weekly_inflation_check) != nrow(chart_data) ||
    any(weekly_inflation_check$days_counted != weekly_inflation_check$interval_days) ||
    any(abs(weekly_inflation_check$compounded_inflation_pct -
            weekly_inflation_check$inflation_since_previous_pct) > 1e-8)) {
  stop("Daily factors do not reconcile with weekly CPI changes")
}
readr::write_csv(weekly_inflation_parts, "data/final/monetary_92_weekly_inflation_parts.csv")
readr::write_csv(weekly_inflation_check, "data/final/monetary_92_weekly_inflation.csv")
real_breaks <- pretty(c(0, chart_data$real_ves / 1e12), n = 7)
growth_breaks <- pretty(c(0, chart_data$nominal_growth_yoy * 1.03), n = 7)
real_figure <- chart_data %>%
  ggplot(aes(date, real_ves / 1e12, color = series)) + geom_line(linewidth = 0.7) +
  scale_color_manual(values = unname(report_palette[c("navy", "red")])) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(limits = range(real_breaks), breaks = real_breaks, expand = expansion(mult = 0),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_x_date(breaks = sort(unique(c(seq(history_start, last_date, by = "year"), last_date))),
    date_labels = "%m/%Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Venezuela: base monetaria y liquidez en términos reales",
    subtitle = paste0("Billones de bolívares (10¹²), a precios de ", format(reference_date, "%m/%Y"),
      " | ", format(min(chart_data$date), "%m/%Y"), "–", format(last_date, "%m/%Y")),
    x = NULL, y = NULL,
    caption = paste0("Notas: IPC diario estimado por capitalización constante dentro de cada mes; índices mensuales anclados al cierre.\n",
      "Fuentes: BCV; saldos homologados por reconversión y deflactados con el INPC general.")) +
  report_chart_theme + guides(color = guide_legend(nrow = 1))
growth_figure <- chart_data %>%
  ggplot(aes(date, nominal_growth_yoy, color = series)) + geom_line(linewidth = 0.7) +
  scale_color_manual(values = unname(report_palette[c("navy", "red")])) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(limits = range(growth_breaks), breaks = growth_breaks, expand = expansion(mult = 0),
    labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ",")) +
  scale_x_date(breaks = sort(unique(c(seq(history_start, last_date, by = "year"), last_date))),
    date_labels = "%m/%Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Venezuela: crecimiento de la base monetaria y la liquidez",
    subtitle = paste0("Variación de saldos nominales en 52 semanas (%) | ",
      format(min(chart_data$date), "%m/%Y"), "–", format(last_date, "%m/%Y")),
    x = NULL, y = NULL,
    caption = paste0("Notas: comparación a 364 días; si falta esa fecha, última observación anterior (máximo 7 días). Cifras provisionales.\n",
      "Fuentes: BCV; cálculos sobre saldos nominales homologados por reconversión.")) +
  report_chart_theme + guides(color = guide_legend(nrow = 1))
figures <- list(monetary_base_liquidity_real = real_figure, monetary_base_liquidity_growth_yoy = growth_figure)
for (name in names(figures)) {
  ggsave(paste0("outputs/figures/", name, ".png"), figures[[name]], width = 10, height = 5.625,
    units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", name, ".pdf"), figures[[name]], width = 10, height = 5.625,
    units = "in", device = grDevices::cairo_pdf, bg = "white")
}
print(aggregates %>% filter(date == last_date) %>%
  select(date, prior_year_observation_date, series, nominal_ves, real_ves, nominal_growth_yoy))
