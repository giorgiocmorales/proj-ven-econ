# Quarterly BCV year-over-year growth; run after real_03_gdp_q_a_bcv.R.
# The combined view compares unadjusted QoQ and rolling-four-quarter GDP growth.
library(tidyverse)

if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) stop("UTF-8 locale required")
}
source("R/report_figures.R", encoding = "UTF-8")
source("R/report_inputs.R")
# Include only quarters that have ended by the common report cutoff.
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
cutoff <- as.Date(config$params$data_cutoff)

source_file <- "data/final/real_03_gdp_q_a_bcv.csv"
quarterly <- readr::read_csv(source_file, show_col_types = FALSE) %>%
  filter(series_label == "Consolidado", frequency == "quarterly", unit == "percent_yoy",
         lubridate::ceiling_date(date, "quarter") - lubridate::days(1) <= cutoff) %>%
  transmute(date, year = as.integer(year), quarter = as.integer(quarter),
            growth = value, provisional, source_id, source_file = source_file,
            source_detail = source_url, downloaded_at) %>% arrange(date)
if (!nrow(quarterly) || anyNA(quarterly$date) || any(!is.finite(quarterly$growth)) ||
    anyDuplicated(quarterly$date) || anyNA(quarterly$quarter) ||
    any(!quarterly$quarter %in% 1:4) ||
    any(quarterly$date != lubridate::make_date(quarterly$year, 3L * quarterly$quarter - 2L, 1L))) {
  stop("Quarterly GDP requires unique, finite observations with consistent quarter dates")
}
# Missing quarters fail explicitly; no zero filling or interpolation.
if (!identical(quarterly$date, seq(min(quarterly$date), max(quarterly$date), by = "3 months"))) {
  stop("Quarterly GDP has internal coverage gaps; review the source extract")
}
quarterly <- quarterly %>% mutate(phase = factor(case_when(
  growth > 0 ~ "Expansión", growth < 0 ~ "Recesión", TRUE ~ "Sin variación"
), levels = c("Expansión", "Recesión", "Sin variación")))
quarter_label <- function(x) paste0(lubridate::year(x), " T", lubridate::quarter(x))
recent_start <- as.Date("2019-01-01")
recent <- quarterly %>% filter(date >= recent_start)
if (!nrow(recent)) stop("No quarterly GDP observations in the recent window")
provisional_note <- if (any(quarterly$provisional %in% TRUE)) {
  "Notas: incluye cifras provisionales según el extracto BCV."
} else "Notas: clasificación provisional según el extracto BCV."

growth_style <- list(

  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid"),
  scale_y_continuous(
    labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
    expand = expansion(mult = 0)
  ),
  report_chart_theme
)
build_quarterly <- function(data, every_quarter) {
  breaks <- if (every_quarter) data$date else sort(unique(c(
    min(data$date), data$date[data$quarter == 1L], max(data$date))))
  ggplot(data, aes(date, growth, fill = phase)) +
    geom_col(width = 66) + growth_style +
    scale_fill_manual(values = c("Expansión" = report_palette[["navy"]],
      "Recesión" = report_palette[["red"]], "Sin variación" = "#b8c1c8")) +
    scale_x_date(breaks = breaks, labels = quarter_label,
                 expand = expansion(add = 65)) +
    labs(title = "Venezuela: crecimiento trimestral del PIB real",
      subtitle = paste0("Variación respecto al mismo trimestre del año anterior (%) | ",
        quarter_label(min(data$date)), "–", quarter_label(max(data$date))),
      x = NULL, y = NULL,
      caption = paste(provisional_note,
                      "Fuentes: BCV; PIB consolidado por actividad económica.", sep = "\n")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
figures <- list(real_gdp_growth_quarterly = build_quarterly(quarterly, FALSE),
                real_gdp_growth_quarterly_recent = build_quarterly(recent, TRUE))

# Use inferred levels to compare the latest four quarters with the preceding four.
# Preserve the published quarterly rates and verify the annual benchmark at each Q4.
reconciled_file <- "data/final/real_10_gdp_quarterly_bcv.csv"
reconciled <- readr::read_csv(reconciled_file, show_col_types = FALSE) %>%
  filter(family == "activity", series_label == "Consolidado",
         date >= recent_start,
         lubridate::ceiling_date(date, "quarter") - lubridate::days(1) <= cutoff) %>%
  select(date, published_yoy_percent, qoq_unadjusted_percent, rolling_four_quarter_yoy_percent,
         annual_source_file, quarterly_source_file, reconstruction_uses_provisional_data)
if (anyDuplicated(reconciled$date) || !setequal(recent$date, reconciled$date)) {
  rlang::abort("Reconciled GDP must cover every plotted quarter exactly once",
               class = "gdp_graph_reconciliation_error")
}
combined <- recent %>% left_join(reconciled, by = "date")
if (any(!is.finite(combined$qoq_unadjusted_percent)) ||
    any(!is.finite(combined$rolling_four_quarter_yoy_percent)) ||
    anyNA(combined$published_yoy_percent) ||
    any(abs(combined$growth - combined$published_yoy_percent) > 1e-6)) {
  rlang::abort("Reconciled GDP is missing or differs from the published quarterly rates",
               class = "gdp_graph_reconciliation_error")
}
annual_file <- "data/final/real_01_gdp_a_a_bcv.csv"
annual <- readr::read_csv(annual_file, show_col_types = FALSE) %>%
  filter(series_label == "Consolidado", frequency == "annual", unit == "percent_yoy",
         year >= lubridate::year(recent_start), year <= max(combined$year),
         lubridate::make_date(year, 12L, 31L) <= cutoff) %>%
  transmute(year = as.integer(year), annual_growth = value)
annual_check <- combined %>% filter(quarter == 4L) %>% left_join(annual, by = "year")
if (!nrow(annual_check) || anyDuplicated(annual$year) ||
    !setequal(annual$year, annual_check$year) || any(!is.finite(annual_check$annual_growth)) ||
    any(abs(annual_check$rolling_four_quarter_yoy_percent - annual_check$annual_growth) > 1e-6)) {
  rlang::abort("Rolling-four-quarter growth must match every published annual benchmark",
               class = "gdp_graph_reconciliation_error")
}

# Retain exact plotted values and input provenance in a single auditable long export.
comparison <- combined %>%
  rename(published_quarter_yoy_percent = growth) %>%
  pivot_longer(cols = c(qoq_unadjusted_percent, rolling_four_quarter_yoy_percent),
               names_to = "measure", values_to = "growth") %>%
  mutate(series = if_else(measure == "qoq_unadjusted_percent", "Trimestral (t/t)",
                         "Últimos cuatro trimestres (a/a)"),
         source_file = reconciled_file)

# Bars show unadjusted adjacent-quarter growth; the line retains four-quarter YoY growth.
# Calendar-year ticks stay horizontal; the last tick identifies the latest observed quarter.
last_date <- max(combined$date)
year_breaks <- combined$date[combined$quarter == 1L &
                              combined$date <= last_date - lubridate::days(180)]
figures$real_gdp_growth_annual_quarterly <- ggplot(comparison, aes(date, growth)) +
  geom_col(data = comparison %>% filter(measure == "qoq_unadjusted_percent"),
           aes(fill = series), width = 66) +
  geom_line(data = comparison %>% filter(measure == "rolling_four_quarter_yoy_percent"),
            aes(color = series), linewidth = 0.9) +
  growth_style +
  scale_fill_manual(values = c("Trimestral (t/t)" = report_palette[["navy"]])) +
  scale_color_manual(values = c("Últimos cuatro trimestres (a/a)" = report_palette[["yellow"]])) +
  scale_x_date(breaks = sort(unique(c(year_breaks, last_date))),
               labels = function(x) if_else(x == last_date, quarter_label(x),
                                            as.character(lubridate::year(x))),
               expand = expansion(add = 65)) +
  labs(title = "Venezuela: crecimiento del PIB real",
    subtitle = paste0("Variaciones porcentuales, sin desestacionalizar | ", quarter_label(min(combined$date)),
                      "–", quarter_label(last_date)),
    x = NULL, y = NULL,
    caption = paste(
      "Notas: barras: trimestre frente al anterior; línea: últimos cuatro trimestres frente a los cuatro anteriores.",
      "En T4, la línea coincide con la tasa anual del BCV. Cálculo con niveles reconciliados; incluye cifras provisionales.",
      "Fuentes: BCV; cálculos propios.", sep = "\n"))
readr::write_csv(comparison, "outputs/data/real_91_gdp_comparison_graph.csv")
dir.create("outputs/data", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(quarterly, "outputs/data/real_91_gdp_quarterly_graph.csv")
for (figure_name in names(figures)) {
  ggsave(paste0("outputs/figures/", figure_name, ".png"), figures[[figure_name]],
    width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", figure_name, ".pdf"), figures[[figure_name]],
    width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
}
message("Validated ", nrow(quarterly), " quarterly observations; exported ", length(figures), " chart variants.")
