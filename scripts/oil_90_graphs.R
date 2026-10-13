# Compare monthly Venezuelan crude production sources and spot crude prices.
# Run after oil_01_prod_opec.R and oil_02_price_opec.R; use their latest revisions.
library(tidyverse)

rm(list = ls())

# Reuse the approved chart styling and render Spanish text consistently on Windows.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("A UTF-8 locale is required for chart labels", class = "oil_graph_locale_error")
  }
}
source("R/report_figures.R", encoding = "UTF-8")
source("R/report_inputs.R")
# Apply the same completed-month cutoff as every other report chart.
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
cutoff <- as.Date(config$params$data_cutoff)

# Use the full monthly history recovered from the report corpus.
# Read monthly observations only: annual averages and year-to-date rows are excluded.
production <- readr::read_csv("data/final/oil_01_prod_opec_latest.csv", show_col_types = FALSE,
                              col_types = cols(footnote_marker = col_character())) %>%
  filter(.data$date <= cutoff, .data$country == "Venezuela", .data$period_type == "monthly",
         .data$source_type %in% c("direct_communication", "secondary_sources")) %>%
  mutate(series = factor(.data$source_type,
    levels = c("direct_communication", "secondary_sources"),
    labels = c("Comunicaci\u00f3n directa", "Fuentes secundarias"))) %>%
  arrange(.data$series, .data$date)

# North Sea Dated is the spot benchmark in the OPEC table, not Brent futures.
prices <- readr::read_csv("data/final/oil_02_price_opec_latest.csv", show_col_types = FALSE,
                          col_types = cols(footnote_marker = col_character())) %>%
  filter(.data$date <= cutoff, .data$period_type == "monthly", .data$variable_id %in% c("wti", "north_sea_dated", "merey")) %>%
  mutate(series = factor(.data$variable_id,
    levels = c("wti", "north_sea_dated", "merey"),
    labels = c("WTI", "Brent", "Merey"))) %>%
  arrange(.data$series, .data$date)

# Require the requested series, units, valid dates, finite values, and unique keys.
if (n_distinct(production$series) != 2L || n_distinct(prices$series) != 3L ||
    !inherits(production$date, "Date") || !inherits(prices$date, "Date") ||
    anyNA(production$date) || anyNA(prices$date) ||
    any(is.infinite(production$value)) || any(production$value <= 0, na.rm = TRUE) ||
    any(is.infinite(prices$value)) ||
    anyNA(production$unit) || anyNA(prices$unit) ||
    any(production$unit != "tb/d") || any(prices$unit != "USD/b") ||
    anyDuplicated(production[c("series", "date")]) > 0 ||
    anyDuplicated(prices[c("series", "date")]) > 0) {
  rlang::abort("Oil charts require complete series with valid units, dates, values and unique keys",
               class = "oil_graph_data_error")
}

# Measure each series' own coverage; membership and benchmark history differ.
coverage <- bind_rows(
  production %>% transmute(chart = "production", series, date, value),
  prices %>% transmute(chart = "prices", series, date, value)
) %>%
  group_by(.data$chart, .data$series) %>%
  summarise(first_date = min(.data$date), last_date = max(.data$date), observations = n(),
            reported = sum(!is.na(.data$value)),
            expected = length(seq(min(.data$date), max(.data$date), by = "month")), .groups = "drop")
if (any(coverage$reported < 8L)) {
  rlang::abort("Oil chart series have fewer than eight reported months",
               class = "oil_graph_coverage_error")
}

# Insert missing months so lines visibly break at gaps without interpolation.
production <- production %>% group_by(.data$series) %>%
  complete(date = seq(min(.data$date), max(.data$date), by = "month")) %>% ungroup()
prices <- prices %>% group_by(.data$series) %>%
  complete(date = seq(min(.data$date), max(.data$date), by = "month")) %>% ungroup()

# Compare production estimates on one scale using solid lines and the approved palette.
production_figure <- production %>%
  ggplot(aes(x = .data$date, y = .data$value, color = .data$series)) +
  geom_line(linewidth = 0.7, linetype = "solid") +
  scale_color_manual(values = unname(report_palette[c("navy", "red")])) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), max(production$date), by = "5 years"), date_labels = "%Y") +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
    expand = expansion(mult = 0)
  ) +
  labs(title = "Venezuela: producci\u00f3n de petr\u00f3leo",
       subtitle = paste0("Miles de barriles diarios | ", format(min(production$date), "%m/%Y"),
                         "\u2013", format(max(production$date), "%m/%Y")),
       x = NULL, y = NULL,
       caption = "Notas: \u00faltima revisi\u00f3n disponible por mes.\nFuentes: OPEP, informes mensuales del mercado petrolero.") +
  report_chart_theme

# Compare like-frequency spot benchmarks, preserving each crude's published identity.
prices_figure <- prices %>%
  ggplot(aes(x = .data$date, y = .data$value, color = .data$series)) +
  geom_line(linewidth = 0.7, linetype = "solid") +
  scale_color_manual(values = c(WTI = report_palette[["cyan"]],
                                Brent = report_palette[["navy"]],
                                Merey = report_palette[["red"]])) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), max(prices$date), by = "5 years"), date_labels = "%Y") +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
    expand = expansion(mult = 0)
  ) +
  labs(title = "Precios del petr\u00f3leo: WTI, Brent y Merey",
       subtitle = paste0("Promedios mensuales, USD por barril | ", format(min(prices$date), "%m/%Y"),
                         "\u2013", format(max(prices$date), "%m/%Y")),
       x = NULL, y = NULL,
       caption = "Notas: Brent corresponde a North Sea Dated.\nFuentes: OPEP, informes mensuales del mercado petrolero.") +
  report_chart_theme

# Export the observations, coverage audit, and matching PNG/PDF assets for both graphs.
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(production, "data/final/oil_90_production_graph.csv")
readr::write_csv(prices, "data/final/oil_90_prices_graph.csv")
readr::write_csv(coverage, "data/final/oil_90_graph_coverage.csv")
oil_figures <- list(oil_production_sources = production_figure, oil_prices_benchmarks = prices_figure)
for (figure_name in names(oil_figures)) {
  ggsave(paste0("outputs/figures/", figure_name, ".png"), plot = oil_figures[[figure_name]],
         width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", figure_name, ".pdf"), plot = oil_figures[[figure_name]],
         width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
}
