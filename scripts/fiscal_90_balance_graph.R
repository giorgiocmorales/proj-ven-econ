# Annual fiscal chart from finalized IMF components; expenditure signs are display-only.
library(tidyverse)
Sys.setlocale("LC_CTYPE", ".UTF-8")
source("R/report_figures.R", encoding = "UTF-8")
config <- yaml::read_yaml(Sys.getenv("REPORT_CONFIG", "config/report.yml"))
first_year <- year(as.Date(if (is.null(config$params$fiscal_history_start))
  config$params$history_start else config$params$fiscal_history_start))
last_year <- year(as.Date(config$params$data_cutoff)) - 1L
fiscal_source <- readr::read_csv("data/processed/fiscal_05_components_imf.csv", show_col_types = FALSE) %>%
  filter(COUNTRY == "VEN", FREQUENCY == "A", INDICATOR %in% c("GGR_NGDP", "GGX_NGDP", "GGXCNL_NGDP")) %>%
  mutate(year = as.integer(TIME_PERIOD))
if (anyDuplicated(fiscal_source[c("year", "INDICATOR")]) ||
    any(!is.finite(fiscal_source$OBS_VALUE))) stop("Invalid IMF fiscal component data")
if (n_distinct(fiscal_source$COUNTRY_UPDATE_DATE) != 1L) stop("IMF fiscal components have different update dates")
fiscal_data <- fiscal_source %>% select(year, INDICATOR, OBS_VALUE) %>%
  pivot_wider(names_from = INDICATOR, values_from = OBS_VALUE) %>%
  rename(revenue = GGR_NGDP, expenditure = GGX_NGDP, balance = GGXCNL_NGDP) %>%
  arrange(year) %>% mutate(reconciliation_pp = revenue - expenditure - balance)
if (any(!complete.cases(fiscal_data)) || any(fiscal_data$revenue < 0 | fiscal_data$expenditure < 0) ||
    any(abs(fiscal_data$reconciliation_pp) > 0.01)) {
  stop("Revenue minus expenditure must reconcile with IMF net lending/borrowing within 0.01 pp")
}
fiscal_data <- fiscal_data %>% mutate(source_id = "imf", unit = "percent_gdp",
  source_file = "data/processed/fiscal_05_components_imf.csv",
  source_update_date = first(fiscal_source$COUNTRY_UPDATE_DATE),
  downloaded_at = first(fiscal_source$downloaded_at))
readr::write_csv(fiscal_data, "data/final/fiscal_05_components_imf.csv")
plotted <- fiscal_data %>% filter(between(year, first_year, last_year))
if (!nrow(plotted) || !identical(plotted$year, seq.int(min(plotted$year), max(plotted$year)))) {
  stop("Fiscal chart needs consecutive annual observations")
}
bars <- plotted %>% select(year, revenue, expenditure) %>%
  pivot_longer(-year, names_to = "component", values_to = "value") %>%
  mutate(value = if_else(component == "expenditure", -value, value),
    component = factor(component, levels = c("revenue", "expenditure"), labels = c("Ingresos", "Gastos")))
# One explicit tick sequence determines both limits and labels.
y_breaks <- pretty(c(bars$value, plotted$balance, 0), n = 7)
fiscal_figure <- bars %>%
  ggplot(aes(year, value)) +
  geom_col(aes(fill = component), width = 0.65, position = "identity") +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  geom_line(data = plotted, aes(year, balance, color = "Balance fiscal"), linewidth = 0.85) +
  geom_point(data = plotted, aes(year, balance, color = "Balance fiscal"), size = 1.8) +
  scale_fill_manual(values = c("Ingresos" = report_palette[["navy"]], "Gastos" = report_palette[["red"]])) +
  scale_color_manual(values = c("Balance fiscal" = report_palette[["yellow"]])) +
  scale_y_continuous(limits = range(y_breaks), breaks = y_breaks, expand = expansion(mult = 0),
    labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = plotted$year, expand = expansion(add = 0.7)) +
  labs(title = "Venezuela: ingresos, gastos y balance fiscal",
    subtitle = paste0("Gobierno general, porcentaje del PIB | ", min(plotted$year), "–", max(plotted$year)),
    x = NULL, y = NULL,
    caption = paste0("Fuente: FMI, World Economic Outlook. Series del FMI; incluyen estimaciones.\n",
      "Gastos representados con signo negativo. Balance fiscal = ingresos − gastos.")) +
  report_chart_theme + guides(fill = guide_legend(order = 1, nrow = 1), color = guide_legend(order = 2, nrow = 1))
readr::write_csv(plotted, "data/final/fiscal_90_balance_graph.csv")
ggsave("outputs/figures/fiscal_revenue_expenditure_balance.png", fiscal_figure,
  width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
ggsave("outputs/figures/fiscal_revenue_expenditure_balance.pdf", fiscal_figure,
  width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
print(plotted %>% select(year, revenue, expenditure, balance, reconciliation_pp))
