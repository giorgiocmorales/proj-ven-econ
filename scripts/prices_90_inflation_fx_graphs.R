# Standalone charts from finalized CPI and FX data; no custom plot wrappers.
library(tidyverse)
Sys.setlocale("LC_CTYPE", ".UTF-8")
source("R/report_figures.R", encoding = "UTF-8")
source("R/report_inputs.R")
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
start_date <- as.Date(config$params$recent_history_start)
cutoff <- as.Date(config$params$data_cutoff)
# A month is included only once its final calendar day is within the cutoff.
last_complete_month <- floor_date(cutoff + 1, "month") - months(1)
inflation <- readr::read_csv("data/final/prices_04_inflation_groups_bcv.csv", show_col_types = FALSE) %>%
  filter(date <= last_complete_month)
if (anyDuplicated(inflation[c("date", "series_id", "variable_id")]) ||
    any(!is.finite(inflation$value))) stop("Invalid inflation chart input")

# Monthly FX averages use the existing consolidated calendar-day rates.
fx <- readr::read_csv("data/final/external_fx.csv", show_col_types = FALSE) %>%
  select(date, official = rate_smc_adj, unofficial = rate_yad) %>% filter(date <= cutoff)
if (anyDuplicated(fx$date) || anyNA(fx$date)) stop("Invalid daily FX dates")
fx_monthly <- fx %>% pivot_longer(-date, names_to = "series_id", values_to = "rate") %>%
  mutate(date_month = floor_date(date, "month")) %>%
  group_by(series_id, date_month) %>%
  summarise(calendar_days = n(), valid_days = sum(is.finite(rate) & rate > 0),
    monthly_rate = if (n() == days_in_month(first(date)) && all(is.finite(rate) & rate > 0)) mean(rate) else NA_real_,
    .groups = "drop") %>% rename(date = date_month) %>%
  filter(date <= last_complete_month) %>% group_by(series_id) %>%
  complete(date = seq(min(date), max(date), by = "month")) %>% arrange(date, .by_group = TRUE) %>%
  mutate(value = 100 * (monthly_rate / lag(monthly_rate) - 1)) %>% ungroup()
readr::write_csv(fx_monthly, "data/final/prices_90_fx_monthly_changes.csv")

yoy_data <- inflation %>% filter(variable_id == "inflation_yoy", date >= start_date) %>%
  mutate(series = factor(series_id, levels = c("headline", "food"),
    labels = c("Índice general", "Alimentos y bebidas no alcohólicas")))
end_date <- max(yoy_data$date)
# Align the four-series comparison to the latest complete month available to every series.
monthly_data <- bind_rows(
  inflation %>% filter(variable_id == "inflation_mom") %>% select(date, series_id, value),
  fx_monthly %>% select(date, series_id, value)
)
coverage <- monthly_data %>% filter(is.finite(value)) %>% group_by(series_id) %>%
  summarise(first_date = min(date), last_date = max(date), .groups = "drop")
if (!setequal(coverage$series_id, c("headline", "food", "official", "unofficial"))) stop("Missing comparison series")
comparison_end <- min(coverage$last_date)
monthly_data <- monthly_data %>% filter(date >= start_date, date <= comparison_end) %>%
  mutate(series = factor(series_id, levels = c("headline", "food", "official", "unofficial"),
    labels = c("Índice general", "Alimentos y bebidas no alcohólicas", "TC oficial", "TC no oficial")))
if (any(!is.finite(monthly_data$value)) || anyDuplicated(monthly_data[c("series_id", "date")])) {
  stop("Monthly comparison contains missing values or duplicate months")
}

yoy_figure <- yoy_data %>%
  ggplot(aes(date, value, color = series)) + geom_line(linewidth = 0.7) +
  scale_color_manual(values = unname(report_palette[c("navy", "red")])) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))), expand = expansion(mult = 0)) +
  scale_x_date(breaks = sort(unique(c(seq(start_date, end_date, by = "year"), end_date))),
    date_labels = "%m/%Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Venezuela: inflación interanual general y de alimentos",
    subtitle = paste0("Variación respecto al mismo mes del año anterior (%) | ",
      format(min(yoy_data$date), "%m/%Y"), "–", format(end_date, "%m/%Y")), x = NULL, y = NULL,
    caption = "Notas: cálculos a partir de los índices; alimentos incluye bebidas no alcohólicas; cifras provisionales.\nFuentes: BCV; INPC nacional clasificado por grupos.") +
  report_chart_theme

monthly_figure <- monthly_data %>%
  ggplot(aes(date, value, fill = series)) +
  geom_col(position = position_dodge(width = 26), width = 24) +
  scale_fill_manual(values = unname(report_palette[c("navy", "red", "cyan", "green")])) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))), expand = expansion(mult = 0)) +
  scale_x_date(breaks = sort(unique(c(seq(start_date, comparison_end, by = "year"), comparison_end))),
    date_labels = "%m/%Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Venezuela: variación mensual de precios y tipos de cambio",
    subtitle = paste0("Variación respecto al mes anterior (%) | ",
      format(min(monthly_data$date), "%m/%Y"), "–", format(comparison_end, "%m/%Y")), x = NULL, y = NULL,
    caption = "Notas: alimentos incluye bebidas no alcohólicas. TC: variación del promedio mensual de tasas diarias consolidadas.\nSolo meses completos; IPC provisional.\nFuentes: BCV y Yadio; cálculos propios.") +
  report_chart_theme + guides(fill = guide_legend(nrow = 1, byrow = TRUE))

readr::write_csv(yoy_data, "data/final/prices_90_inflation_yoy_graph.csv")
readr::write_csv(monthly_data, "data/final/prices_90_inflation_fx_monthly_graph.csv")
readr::write_csv(coverage, "data/final/prices_90_graph_coverage.csv")
figures <- list(prices_inflation_headline_food_yoy = yoy_figure,
                prices_inflation_fx_monthly = monthly_figure)
for (name in names(figures)) {
  ggsave(paste0("outputs/figures/", name, ".png"), figures[[name]], width = 10, height = 5.625,
    units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", name, ".pdf"), figures[[name]], width = 10, height = 5.625,
    units = "in", device = grDevices::cairo_pdf, bg = "white")
}
print(coverage)
