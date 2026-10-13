# Standalone FX charts in the approved report style.
# Run after external_95_fx_combine.R; uses existing data without downloading.
library(tidyverse)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) stop("UTF-8 locale required")
}
source("R/report_figures.R", encoding = "UTF-8")

config_path <- Sys.getenv("REPORT_CONFIG", "config/report.yml")
source("R/report_inputs.R")
config <- read_report_config(path = config_path)
start_date <- as.Date(config$params$recent_history_start)
cutoff <- as.Date(config$params$data_cutoff)
if (length(start_date) != 1L || length(cutoff) != 1L ||
    is.na(start_date) || is.na(cutoff) || start_date > cutoff) {
  stop("Report configuration requires a valid recent window and cutoff")
}
input_path <- "data/final/external_fx.csv"
fx <- readr::read_csv(input_path, show_col_types = FALSE) %>%
  select(date, official = rate_smc_adj, unofficial = rate_yad) %>%
  arrange(date)
if (!nrow(fx) || !inherits(fx$date, "Date") || anyNA(fx$date) || anyDuplicated(fx$date)) {
  stop("FX input requires unique valid dates")
}
for (series in c("official", "unofficial")) {
  values <- fx[[series]]
  if (any(!is.na(values) & (!is.finite(values) | values <= 0))) {
    stop("FX rates must be positive and finite where reported")
  }
}
# Complete the calendar without filling values; preserve any additional gaps.
# The upstream consolidated input already carries rates between observations.
fx <- fx %>% filter(date <= cutoff) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%
  arrange(date) %>%
  mutate(gap_pct = (unofficial / official - 1) * 100,
    gap_ma30_pct = slider::slide_dbl(gap_pct,
      ~if (all(is.finite(.x))) mean(.x) else NA_real_,
      .before = 29L, .complete = TRUE))
# Calculate the trailing mean before selecting the display window.
plotted <- fx %>% filter(date >= start_date, !is.na(official) | !is.na(unofficial))
if (!nrow(plotted) || sum(is.finite(plotted$gap_pct)) < 30L ||
    !any(is.finite(plotted$gap_ma30_pct))) stop("Insufficient overlapping FX history")
# Retain missing calendar rows inside the plotted span to break lines at gaps.
plotted <- fx %>% filter(date >= min(plotted$date), date <= max(plotted$date))
last_date <- max(plotted$date)
first_date <- min(plotted$date)
coverage <- map_dfr(c("official", "unofficial", "gap_pct", "gap_ma30_pct"), function(series) {
  dates <- plotted$date[is.finite(plotted[[series]])]
  tibble(series, first_date = min(dates), last_date = max(dates), observations = length(dates))
})
date_breaks <- sort(unique(c(first_date,
  seq(lubridate::ceiling_date(first_date, "year"), last_date, by = "year"), last_date)))
# Avoid a crowded year tick immediately next to the final date.
date_breaks <- date_breaks[date_breaks == first_date | date_breaks == last_date |
                           as.numeric(last_date - date_breaks) >= 90]
date_labels <- ifelse(date_breaks %in% c(first_date, last_date),
                      format(date_breaks, "%m/%Y"), format(date_breaks, "%Y"))
period_label <- paste0(format(first_date, "%d/%m/%Y"), "–", format(last_date, "%d/%m/%Y"))
common_axes <- list(
  scale_x_date(breaks = date_breaks, labels = date_labels, expand = expansion(mult = c(0.01, 0.02))),
  report_chart_theme
)
rates <- plotted %>% select(date, official, unofficial) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(series = factor(series, levels = c("official", "unofficial"),
    labels = c("Oficial (BCV)", "No oficial (Yadio)")))
rates_figure <- ggplot(rates, aes(date, value, color = series)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = c("Oficial (BCV)" = report_palette[["navy"]],
    "No oficial (Yadio)" = report_palette[["red"]])) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
    expand = expansion(mult = 0)
  ) +
  labs(title = "Venezuela: tipo de cambio oficial y no oficial",
    subtitle = paste0("Series diarias, bolívares actuales por USD | ", period_label),
    x = NULL, y = NULL,
    caption = paste0("Notas: la serie consolidada conserva el último valor entre observaciones de cada fuente.\n",
      "Fuentes: BCV (SMC) y Yadio (tipo implícito).")) +
  common_axes
gap_figure <- ggplot(plotted, aes(date)) +

  geom_line(aes(y = gap_pct, color = "Brecha diaria"), linewidth = 0.45, alpha = 1) +
  geom_line(aes(y = gap_ma30_pct, color = "Media móvil de 30 días"), linewidth = 0.85) +
  scale_color_manual(values = c("Brecha diaria" = report_palette[["navy"]],
    "Media móvil de 30 días" = report_palette[["red"]])) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
    expand = expansion(mult = 0)
  ) +
  labs(title = "Venezuela: brecha cambiaria",
    subtitle = paste0("(Tipo no oficial / tipo oficial − 1) × 100 | ", period_label),
    x = NULL, y = NULL,
    caption = paste0("Notas: media móvil de los últimos 30 días calendario, incluido el día actual; exige 30 valores.\n",
      "Fuentes: BCV y Yadio; cálculos propios sobre la serie consolidada.")) +
  common_axes

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(plotted, "data/final/external_99_fx_graph.csv")
readr::write_csv(coverage, "data/final/external_99_fx_graph_coverage.csv")
figures <- list(external_fx_daily = rates_figure, external_fx_gap_daily_ma30 = gap_figure)
for (figure_name in names(figures)) {
  ggsave(paste0("outputs/figures/", figure_name, ".png"), figures[[figure_name]],
    width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", figure_name, ".pdf"), figures[[figure_name]],
    width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
}
# Independently verify the final rolling window against its calendar dates.
last_average <- max(fx$date[is.finite(fx$gap_ma30_pct)])
last_window <- fx %>% filter(date >= last_average - 29, date <= last_average)
stopifnot(nrow(last_window) == 30L,
  isTRUE(all.equal(last_window$gap_ma30_pct[30], mean(last_window$gap_pct))))
print(coverage)
message("Exported two FX charts in PNG and PDF; verified the final 30-day average.")
