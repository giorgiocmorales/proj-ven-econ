# Plot reviewed Reuters shipment estimates within the report edition's monthly window.
library(tidyverse)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("A UTF-8 locale is required for chart labels", class = "oil_graph_locale_error")
  }
}
source("R/report_figures.R", encoding = "UTF-8")
source("R/report_inputs.R")
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))

# Keep missing calendar months so no line bridges an unverified observation.
exports <- readr::read_csv("data/final/oil_15_exports_reuters_monthly.csv", show_col_types = FALSE,
  col_types = cols(raw_value = col_character())) %>%
  filter(.data$date >= as.Date(config$params$history_start),
         .data$date <= as.Date(config$params$data_cutoff)) %>%
  arrange(.data$date) %>%
  mutate(segment = cumsum(is.na(.data$exports_bpd)))
if (sum(!is.na(exports$exports_bpd)) < 8L || anyDuplicated(exports$date) > 0L ||
    anyNA(exports$date) || any(!is.finite(exports$exports_bpd[!is.na(exports$exports_bpd)]))) {
  rlang::abort("Reuters chart requires eight reported months with valid unique dates and values",
               class = "oil_graph_data_error")
}

# Use the standard report palette, zero baseline, units and notes-before-sources order.
figure <- ggplot(exports, aes(x = .data$date, y = .data$exports_thousand_bpd, group = .data$segment)) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  geom_line(color = report_palette[["navy"]], linewidth = 0.7, linetype = "solid", na.rm = TRUE) +
  scale_x_date(breaks = seq(lubridate::floor_date(min(exports$date), "year"),
                          max(exports$date), by = "year"), date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)), breaks = scales::breaks_pretty(n = 7),
    expand = expansion(mult = 0)) +
  labs(title = "Venezuela: exportaciones de crudo y combustibles",
       subtitle = paste0("Miles de barriles diarios | ", format(min(exports$date), "%m/%Y"),
                         "\u2013", format(max(exports$date), "%m/%Y")), x = NULL, y = NULL,
       caption = paste0("Notas: \u00faltimo dato revisado por mes; incluye tres datos aportados por el usuario, identificados en la base.\n",
                        "Fuentes: Reuters, reportes y gr\u00e1ficos; citas identificadas y datos aportados por el usuario.")) +
  report_chart_theme

# Export exactly the observations displayed, retaining source links, flags and vintages.
readr::write_csv(exports, "outputs/data/oil_91_exports_graph.csv", na = "")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("outputs/figures/oil_15_exports_reuters_monthly.png", plot = figure,
       width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
ggsave("outputs/figures/oil_15_exports_reuters_monthly.pdf", plot = figure,
       width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
