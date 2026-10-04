# Quarterly BCV year-over-year growth; run after real_03_gdp_q_a_bcv.R.
# The optional annual comparison uses the export from real_90_gdp_growth_graphs.R.
library(tidyverse)

if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) stop("UTF-8 locale required")
}
source("R/report_figures.R", encoding = "UTF-8")

source_file <- "data/processed/real_03_gdp_q_a_bcv.csv"
quarterly <- readr::read_csv(source_file, show_col_types = FALSE) %>%
  filter(series_label == "Consolidado", frequency == "quarterly", unit == "percent_yoy") %>%
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
  "Nota: incluye cifras provisionales según el extracto BCV."
} else "Nota: clasificación provisional según el extracto BCV."

growth_style <- list(

  scale_fill_manual(values = c("Expansión" = report_palette[["navy"]],
    "Recesión" = report_palette[["red"]], "Sin variación" = "#b8c1c8")),
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid"),
  scale_y_continuous(
    labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
    expand = expansion(mult = 0)
  ),
  report_chart_theme
)
build_quarterly <- function(data, every_quarter = FALSE) {
  breaks <- if (every_quarter) data$date else sort(unique(c(
    min(data$date), data$date[data$quarter == 1L], max(data$date))))
  ggplot(data, aes(date, growth, fill = phase)) +
    geom_col(width = 66) + growth_style +
    scale_x_date(breaks = breaks, labels = quarter_label,
                 expand = expansion(add = 65)) +
    labs(title = "Venezuela: crecimiento trimestral del PIB real",
      subtitle = paste0("Variación respecto al mismo trimestre del año anterior (%) | ",
        quarter_label(min(data$date)), "–", quarter_label(max(data$date))),
      x = NULL, y = NULL,
      caption = paste("Fuente: BCV; PIB consolidado por actividad económica.",
                      provisional_note, sep = "\n")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
figures <- list(real_gdp_growth_quarterly = build_quarterly(quarterly),
                real_gdp_growth_quarterly_recent = build_quarterly(recent, TRUE))

annual_file <- "data/final/real_90_gdp_growth.csv"
if (file.exists(annual_file)) {
  annual <- readr::read_csv(annual_file, show_col_types = FALSE) %>%
    filter(year >= lubridate::year(recent_start), year <= max(quarterly$year))
  if (!nrow(annual) || anyDuplicated(annual$year) || any(!is.finite(annual$growth))) {
    stop("Annual comparison requires unique, finite annual growth observations")
  }
  comparison <- bind_rows(
    annual %>% transmute(period = as.character(year), growth, phase,
                         panel = "Anual", source_file, source_detail),
    recent %>% transmute(period = quarter_label(date), growth, phase,
                         panel = "Trimestral interanual", source_file, source_detail)
  ) %>% mutate(panel = factor(panel, levels = c("Anual", "Trimestral interanual")),
               period = factor(period, levels = unique(period)))
  figures$real_gdp_growth_annual_quarterly <- ggplot(comparison, aes(period, growth, fill = phase)) +
    geom_col(width = 0.72) + growth_style +
    facet_wrap(~panel, nrow = 1, scales = "free_x") +
    labs(title = "Venezuela: crecimiento del PIB real",
      subtitle = "Variación interanual (%) | Perspectiva anual y trimestral; escala vertical común",
      x = NULL, y = NULL,
      caption = paste("Fuente: BCV. Tasas anuales publicadas; no son promedios de las tasas trimestrales.",
                      provisional_note, sep = "\n")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  readr::write_csv(comparison, "data/final/real_91_gdp_comparison_graph.csv")
}
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(quarterly, "data/final/real_91_gdp_quarterly_graph.csv")
for (figure_name in names(figures)) {
  ggsave(paste0("outputs/figures/", figure_name, ".png"), figures[[figure_name]],
    width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", figure_name, ".pdf"), figures[[figure_name]],
    width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
}
message("Validated ", nrow(quarterly), " quarterly observations; exported ", length(figures), " chart variants.")
