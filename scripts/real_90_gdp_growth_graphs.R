# Plot annual real GDP growth: signed columns for Venezuela, 1999-2025.
# Run after real_01_gdp_a_a_bcv.R and real_09_gdp_historical_bcv.R.

library(tidyverse)

rm(list = ls())

# Preserve Spanish text on Windows and reuse the approved palette and ggplot theme.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("A UTF-8 locale is required for chart labels", class = "gdp_graph_locale_error")
  }
}
source("R/report_figures.R", encoding = "UTF-8")

# Use official historical growth through 2007 and the revised activity series from 2008.
# Historical growth was calculated within the 1997 price base by the source pipeline.
historical_growth <- readr::read_csv(
  "data/processed/real_09_gdp_historical_bcv.csv", show_col_types = FALSE
) %>%
  filter(.data$base_year == 1997L, between(.data$year, 1999, 2007)) %>%
  transmute(
    year = as.integer(.data$year), growth = .data$growth_percent, provisional,
    source_id, source_file = "data/processed/real_09_gdp_historical_bcv.csv",
    source_detail = .data$source_url
  )

bcv_growth <- readr::read_csv(
  "data/processed/real_01_gdp_a_a_bcv.csv", show_col_types = FALSE
) %>%
  filter(.data$series_label == "Consolidado", .data$frequency == "annual",
         .data$unit == "percent_yoy", between(.data$year, 2008, 2025)) %>%
  transmute(
    year = as.integer(.data$year), growth = .data$value, provisional,
    source_id, source_file = "data/processed/real_01_gdp_a_a_bcv.csv",
    source_detail = .data$source_url
  )

# Fail explicitly if coverage, numeric values, or annual uniqueness is invalid.
gdp_growth <- bind_rows(historical_growth, bcv_growth) %>% arrange(.data$year)
if (!identical(gdp_growth$year, 1999:2025) || any(!is.finite(gdp_growth$growth))) {
  rlang::abort("GDP graph requires exactly one finite observation for every year 1999-2025",
               class = "gdp_graph_data_error")
}

# Sign determines the phase; the zero baseline also communicates it without color.
gdp_growth <- gdp_growth %>%
  mutate(phase = factor(case_when(
    .data$growth > 0 ~ "Expansi\u00f3n",
    .data$growth < 0 ~ "Recesi\u00f3n",
    TRUE ~ "Sin variaci\u00f3n"
  ), levels = c("Expansi\u00f3n", "Recesi\u00f3n", "Sin variaci\u00f3n")))

# Annual comparisons use columns, a common zero, and a label for every calendar year.
gdp_growth_figure <- gdp_growth %>%
  ggplot(aes(x = .data$year, y = .data$growth, fill = .data$phase)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, color = "#2b3033", linewidth = 0.35) +
  scale_fill_manual(values = c(
    "Expansi\u00f3n" = report_palette[["navy"]],
    "Recesi\u00f3n" = report_palette[["red"]], "Sin variaci\u00f3n" = "#b8c1c8"
  )) +
  scale_x_continuous(breaks = 1999:2025, expand = expansion(add = 0.7)) +
  scale_y_continuous(
    breaks = scales::breaks_width(10),
    labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(
    title = "Venezuela: crecimiento anual del PIB real",
    subtitle = "Variaci\u00f3n interanual (%) | 1999\u20132025",
    x = NULL, y = NULL,
    caption = paste0(
      "Fuente: BCV; serie hist\u00f3rica (1999\u20132007) y serie revisada (2008\u20132025).\n",
      "Nota: revisiones distintas; cifras BCV 2017\u20132025 provisionales."
    )
  ) +
  report_chart_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# Export the exact plotted observations and both raster and vector chart assets.
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(gdp_growth, "data/final/real_90_gdp_growth.csv")
ggsave("outputs/figures/real_gdp_growth_1999_2025.png", plot = gdp_growth_figure,
       width = 10, height = 5.625, units = "in", dpi = 300,
       device = ragg::agg_png, bg = "white")
ggsave("outputs/figures/real_gdp_growth_1999_2025.pdf", plot = gdp_growth_figure,
       width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
