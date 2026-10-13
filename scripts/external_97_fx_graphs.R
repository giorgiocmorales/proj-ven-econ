# FX Graphs

library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(slider)
library(egg)
library(grid)
library(ragg)

source("R/source_helpers.R")
source("R/chart_theme.R")
ensure_project_dirs()

fx_consolidated <- read_csv(
  "data/final/external_fx.csv",
  col_types = cols(
    date = col_date(),
    rate_smc = col_double(),
    rate_smc_adj = col_double(),
    rate_indx = col_double(),
    rate_indx_adj = col_double(),
    rate_yad = col_double()
  )
) %>%
  select(date, rate_smc_adj, rate_indx_adj, rate_yad) %>%
  filter(date <= Sys.Date()) %>%
  mutate(gap = (rate_yad / rate_smc_adj - 1) * 100)

ven_set_theme()

last_date <- max(fx_consolidated$date, na.rm = TRUE)
last_date_label <- format(last_date, "%d-%m-%Y")

# 1 FX time series ---------

fx_1 <- fx_consolidated %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = rate_smc_adj, color = "TC oficial"), linewidth = 1) +
  geom_line(aes(y = rate_yad, color = "TC no-oficial"), linewidth = 1) +
  scale_color_manual(values = c("TC oficial" = "#003A5D", "TC no-oficial" = "#D70036")) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(NA, 900),
    breaks = seq(0, 900, 50),
    expand = c(0, 0),
    sec.axis = dup_axis(
      name = waiver(),
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    )
  ) +
  scale_x_date(limits = c(ymd("2022-01-01"), NA)) +
  labs(
    title = "Tipos de cambio en Venezuela",
    subtitle = "\u00A0",
    caption = paste0("Fuente: BCV; Yadio; Calculos propios. | Actualizado: ", last_date_label),
    y = "VES/USD"
  ) +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(clip = "off")

ven_save_plot("outputs/figures/fx_1.jpeg", fx_1, device = ragg::agg_jpeg)

# 2 FX time series with gap -------

last_vals <- fx_consolidated %>%
  filter(!is.na(rate_smc_adj), !is.na(rate_yad)) %>%
  filter(date == last_date) %>%
  mutate(gap_pct = rate_yad / rate_smc_adj - 1)

fx_2 <- fx_consolidated %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = rate_smc_adj, color = "TC oficial"), linewidth = 0.7) +
  geom_line(aes(y = rate_yad, color = "TC no-oficial"), linewidth = 0.7) +
  scale_color_manual(values = c("TC oficial" = "#003A5D", "TC no-oficial" = "#D70036")) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = c(0, 800),
    breaks = scales::breaks_width(50),
    expand = c(0, 0),
    sec.axis = dup_axis(
      name = waiver(),
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    )
  ) +
  scale_x_date(
    limits = c(ymd("2024-01-01"), last_date + 20),
    breaks = seq(ymd("2024-01-01"), last_date + 20, by = "3 months"),
    date_labels = "%b-%y"
  ) +
  geom_text(
    data = last_vals,
    aes(x = date + 7, y = (rate_smc_adj + rate_yad) / 2, label = percent(gap_pct, accuracy = 0.1, decimal.mark = ",")),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    family = ven_chart_specs$base_family
  ) +
  geom_text(
    data = last_vals,
    aes(x = date + 7, y = rate_smc_adj, label = number(rate_smc_adj, big.mark = ".", decimal.mark = ",", accuracy = 0.1)),
    color = "#003A5D",
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    family = ven_chart_specs$base_family
  ) +
  geom_text(
    data = last_vals,
    aes(x = date + 7, y = rate_yad, label = number(rate_yad, big.mark = ".", decimal.mark = ",", accuracy = 0.1)),
    color = "#D70036",
    inherit.aes = FALSE,
    hjust = 0,
    size = 3,
    family = ven_chart_specs$base_family
  ) +
  geom_segment(
    data = last_vals,
    aes(x = date, xend = date, y = rate_smc_adj + 7, yend = rate_yad - 5),
    color = "black",
    linewidth = 0.5,
    arrow = arrow(ends = "both", type = "closed", length = unit(0.025, "inches"))
  ) +
  labs(
    title = "Tipos de cambio",
    subtitle = "\u00A0",
    caption = paste0("Fuente: BCV; Yadio; Calculos propios. | Actualizado: ", last_date_label),
    y = "VES/USD"
  ) +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(clip = "off")

ven_save_plot("outputs/figures/fx_2.jpeg", fx_2, device = ragg::agg_jpeg)

# 3 FX gap -----

fx_3 <- fx_consolidated %>%
  mutate(gap = rate_yad / rate_smc_adj - 1) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = gap, color = "Brecha"), linewidth = 0.75, na.rm = TRUE) +
  scale_color_manual(values = c("Brecha" = "#003A5D")) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1, decimal.mark = ","),
    limits = c(0, 1.5),
    breaks = seq(0, 1.5, by = 0.1),
    expand = c(0, 0),
    sec.axis = ven_blank_secondary_axis()
  ) +
  scale_x_date(
    limits = c(ymd("2024-01-01"), last_date + 15),
    breaks = seq(ymd("2024-01-01"), last_date + 15, by = "3 months"),
    date_labels = "%b-%y"
  ) +
  labs(
    title = "Brecha cambiaria",
    subtitle = "Tipo de cambio no-oficial/oficial",
    caption = paste0("Fuente: BCV; Yadio; Calculos propios. | Actualizado: ", last_date_label),
    y = "Brecha"
  ) +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(clip = "off")

ven_save_plot("outputs/figures/fx_3.jpeg", fx_3, device = ragg::agg_jpeg)
