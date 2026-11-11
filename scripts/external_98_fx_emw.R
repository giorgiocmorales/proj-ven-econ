# Emerging Market Watch

# Load packages

library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(slider)
library(egg)
library(grid)
library(ragg)

# Clean up --------
rm(list = ls())

source("R/source_helpers.R")
source("R/chart_theme.R")
ensure_project_dirs()

# Set time
Sys.setlocale("LC_TIME", "English")

# Load data ------

fx_consolidated <- read_csv("data/final/external_fx.csv",
                            col_types = cols(
                              date = col_date(),
                              rate_smc = col_double(),
                              rate_smc_adj = col_double(),
                              rate_indx = col_double(),
                              rate_indx_adj = col_double(),
                              rate_yad = col_double())) %>%
  select(date, rate_smc_adj, rate_indx_adj, rate_yad) %>%
  filter(date <= Sys.Date()) %>%
  mutate(gap = (rate_yad/rate_smc_adj - 1)*100)

# Reset specs (no green rule for now) ----
start_date <- as.Date("01-01-2025", format = "%d-%m-%Y")
naval_date <- as.Date("21-09-2025", format = "%d-%m-%Y")
tanker_date <- as.Date("10-12-2025", format = "%d-%m-%Y")
capture_date <- as.Date("03-01-2026", format = "%d-%m-%Y")

# Keep only window needed
fx_plot <- fx_consolidated %>%
  filter(date >= start_date, date <= Sys.Date())

# EMW-specific charts use the same locked canvas/panel helpers, with smaller text.
theme_emw <- function(base_family = ven_chart_specs$base_family, base_size = 8) {
  ven_theme(base_family = base_family, base_size = base_size) +
    theme(
      axis.title = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "#2b3033", linewidth = 0.35, linetype = "solid"),
      axis.line.y = element_line(color = "#2b3033", linewidth = 0.35, linetype = "solid"),
      axis.ticks = element_blank(),
      axis.ticks.length = unit(0, "pt")
    )
}


# p1 = RATES (3 lines)

p1_rates <- fx_plot %>%
  select(date, rate_smc_adj, rate_yad) %>%
  pivot_longer(-date, names_to = "series", values_to = "rate") %>%
  mutate(
    series = recode(
      series,
      rate_smc_adj  = "Official rate (BCV)",
      rate_yad = "Unofficial rate"
    ),
    series = factor(series, levels = c("Official rate (BCV)", "Unofficial rate"))
  )

p1_gap <- fx_plot %>%
  select(date, gap) %>%
  mutate(gap = gap/100)

# Axis rates
rate_min <- 0
rate_max <- 800

gap_min  <- 0
gap_max <- 1.60

gap_to_rate <- function(g) (g - gap_min) / (gap_max - gap_min) * (rate_max - rate_min) + rate_min
rate_to_gap <- function(r) (r - rate_min) / (rate_max - rate_min) * (gap_max - gap_min) + gap_min


p1 <- ggplot(p1_rates, aes(x = date)) +
  
  # Left axis
  geom_line(aes(y = rate, color = series), linewidth = 0.7) +
  guides(color = guide_legend(override.aes = list(linewidth = 6))) +
  
  #Right axis
  geom_line(data = p1_gap, aes(y = gap_to_rate(gap), color = "Exchange rate gap. rhs"), linewidth = 0.7) +
  
  #Event lines
  geom_vline(xintercept = c(naval_date, tanker_date, capture_date), linewidth = 0.3, linetype = "solid", color = "grey40") +
  
  # Events
  annotate("text", x = capture_date, y = 55,
    label = "Maduro \ncaptured", angle = 90, vjust = -0.4, hjust = 0.5, size = 2, color = "grey20") +
  annotate("text", x = naval_date, y = 55,
    label = "US strikes\nboats",  angle = 90, vjust = -0.4, hjust = 0.5, size = 2, color = "grey20") +
  annotate("text", x = tanker_date, y = 55,
    label = "US seizes \ntanker",  angle = 90, vjust = -0.4, hjust = 0.5, size = 2, color = "grey20") +
  
  # X axisa
  scale_x_date(
    date_breaks = "1 months",
    labels = function(x)
      paste0(tools::toTitleCase(format(x, "%b")), "-", format(x, "%y")),
    expand = c(0.01, 0.01)
  ) +
  
  # Y axis
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(
    limits = c(0, 800),
    breaks = seq(0, 800, by = 100),
    labels = label_number(big.mark = ","),
    expand = c(0, 0),
    sec.axis = sec_axis(
      transform = ~ rate_to_gap(.),
      breaks = seq(gap_min, gap_max, by = 0.20),
      labels = scales::percent_format(accuracy = 1)
    )
  ) +
  scale_color_manual(values = c(
    "Official rate (BCV)"     = "#2E7D32",
    "Unofficial rate"         = "#D08D3C",
    "Exchange rate gap. rhs"  = "#8B0000"
  )) +
  labs(
    title = "Daily FX rates (VES/USD) and FX gap (%)",
    subtitle = "Source | BCV, Yadio, consolidated estimations"
  ) +
  theme_emw()

p1

# Export

ven_save_plot("outputs/figures/p1_emw.png", p1, device = ragg::agg_png)

