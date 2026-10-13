# Export annual goods trade through the shared chart builder, with the edition's cutoff.
library(tidyverse)
source("R/report_inputs.R")
source("R/report_figures.R", encoding = "UTF-8")
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
goods_data <- read_report_series(file_path = "data/final/report_series.csv") %>%
  filter(.data$variable_id %in% c("goods_exports_fob", "goods_imports_fob"),
         .data$date >= as.Date(config$params$history_start),
         lubridate::ceiling_date(.data$date, "year") - lubridate::days(1) <= as.Date(config$params$data_cutoff))
goods_figure <- build_external_figure(data = goods_data)

# Keep the exact plotted data and matching raster/vector assets for report reuse.
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(goods_data, "data/final/external_98_goods_graph.csv")
ggsave("outputs/figures/external_goods_trade.png", plot = goods_figure,
       width = 10, height = 5.625, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
ggsave("outputs/figures/external_goods_trade.pdf", plot = goods_figure,
       width = 10, height = 5.625, units = "in", device = grDevices::cairo_pdf, bg = "white")
