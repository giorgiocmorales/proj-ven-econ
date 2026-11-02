# Plot source-backed Merey/Brent prices and positive Merey discounts in separate units.
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for shared chart labels", class = "oil_graph_locale_error")
  }
}
source("R/report_figures.R", encoding = "UTF-8")

# Use the full common monthly history and retain gaps exactly as in the spread dataset.
input_path <- "data/final/oil_16_merey_brent_spread.csv"
spread <- read_csv(input_path, show_col_types = FALSE,
  col_types = cols(merey_footnote_marker = col_character(), brent_footnote_marker = col_character())) %>%
  mutate(discount_usd_per_barrel = -spread_usd_per_barrel, discount_pct_brent = -spread_pct_brent)
stop_for_problems(spread)
if (anyDuplicated(spread$date) || anyNA(spread$date) ||
    !identical(spread$date, seq.Date(min(spread$date), max(spread$date), by = "month"))) {
  rlang::abort("Oil-price charts require a unique complete monthly calendar", class = "oil_graph_data_error")
}
start_date <- min(spread$date)
end_date <- max(spread$date)
date_breaks <- sort(unique(c(start_date, as.Date(c("2015-01-01", "2020-01-01", "2025-01-01")), end_date)))
date_breaks <- date_breaks[date_breaks >= start_date & date_breaks <= end_date]
date_labels <- ifelse(date_breaks %in% c(start_date, end_date), format(date_breaks, "%m/%Y"), format(date_breaks, "%Y"))
shared_style <- list(
  scale_x_date(breaks = date_breaks, labels = date_labels, expand = expansion(mult = c(0.01, 0.02))),
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid"),
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
    limits = ~range(pretty(c(.x, 0), n = 6)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 6)))), expand = expansion(mult = 0)),
  report_chart_theme)
prices <- spread %>% select(date, `Brent (OPEC benchmark)` = brent_price_usd_per_barrel,
  Merey = merey_price_usd_per_barrel) %>%
  pivot_longer(-date, names_to = "series", values_to = "value")
prices_figure <- ggplot(prices, aes(date, value, color = series)) +
  geom_line(linewidth = 0.7, linetype = "solid") +
  scale_color_manual(values = c("Brent (OPEC benchmark)" = report_palette[["navy"]], Merey = report_palette[["red"]])) +
  shared_style + labs(title = "Merey and Brent: monthly crude prices",
    subtitle = paste0(format(start_date, "%m/%Y"), " - ", format(end_date, "%m/%Y"), " | USD per barrel"),
    x = NULL, y = "USD per barrel",
    caption = "Notes: latest published revision by month. OPEC benchmark labels include Brent, Dated Brent and North Sea Dated.\nSource: OPEC Monthly Oil Market Reports, archived price tables. Missing Merey in March 2020 remains a gap.")

# A positive discount means Merey is cheaper; negative values would indicate a premium.
# Separate panels avoid a dual axis and keep the USD and percentage measures explicit.
discounts <- spread %>% select(date, discount_usd_per_barrel, discount_pct_brent) %>%
  pivot_longer(-date, names_to = "measure", values_to = "value") %>%
  mutate(measure = factor(measure, levels = c("discount_usd_per_barrel", "discount_pct_brent"),
    labels = c("Discount: USD per barrel", "Discount: percent of Brent")))
discount_figure <- ggplot(discounts, aes(date, value)) +
  geom_line(color = report_palette[["navy"]], linewidth = 0.7, linetype = "solid") +
  facet_wrap(~measure, ncol = 1L, scales = "free_y") + shared_style +
  labs(title = "Merey discount relative to Brent",
    subtitle = "Positive values mean Merey is cheaper than Brent",
    x = NULL, y = NULL,
    caption = "Notes: USD discount = Brent - Merey; percent discount = 100 x (Brent - Merey) / Brent. No gap filling.\nSource: OPEC Monthly Oil Market Reports. Brent currently corresponds to North Sea Dated.")

# Retain the plotted numbers and export both requested graphs with matching PDF copies.
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
write_csv(spread, "outputs/data/oil_92_merey_brent_graph.csv")
figures <- list(oil_merey_brent_prices = prices_figure, oil_merey_brent_discount = discount_figure)
for (figure_name in names(figures)) {
  plot_height <- if (figure_name == "oil_merey_brent_discount") 6.5 else 5.625
  ggsave(paste0("outputs/figures/", figure_name, ".png"), figures[[figure_name]],
    width = 10, height = plot_height, units = "in", dpi = 250, device = ragg::agg_png, bg = "white")
  ggsave(paste0("outputs/figures/", figure_name, ".pdf"), figures[[figure_name]],
    width = 10, height = plot_height, units = "in", device = grDevices::cairo_pdf, bg = "white")
}
