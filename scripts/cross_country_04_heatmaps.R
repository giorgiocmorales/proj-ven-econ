# Plot annual macroeconomic states without mixing overlapping sources.
# Render IMF and WB histories for all countries with available historical data.
# Run from the repository root after the raw-only download; no network is required.
library(tidyverse)
library(jsonlite)
# Read the existing presentation theme with its Spanish labels correctly on Windows.
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("A UTF-8 locale is required for heatmap labels.", class = "macro_config_error")
  }
}
source("R/report_figures.R", encoding = "UTF-8")
source("R/cross_country_heatmap_helpers.R")

## Historical observations and regional metadata ----
# Use the cached World Bank region classification, including separately reported economies.
raw_dir <- "data/raw/cross_country_macro"
output_dir <- "outputs/cross_country_macro"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
observations <- read_csv(file.path(raw_dir, "cross_country_inputs.csv"), show_col_types = FALSE) %>%
  filter(period_class != "future_or_current_year_projection")
country_metadata <- read_cross_country_regions(raw_dir) %>% select(country_code, region)
chart_definitions <- tribble(~variable, ~source, ~threshold, ~title, ~source_label, ~filename,
  "gdp_growth", "WB_WDI", 8, "Real GDP growth", "World Bank WDI", "gdp_growth_heatmap",
  "gdp_growth", "IMF_WEO", 8, "Real GDP growth", "IMF WEO", "gdp_growth_heatmap_imf",
  "primary_balance_gdp", "IMF_WEO", 10, "Primary balance", "IMF WEO", "primary_balance_heatmap",
  "primary_balance_gdp", "WB_CCDFS", 10, "Primary balance", "World Bank Data360", "primary_balance_heatmap_wb",
  "inflation", "WB_WDI", 200, "Inflation", "World Bank WDI", "inflation_heatmap_wb_bands",
  "inflation", "IMF_WEO", 200, "Inflation", "IMF WEO", "inflation_heatmap_imf_bands",
  "debt_gdp", "WB_CCDFS", 100, "General-government gross debt / GDP", "World Bank Data360", "debt_gdp_heatmap_wb_bands",
  "debt_gdp", "IMF_WEO", 100, "General-government gross debt / GDP", "IMF WEO", "debt_gdp_heatmap_imf_bands")

## Shared heatmap style ----
# Growth and balance retain sign colors; debt and inflation use ordered severity bands.
heatmap_colors <- c(negative = "#C95B5B", zero = "#929CA4", positive = "#BFE3C7",
                   above_threshold = report_palette[["green"]], missing = "#F1F3F5")
heatmap_theme <- report_chart_theme + theme(
  # Inherit the shared serif family for titles, axes, regional strips, and legends.
  plot.title = element_text(size = 20, face = "bold"),
  plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
  plot.caption = element_text(size = 9, lineheight = 1.25),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 11),
  axis.line = element_blank(), axis.title = element_blank(),
  panel.grid = element_blank(), panel.grid.major.y = element_blank(),
  panel.spacing.y = grid::unit(9, "pt"),
  strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, size = 10),
  strip.background = element_blank(), legend.position = "top",
  legend.text = element_text(size = 10), legend.key.width = grid::unit(14, "pt"),
  legend.key.height = grid::unit(12, "pt"), plot.margin = margin(15, 20, 15, 15))

## Annual matrices and exports ----
# Include every country with at least one historical observation, retaining its history.
# Expand each included economy to the same year grid so missing years remain visible.
for (chart_number in seq_len(nrow(chart_definitions))) {
  definition <- chart_definitions[chart_number, ]
  chart_data <- observations %>%
    filter(variable == definition$variable, source == definition$source)
  included_countries <- chart_data %>% filter(!is.na(value)) %>%
    distinct(country_code) %>% left_join(country_metadata, by = "country_code")
  if (!nrow(included_countries) || anyNA(included_countries$region)) {
    rlang::abort("Heatmap countries are empty or missing region metadata.", class = "macro_coverage_error")
  }
  if (anyDuplicated(chart_data[c("country_code", "year")])) {
    rlang::abort("Heatmap input contains duplicate country-year cells.", class = "macro_duplicate_error")
  }
  if (any(!is.finite(chart_data$value) & !is.na(chart_data$value))) {
    rlang::abort("Heatmap input contains non-finite values.", class = "macro_value_error")
  }
  if (definition$variable == "debt_gdp" && any(chart_data$value < 0, na.rm = TRUE)) {
    rlang::abort("Gross debt/GDP contains negative observations.", class = "macro_value_error")
  }
  first_year <- min(chart_data$year[!is.na(chart_data$value)])
  last_year <- max(chart_data$year[!is.na(chart_data$value)])
  country_order <- included_countries %>% arrange(region, country_code) %>% pull(country_code)
  matrix_data <- crossing(country_code = included_countries$country_code, year = seq.int(first_year, last_year)) %>%
    left_join(chart_data, by = c("country_code", "year")) %>%
    left_join(included_countries, by = "country_code") %>%
    mutate(state = factor(case_when(
      is.na(value) ~ "missing", value < 0 ~ "negative", value == 0 ~ "zero",
      value > definition$threshold ~ "above_threshold", TRUE ~ "positive"),
      levels = names(heatmap_colors)),
      region = factor(region, levels = sort(unique(included_countries$region))),
      country_code = factor(country_code, levels = rev(country_order)))
  stopifnot(nrow(matrix_data) == nrow(included_countries) * (last_year - first_year + 1L))
  color_labels <- c(negative = "Negative", zero = "Zero",
                    positive = paste0("Positive, <=", definition$threshold, "%"),
                    above_threshold = paste0(">", definition$threshold, "%"), missing = "No data")
  chart_colors <- heatmap_colors
  interpretation_note <- "Each cell is one year. Red/green boundaries show sign changes; light/dark green boundaries show threshold crossings."

  # cut() includes the upper boundary: e.g. 100 belongs to 75-100, not >100.
  # These are visual severity bands, not an official IMF classification or episode rule.
  if (definition$variable == "debt_gdp") {
    color_labels <- c(low = "0-25%", moderate = ">25-50%", elevated = ">50-75%",
                      high = ">75-100%", above_threshold = ">100%", missing = "No data")
    chart_colors <- c(low = report_palette[["green"]], moderate = "#BFE3C7",
                      elevated = report_palette[["yellow"]], high = "#F3B6BB",
                      above_threshold = report_palette[["red"]], missing = "#F1F3F5")
    matrix_data <- matrix_data %>% mutate(state = factor(if_else(is.na(value), "missing",
      as.character(cut(value, breaks = c(0, 25, 50, 75, 100, Inf),
                       labels = names(color_labels)[1:5], include.lowest = TRUE, right = TRUE))),
      levels = names(chart_colors)))
    interpretation_note <- "Each cell is one year. Green/yellow/red bands show increasing debt/GDP; these are custom visual bands, not an official IMF scale."
  }
  if (definition$variable == "inflation") {
    color_labels <- c(deflation = "Deflation (<0%)", low = "0-5%", moderate = ">5-10%",
                      elevated = ">10-20%", alarm = ">20-100%", high = ">100-200%",
                      above_threshold = ">200%", missing = "No data")
    chart_colors <- c(deflation = "#929CA4", low = report_palette[["green"]], moderate = "#BFE3C7",
                      elevated = report_palette[["yellow"]], alarm = report_palette[["orange"]],
                      high = "#F3B6BB", above_threshold = report_palette[["red"]], missing = "#F1F3F5")
    matrix_data <- matrix_data %>% mutate(state = factor(case_when(
      is.na(value) ~ "missing", value < 0 ~ "deflation",
      TRUE ~ as.character(cut(pmax(value, 0), breaks = c(0, 5, 10, 20, 100, 200, Inf),
                              labels = names(color_labels)[2:7], include.lowest = TRUE, right = TRUE))),
      levels = names(chart_colors)))
    interpretation_note <- "Each cell is one year. Inflation above 20% enters orange/red alert bands; deflation is shown separately in gray."
  }
  source_note <- switch(definition$source,
    WB_WDI = if (definition$variable == "gdp_growth") {
      "Honduras growth in 1990-2000 differs materially from IMF; WDI values are retained."
    } else {
      "Consumer-price inflation observations are retained as published by WDI."
    },
    IMF_WEO = "Completed-year estimates are retained. Projection years are excluded.",
    WB_CCDFS = "Data360 fiscal-space observations are retained as published; this source may incorporate IMF inputs.")
  region_note <- if ("TWN" %in% included_countries$country_code) {
    "World Bank regions; TWN is assigned to East Asia & Pacific. ISO codes are alphabetical within each region."
  } else {
    "Regions follow World Bank classifications; ISO codes are alphabetical within each region."
  }
  figure <- ggplot(matrix_data, aes(x = year, y = country_code, fill = state)) +
    geom_tile(width = 1, height = 1, show.legend = TRUE) +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y", switch = "y",
               labeller = labeller(region = label_wrap_gen(width = 22))) +
    scale_fill_manual(values = chart_colors, labels = color_labels, drop = FALSE) +
    scale_x_continuous(breaks = seq(ceiling(first_year / 10) * 10, last_year, by = 10),
                       position = "top", expand = expansion(add = 0.5)) +
    scale_y_discrete(expand = expansion(add = 0.5)) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = definition$title,
         subtitle = paste0(definition$source_label, " | ", first_year, "-", last_year,
                           " | ", nrow(included_countries), " countries and economies | ",
                           if (definition$variable %in% c("gdp_growth", "inflation")) "Annual percent change" else "Percent of GDP"),
         fill = NULL,
         caption = paste0("All countries with available historical data are shown; no threshold-based country filter is applied.\n",
                           interpretation_note, "\n",
                           region_note, "\n", source_note)) +
    heatmap_theme
  figure_height <- nrow(included_countries) * 0.16 + n_distinct(included_countries$region) * 0.25 + 3
  ggsave(file.path(output_dir, paste0(definition$filename, ".png")), figure,
         width = 16, height = figure_height, units = "in", dpi = 150, bg = "white", limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(definition$filename, ".pdf")), figure,
         width = 16, height = figure_height, units = "in", device = cairo_pdf, bg = "white", limitsize = FALSE)
  write_csv(matrix_data %>% mutate(country_code = as.character(country_code), region = as.character(region)),
            file.path(output_dir, paste0(definition$filename, "_cells.csv")))
  message("Saved ", definition$filename, ": ", nrow(included_countries), " economies, ",
          first_year, "-", last_year)
}
