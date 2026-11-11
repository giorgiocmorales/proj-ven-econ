# Select countries on any annual threshold crossing and show all four annual histories.
# IMF and WB selections are separate; WB uses WDI growth/CPI and Data360 fiscal data.
# Run from the repository root after the raw-only download; no API refresh is required.
library(tidyverse)
library(jsonlite)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("A UTF-8 locale is required for heatmap labels.", class = "macro_config_error")
  }
}
source("R/report_figures.R", encoding = "UTF-8")
source("R/cross_country_heatmap_helpers.R")
source("R/cross_country_episode_helpers.R")

## Thresholds and historical inputs ----
# The stricter companion script supplies its own growth cutoff and required duration.
if (!exists("joined_heatmap_config", inherits = FALSE)) {
  joined_heatmap_config <- list(growth_threshold = 10, minimum_years = 1L, filename_suffix = "")
}
variables <- tribble(~variable, ~variable_label, ~threshold, ~above_state,
  "gdp_growth", "Growth", joined_heatmap_config$growth_threshold, "above_growth_balance",
  "primary_balance_gdp", "Primary balance", 10, "above_growth_balance",
  "debt_gdp", "Debt/GDP", 100, "above_debt_inflation",
  "inflation", "Inflation", 200, "above_debt_inflation") %>%
  mutate(wb_source = if_else(variable %in% c("gdp_growth", "inflation"), "WB_WDI", "WB_CCDFS"))
raw_dir <- "data/raw/cross_country_macro"
output_dir <- "outputs/cross_country_macro"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
country_metadata <- read_cross_country_regions(raw_dir)
observations <- read_csv(file.path(raw_dir, "cross_country_inputs.csv"), show_col_types = FALSE) %>%
  filter(period_class != "future_or_current_year_projection") %>%
  inner_join(variables, by = "variable") %>%
  filter(source == "IMF_WEO" | source == wb_source) %>%
  mutate(provider = if_else(source == "IMF_WEO", "IMF", "WB"))
if (anyDuplicated(observations[c("provider", "country_code", "variable", "year")])) {
  rlang::abort("Joined heatmap input contains duplicate provider-country-variable-year keys.", class = "macro_duplicate_error")
}
if (any(!is.finite(observations$value) & !is.na(observations$value))) {
  rlang::abort("Joined heatmap input contains non-finite values.", class = "macro_value_error")
}

## Shared threshold palette and typography ----
# Gray values are below the inclusive threshold; missing observations remain separate.
chart_colors <- c(above_growth_balance = report_palette[["green"]],
                  above_debt_inflation = report_palette[["red"]],
                  below_threshold = "#C5CCD2", missing = "#F1F3F5")
chart_labels <- c(above_growth_balance = "Growth / primary balance at or above threshold",
                  above_debt_inflation = "Debt / inflation at or above threshold",
                  below_threshold = "Below threshold", missing = "No data")
joined_theme <- report_chart_theme + theme(
  plot.title = element_text(size = 20, face = "bold"),
  plot.subtitle = element_text(size = 11, margin = margin(b = 12)),
  plot.caption = element_text(size = 9, lineheight = 1.25),
  # Draw separate ISO and variable label columns in the reserved left panel space.
  axis.text.y = element_blank(),
  axis.text.x = element_text(size = 11),
  axis.title = element_blank(), axis.line = element_blank(),
  panel.grid = element_blank(), panel.grid.major.y = element_blank(),
  panel.spacing.y = grid::unit(12, "pt"), strip.placement = "outside",
  strip.text.y.left = element_text(angle = 90, hjust = 0.5, size = 10),
  strip.background = element_blank(), legend.position = "top", legend.text = element_text(size = 10),
  plot.margin = margin(15, 20, 15, 15))

## Separate provider populations and complete matrices ----
for (provider_name in c("IMF", "WB")) {
  provider_data <- observations %>% filter(provider == provider_name)
  # Passing values are segmented before filtering; missing years and failures break runs.
  selected <- if (joined_heatmap_config$minimum_years == 1L) {
    provider_data %>% filter(!is.na(value), value >= threshold) %>%
      group_by(country_code, variable) %>%
      summarise(years_at_or_above_threshold = n(), first_crossing_year = min(year),
                last_crossing_year = max(year), .groups = "drop")
  } else {
    provider_data %>%
      mutate(value = case_when(is.na(value) ~ NA_real_, value >= threshold ~ 1, TRUE ~ -1)) %>%
      build_sign_episode_paths() %>% filter(episode_sign == "positive") %>%
      group_by(country_code, variable, episode_id) %>%
      summarise(start_year = first(year), end_year = last(year), consecutive_years = n(),
                .groups = "drop") %>% filter(consecutive_years >= joined_heatmap_config$minimum_years)
  }
  selected <- selected %>%
    left_join(country_metadata, by = "country_code") %>% arrange(region, country_code, variable)
  countries <- selected %>% distinct(country_code, country, region) %>% arrange(region, country_code)
  if (!nrow(countries) || anyNA(countries$region)) {
    rlang::abort("Joined heatmap countries are empty or missing regional metadata.", class = "macro_coverage_error")
  }
  first_year <- min(provider_data$year[!is.na(provider_data$value)])
  last_year <- max(provider_data$year[!is.na(provider_data$value)])
  rows <- crossing(country_code = countries$country_code, variable = variables$variable) %>%
    left_join(countries, by = "country_code") %>% left_join(variables, by = "variable") %>%
    left_join(selected %>% distinct(country_code, variable) %>% mutate(qualifies = TRUE),
              by = c("country_code", "variable")) %>%
    mutate(qualifies = replace_na(qualifies, FALSE)) %>%
    mutate(variable_order = match(variable, variables$variable)) %>%
    arrange(region, country_code, variable_order) %>%
    mutate(row_id = paste(country_code, variable, sep = ":"), row_label = variable_label)
  matrix_data <- crossing(row_id = rows$row_id, year = seq.int(first_year, last_year)) %>%
    left_join(rows, by = "row_id") %>%
    left_join(provider_data %>% select(country_code, variable, year, source, value, period_class, unit),
              by = c("country_code", "variable", "year")) %>%
    mutate(provider = provider_name,
           state = factor(case_when(is.na(value) ~ "missing", value >= threshold ~ above_state,
                                    TRUE ~ "below_threshold"), levels = names(chart_colors)),
           row_id = factor(row_id, levels = rev(rows$row_id)),
           region = factor(region, levels = sort(unique(countries$region))))
  stopifnot(nrow(matrix_data) == nrow(countries) * 4L * (last_year - first_year + 1L))
  # Country dividers align with four-row blocks within each free-height region facet.
  separators <- rows %>% count(region, name = "row_count") %>%
    mutate(yintercept = map(row_count, ~ seq(4.5, .x + 0.5, by = 4))) %>% unnest(yintercept) %>%
    filter(yintercept < row_count)
  # Center ISO codes over each four-row country block in its region's local y scale.
  # Reserve fixed-width columns before the first year, without inventing data cells.
  label_width_years <- (last_year - first_year + 1L) * 0.13
  variable_labels <- rows %>% group_by(region) %>%
    mutate(panel_y = n() - row_number() + 1L,
           label_x = first_year - label_width_years * 0.1) %>% ungroup()
  country_labels <- variable_labels %>%
    group_by(region, country_code) %>% summarise(panel_y = mean(panel_y), .groups = "drop") %>%
    mutate(label_x = first_year - label_width_years * 0.8)
  # Check marks use the chart's selection duration, rather than any isolated colored cell.
  qualification_labels <- variable_labels %>% filter(qualifies) %>%
    mutate(label_x = last_year + (last_year - first_year + 1L) * 0.045)
  qualification_header <- countries %>% slice_head(n = 1) %>%
    mutate(label_x = last_year + (last_year - first_year + 1L) * 0.045)
  source_note <- if (provider_name == "WB") {
    "Sources: WDI growth/CPI; Data360 primary balance/general-government debt. Honduras growth differs materially from IMF."
  } else {
    "Source: IMF WEO. Completed-year estimates are retained; projections are excluded."
  }
  if ("TWN" %in% countries$country_code) {
    source_note <- paste0(source_note, " TWN is assigned to East Asia & Pacific.")
  }
  figure <- ggplot(matrix_data, aes(year, row_id, fill = state)) +
    geom_tile(width = 1, height = 1, show.legend = TRUE) +
    geom_hline(data = separators, aes(yintercept = yintercept), inherit.aes = FALSE,
               color = "white", linewidth = 0.8) +
    geom_text(data = country_labels, aes(x = label_x, y = panel_y, label = country_code),
              inherit.aes = FALSE, family = "serif", fontface = "bold", size = 10 / ggplot2::.pt) +
    geom_text(data = variable_labels, aes(x = label_x, y = panel_y, label = variable_label),
              inherit.aes = FALSE, family = "serif", hjust = 1, size = 10 / ggplot2::.pt) +
    geom_text(data = qualification_labels, aes(x = label_x, y = panel_y, color = above_state),
              label = "\u2713", inherit.aes = FALSE, family = "serif", size = 13 / ggplot2::.pt,
              show.legend = FALSE) +
    geom_text(data = qualification_header, aes(x = label_x), y = Inf, label = "Qualifies",
              inherit.aes = FALSE, family = "serif", size = 10 / ggplot2::.pt, vjust = -0.8) +
    facet_grid(rows = vars(region), scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_manual(values = chart_colors, labels = chart_labels, drop = FALSE) +
    scale_color_manual(values = chart_colors, guide = "none") +
    scale_y_discrete(breaks = NULL, expand = expansion(add = 0.5)) +
    scale_x_continuous(breaks = seq(ceiling(first_year / 10) * 10, last_year, by = 10),
                       position = "top", limits = c(first_year - label_width_years - 0.5,
                         last_year + (last_year - first_year + 1L) * 0.09 + 0.5),
                       oob = scales::oob_keep, expand = expansion(mult = 0)) +
    coord_cartesian(clip = "off") +
    guides(fill = guide_legend(nrow = 1)) +
    labs(title = paste0(provider_name, " | Countries meeting at least one threshold",
                        if (joined_heatmap_config$minimum_years > 1L) {
                          paste0(" for ", joined_heatmap_config$minimum_years, "+ consecutive years")
                        } else ""),
         subtitle = paste0(first_year, "-", last_year, " | ", nrow(countries),
                           " countries and economies | Region > ISO code > variable\n",
                           "Growth \u2265", joined_heatmap_config$growth_threshold,
                           "% | Primary balance \u226510% GDP | Debt/GDP \u2265100% | Inflation \u2265200%"), fill = NULL,
         caption = paste0("Selection: any one condition meets or exceeds its threshold for at least ",
                           joined_heatmap_config$minimum_years, " consecutive historical year(s).\n",
                           "All four annual histories are retained. Missing values or calendar gaps break qualifying runs; simultaneous crossings are not required.\n",
                           "A check mark identifies each variable satisfying this chart's required consecutive-year duration.\n",
                           "GDP growth and CPI inflation are annual percent changes; primary balance and general-government gross debt are percent of GDP.\n",
                           source_note)) + joined_theme
  filename <- paste0("joined_threshold_heatmap_", tolower(provider_name), joined_heatmap_config$filename_suffix)
  figure_height <- nrow(countries) * 4 * 0.16 + n_distinct(countries$region) * 0.25 + 3.5
  ggsave(file.path(output_dir, paste0(filename, ".png")), figure, width = 18, height = figure_height,
         units = "in", dpi = 120, bg = "white", limitsize = FALSE)
  ggsave(file.path(output_dir, paste0(filename, ".pdf")), figure, width = 18, height = figure_height,
         units = "in", device = cairo_pdf, bg = "white", limitsize = FALSE)
  write_csv(matrix_data, file.path(output_dir, paste0(filename, "_cells.csv")))
  write_csv(selected, file.path(output_dir, paste0(filename, "_selection.csv")))
  message(provider_name, ": selected ", nrow(countries), " countries; exported ", nrow(matrix_data), " cells.")
}
