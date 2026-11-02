# Render additional five-year OR-filter heatmaps, preserving the ten-year exports.
# Use inclusive cutoffs: growth 8%, primary balance 10%, debt/GDP 100%, inflation 200%.
joined_heatmap_config <- list(growth_threshold = 8, minimum_years = 5L, filename_suffix = "_5years")
source("scripts/cross_country_05_joined_heatmaps.R", local = TRUE)
