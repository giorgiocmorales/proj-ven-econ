# Render new ten-year OR-filter heatmaps through the existing joined-chart renderer.
# Retain the one-year charts; use inclusive cutoffs with growth at or above 8%.
joined_heatmap_config <- list(growth_threshold = 8, minimum_years = 10L, filename_suffix = "_10years")
source("scripts/cross_country_05_joined_heatmaps.R", local = TRUE)
