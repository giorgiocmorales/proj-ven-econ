# Read the shared regional directory used by individual and joined heatmaps.
read_cross_country_regions <- function(raw_dir) {
  country_metadata <- jsonlite::fromJSON(file.path(raw_dir, "wb_countries_page_1.json"), flatten = TRUE)[[2]] %>%
    as_tibble() %>% filter(region.value != "Aggregates") %>%
    transmute(country_code = id, country = name, region = region.value)
  # IMF reports TWN outside the WB directory; charts disclose this geographic assignment.
  country_metadata <- bind_rows(country_metadata,
    tibble(country_code = "TWN", country = "Taiwan Province of China", region = "East Asia & Pacific"))
  if (anyDuplicated(country_metadata$country_code)) {
    rlang::abort("Regional metadata contains duplicate country codes.", class = "macro_duplicate_error")
  }
  country_metadata
}
