# Extract VES/USD data from Yadio API

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)

source("R/source_helpers.R")
ensure_project_dirs()

# Fetch url ------
url_hist <- "https://web2.xekura.io/json/VES/hist.json"
# Preserve the complete API response before parsing, including in response replays.
raw_path <- "data/raw/yadio_hist.json"
legacy_path <- "data/raw/external_04_fx_yadio.csv"
if (source_acquisition_mode() != "update" && !file.exists(raw_path) && file.exists(legacy_path)) {
  # The original acquisition retained its full parsed response, but not HTTP JSON bytes.
  message("Using retained Yadio response table; historical HTTP JSON was not retained")
  data_list <- readr::read_csv(legacy_path, show_col_types = FALSE)
} else {
  download_latest_file(url_hist, raw_path, max_attempts = 3, timeout_seconds = 45)
  data_list <- fromJSON(raw_path)
}

# Reject schema drift before transforming or replacing the local history.
required_columns <- c("date", "rate", "avg24h", "usdbtc")
if (!is.data.frame(data_list) || !all(required_columns %in% names(data_list))) {
  rlang::abort("Yadio response is missing required rate columns", class = "yadio_schema_error")
}

#Clean ------
# Preserve the full API response, including historical zero-volume price placeholders.
write_raw_dataset(data_list, "external_04_fx_yadio")
message(sprintf("Excluding %s historical rows with unavailable zero BTC prices", sum(data_list$avg24h == 0, na.rm = TRUE)))
ves_hist_clean <- data_list %>%
  filter(is.na(.data$avg24h) | .data$avg24h != 0) %>%
  mutate(date = mdy(date), implicit_rate = avg24h/usdbtc) %>%
  arrange(date) %>%
  select(date, rate, implicit_rate, avg24h, usdbtc) %>%
  arrange(date)

# Validate observations before replacing the local history.
if (nrow(ves_hist_clean) == 0 || anyNA(ves_hist_clean$date) ||
    any(!is.finite(ves_hist_clean$implicit_rate) | ves_hist_clean$implicit_rate <= 0) ||
    anyDuplicated(ves_hist_clean$date) > 0) {
  rlang::abort("Yadio response contains empty, invalid, or duplicate observations", class = "yadio_data_error")
}

# Save ------
write_csv(ves_hist_clean, "data/final/external_04_fx_yadio.csv")

#Clean up -----
rm(data_list, ves_hist_clean, raw_path, url_hist)
