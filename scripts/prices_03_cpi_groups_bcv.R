# Download and extract headline and food CPI from BCV's national group table.
library(tidyverse)
source("R/source_helpers.R")
Sys.setlocale("LC_CTYPE", ".UTF-8")
dataset_id <- "prices_03_cpi_groups_bcv"
source_url <- "https://www.bcv.org.ve/sites/default/files/precios_consumidor/4_5_3.xls"
raw_path <- "data/raw/bcv_cpi_groups.xls"
if (!identical(Sys.getenv("BCV_CPI_GROUPS_OFFLINE"), "true")) {
  download_latest_file(source_url, raw_path)
}
sheet <- read_bcv_sheet(raw_path, sheet = "cuadroI ", col_names = FALSE,
                        .name_repair = "unique_quiet")
if (!identical(clean_bcv_text(sheet[[3]][7]), "Índice General (Nacional)") ||
    !identical(clean_bcv_text(sheet[[5]][7]), "Alimentos y bebidas no alcohólicas")) {
  stop("BCV CPI group headings changed; review the workbook layout")
}
cpi <- sheet %>%
  transmute(row_id = row_number(), label = clean_bcv_text(...1),
    year_marker = if_else(str_detect(coalesce(label, ""), "^20[0-9]{2}"), label, NA_character_),
    headline = clean_bcv_numeric(...3), food = clean_bcv_numeric(...5)) %>%
  fill(year_marker) %>%
  mutate(date = parse_bcv_month_date(label, year_marker),
    provisional = str_detect(coalesce(year_marker, ""), fixed("*"))) %>%
  filter(!is.na(date)) %>%
  pivot_longer(c(headline, food), names_to = "series_id", values_to = "cpi_index") %>%
  mutate(dataset_id = dataset_id, source_id = "bcv", source_url = source_url,
    source_file = raw_path, source_md5 = unname(tools::md5sum(raw_path)),
    source_modified_at = format(file.info(raw_path)$mtime, "%Y-%m-%d %H:%M:%S"),
    sheet_name = "cuadroI ", frequency = "monthly", unit = "index_dec_2007_100") %>%
  arrange(series_id, date)
if (nrow(cpi) == 0L || anyDuplicated(cpi[c("series_id", "date")]) ||
    any(!is.finite(cpi$cpi_index) | cpi$cpi_index <= 0)) stop("Invalid CPI group observations")
for (id in unique(cpi$series_id)) {
  dates <- cpi$date[cpi$series_id == id]
  if (!identical(dates, seq(min(dates), max(dates), by = "month"))) stop("CPI group history contains gaps")
}
# Audit overlapping headline values; preserve both source extracts on a revision.
headline_check <- readr::read_csv("data/processed/prices_01_cpi_bcv.csv", show_col_types = FALSE) %>%
  select(date, previous_index = cpi_index) %>%
  inner_join(cpi %>% filter(series_id == "headline") %>% select(date, cpi_index), by = "date") %>%
  mutate(relative_difference = cpi_index / previous_index - 1,
    agrees = abs(relative_difference) < 1e-8)
write_interim_dataset(headline_check, "prices_03_headline_overlap_check")
write_processed_dataset(cpi, dataset_id)
message("Extracted ", nrow(cpi), " CPI values; headline overlap: ", sum(headline_check$agrees),
  "/", nrow(headline_check), " agree. Latest month: ", max(cpi$date))
