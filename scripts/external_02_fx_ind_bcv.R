# Extract BCV investment-index VES/USD observations from the paginated archive.

library(rvest)
library(tidyverse)
library(lubridate)
library(glue)

source("R/source_helpers.R")
ensure_project_dirs()

base_url <- "https://www.bcv.org.ve/estadisticas/indice-de-inversion"
processed_path <- "data/processed/external_02_fx_ind_bcv.csv"

# Download a BCV page locally before parsing because direct rvest requests can fail.
download_bcv_index_page <- function(url) {
  downloaded_path <- tempfile(fileext = ".html")

  download.file(
    url = url,
    destfile = downloaded_path,
    mode = "wb",
    quiet = TRUE,
    method = "libcurl"
  )

  downloaded_path
}

# Read the page number from BCV's pagination links, including the final page.
extract_bcv_index_page_numbers <- function(page) {
  hrefs <- rvest::html_attr(rvest::html_elements(page, "a"), "href")
  page_numbers <- stringr::str_match(hrefs, "[?&]page=(\\d+)")[, 2] %>%
    as.integer()

  page_numbers <- page_numbers[!is.na(page_numbers)]

  if (length(page_numbers) == 0) {
    stop("BCV investment-index page contains no pagination links", call. = FALSE)
  }

  seq.int(0L, max(page_numbers))
}

# Extract and normalize the two-column rate table from one archive page.
scrape_bcv_fx_page <- function(page_number) {
  page_url <- glue("{base_url}?page={page_number}")
  downloaded_path <- download_bcv_index_page(url = page_url)
  on.exit(unlink(downloaded_path), add = TRUE)

  table_node <- rvest::read_html(downloaded_path) %>%
    rvest::html_element("table")

  if (inherits(table_node, "xml_missing")) {
    stop(sprintf("BCV investment-index page %s contains no table", page_number), call. = FALSE)
  }

  raw_table <- rvest::html_table(table_node)

  if (ncol(raw_table) < 2) {
    stop(sprintf("BCV investment-index page %s has fewer than two table columns", page_number), call. = FALSE)
  }

  clean_table <- raw_table %>%
    dplyr::select(fecha = 1, tasa = 2) %>%
    dplyr::mutate(
      fecha = lubridate::dmy(.data$fecha),
      tasa = .data$tasa %>%
        stringr::str_replace_all("\\.", "") %>%
        stringr::str_replace(",", ".") %>%
        as.numeric()
    ) %>%
    dplyr::filter(!is.na(.data$fecha), !is.na(.data$tasa))

  if (nrow(clean_table) == 0) {
    stop(sprintf("BCV investment-index page %s produced no valid observations", page_number), call. = FALSE)
  }

  clean_table
}

landing_path <- download_bcv_index_page(url = base_url)
landing_page <- rvest::read_html(landing_path)
# Release the temporary file after parsing; on.exit() is only valid in a function.
unlink(landing_path)
page_numbers <- extract_bcv_index_page_numbers(page = landing_page)

bcv_scraped_data <- purrr::map_dfr(page_numbers, function(page_number) {
  message(glue("Scraping BCV investment-index page {page_number} of {max(page_numbers)}"))
  scrape_bcv_fx_page(page_number = page_number)
}) %>%
  dplyr::arrange(.data$fecha) %>%
  dplyr::distinct(.data$fecha, .keep_all = TRUE)

if (nrow(bcv_scraped_data) == 0) {
  stop("BCV investment-index extractor produced no observations", call. = FALSE)
}

readr::write_csv(bcv_scraped_data, processed_path)
