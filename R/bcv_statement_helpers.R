# Discover BCV's own financial-statement PDFs without interpreting their contents.
bcv_statement_last_page <- function(page, page_url) {
  # Restrict pagination to the statement archive, excluding other sidebar widgets.
  links <- rvest::html_elements(page, ".view-estados-financieros a[href]") %>%
    rvest::html_attr("href") %>% xml2::url_absolute(page_url)
  links <- links[stringr::str_detect(links, "^https://www\\.bcv\\.org\\.ve/bcv/estados-financieros\\?page=[0-9]+$")]
  numbers <- as.integer(stringr::str_match(links, "[?&]page=([0-9]+)")[, 2])
  max(c(0L, numbers))
}

bcv_statement_links <- function(page, page_url) {
  # Read only the archive's document rows; empty or changed pages stop discovery.
  rows <- rvest::html_elements(page, ".view-estados-financieros tbody tr")
  if (length(rows) == 0L) {
    rlang::abort(paste("No financial-statement rows:", page_url), class = "bcv_statement_schema_error")
  }
  links <- purrr::map_dfr(rows, function(row) {
    anchors <- rvest::html_elements(row, "a[href]")
    urls <- xml2::url_absolute(rvest::html_attr(anchors, "href"), page_url)
    tibble::tibble(source_url = urls,
      title_raw = rvest::html_text2(rvest::html_element(row, "td")),
      listing_url = page_url) %>%
      dplyr::filter(stringr::str_detect(.data$source_url, stringr::regex("\\.pdf(?:$|[?#])", ignore_case = TRUE)))
  }) %>%
    dplyr::mutate(source_url = stringr::str_remove(.data$source_url, "#.*$")) %>%
    dplyr::distinct()
  if (nrow(links) == 0L || any(!stringr::str_detect(links$source_url, "^https://(?:www\\.)?bcv\\.org\\.ve/"))) {
    rlang::abort(paste("Missing or unexpected PDF links:", page_url), class = "bcv_statement_link_error")
  }
  links
}

validate_bcv_statement_pdf <- function(file_path) {
  # Monthly statements may be one page; HTML responses must never become archived PDFs.
  if (!file.exists(file_path) ||
      !identical(rawToChar(readBin(file_path, "raw", n = 5L)), "%PDF-")) {
    rlang::abort(paste("Invalid BCV PDF signature:", file_path), class = "bcv_statement_pdf_error")
  }
  info <- pdftools::pdf_info(file_path)
  if (info$pages < 1L) {
    rlang::abort(paste("BCV PDF has no pages:", file_path), class = "bcv_statement_pdf_error")
  }
  tibble::tibble(pdf_md5 = unname(tools::md5sum(file_path)),
                 page_count = info$pages, size_bytes = file.info(file_path)$size)
}
