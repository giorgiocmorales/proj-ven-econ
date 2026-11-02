# Resolve the rolling MOMR endpoint and validate dated PDFs before archiving them.
opec_publication_month <- function(text) {
  # Match English publication months independently of the host's locale.
  label <- stringr::str_extract(text, stringr::regex(
    paste0("\\b(?:", paste(month.name, collapse = "|"), ")\\s+[12][0-9]{3}\\b"),
    ignore_case = TRUE))
  if (length(label) != 1L || is.na(label)) {
    rlang::abort("Cannot identify the MOMR publication month", class = "opec_publication_month_error")
  }
  month_id <- match(tolower(stringr::word(label, 1L)), tolower(month.name))
  as.Date(sprintf("%s-%02d-01", stringr::word(label, 2L), month_id))
}

opec_latest_report_from_html <- function(page, page_url) {
  # The official download page names the current issue and links its delivery endpoint.
  heading <- rvest::html_elements(page, "h1") %>% rvest::html_text2()
  heading <- heading[stringr::str_detect(heading, stringr::regex("Monthly Oil Market Report", ignore_case = TRUE))]
  if (length(heading) != 1L) {
    rlang::abort("Expected one latest MOMR heading", class = "opec_latest_schema_error")
  }
  links <- rvest::html_elements(page, "a[href]") %>% rvest::html_attr("href") %>%
    xml2::url_absolute(page_url)
  links <- unique(links[stringr::str_detect(links, "/res/pdf_delivery_momr\\.php(?:\\?|$)")])
  if (length(links) != 1L) {
    rlang::abort("Expected one latest MOMR delivery link", class = "opec_latest_schema_error")
  }
  tibble::tibble(report_month = opec_publication_month(heading), pdf_url = links[[1]])
}

discover_opec_latest_report <- function(timeout_seconds) {
  # Fetch once per discovery call; callers record failures and retain archive candidates.
  page_url <- "https://momr.opec.org/pdf-download/"
  response <- httr::GET(page_url, httr::timeout(timeout_seconds))
  httr::stop_for_status(response)
  opec_latest_report_from_html(xml2::read_html(httr::content(response, as = "raw")), page_url)
}

validate_opec_report_pdf <- function(file_path, report_month) {
  # A rolling endpoint may return HTML or another issue; neither can replace the archive.
  if (!identical(rawToChar(readBin(file_path, "raw", n = 5L)), "%PDF-") ||
      pdftools::pdf_info(file_path)$pages <= 4L) {
    rlang::abort("Response is not a valid multi-page MOMR PDF", class = "opec_pdf_validation_error")
  }
  cover_month <- opec_publication_month(pdftools::pdf_text(file_path)[[1]])
  if (cover_month != report_month) {
    rlang::abort(sprintf("MOMR cover month %s does not match requested month %s", cover_month, report_month),
                 class = "opec_pdf_month_error")
  }
  invisible(TRUE)
}

download_opec_report_pdf <- function(urls, file_path, report_month, max_attempts, timeout_seconds) {
  # Try candidates in order and retain each failure even when a fallback succeeds.
  failures <- character()
  for (url in unique(urls)) {
    attempt <- tryCatch({
      download_latest_file(url = url, dest_path = file_path,
                           max_attempts = max_attempts, timeout_seconds = timeout_seconds)
      validate_opec_report_pdf(file_path = file_path, report_month = report_month)
    }, error = identity)
    if (!inherits(attempt, "error")) {
      return(list(download_url = url, attempt_errors = paste(failures, collapse = "\n")))
    }
    failures <- c(failures, paste(url, conditionMessage(attempt), sep = ": "))
    unlink(file_path)
  }
  rlang::abort(paste(c("No candidate provided the requested MOMR PDF", failures), collapse = "\n"),
               class = "opec_download_error")
}
