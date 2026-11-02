# Offline checks cover pagination, PDF-link identity, and invalid download responses.
library(tidyverse)
source("R/bcv_statement_helpers.R")

# Sidebar PDFs and pagers must not contaminate the financial-statement archive.
base_url <- "https://www.bcv.org.ve/bcv/estados-financieros"
page <- xml2::read_html(paste0(
  '<a href="/other.pdf">Unrelated</a><a href="?page=99">Sidebar</a>',
  '<div class="view-estados-financieros"><table><tbody>',
  '<tr><td>Balance General al 30/09/2026</td><td>',
  '<a href="/system/files/estados_financieros/septiembre_2026.pdf#page=1">PDF</a>',
  '<a href="/system/files/estados_financieros/septiembre_2026.pdf">PDF</a>',
  '</td></tr></tbody></table><a href="?page=23">Last</a></div>'
))
links <- bcv_statement_links(page, base_url)
stopifnot(bcv_statement_last_page(page, base_url) == 23L, nrow(links) == 1L,
  links$source_url[[1]] == "https://www.bcv.org.ve/system/files/estados_financieros/septiembre_2026.pdf",
  links$title_raw[[1]] == "Balance General al 30/09/2026")

# A disappeared listing structure must fail rather than claim that the archive is empty.
missing_page <- tryCatch(bcv_statement_links(xml2::read_html("<html>Blocked</html>"), base_url),
                          bcv_statement_schema_error = identity)
stopifnot(inherits(missing_page, "bcv_statement_schema_error"))

# A one-page balance sheet is valid; retaining PDF metadata does not extract table values.
pdf_path <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_path)
graphics::plot.new()
graphics::text(0.5, 0.5, "Synthetic single-page balance sheet")
grDevices::dev.off()
metadata <- validate_bcv_statement_pdf(pdf_path)
stopifnot(metadata$page_count[[1]] == 1L, metadata$size_bytes[[1]] > 0,
  metadata$pdf_md5[[1]] == unname(tools::md5sum(pdf_path)))
unlink(pdf_path)

# Non-PDF error pages can never be registered as successful downloads.
html_path <- tempfile(fileext = ".pdf")
writeLines("<html>Access denied</html>", html_path)
invalid_pdf <- tryCatch(validate_bcv_statement_pdf(html_path), bcv_statement_pdf_error = identity)
stopifnot(inherits(invalid_pdf, "bcv_statement_pdf_error"))
unlink(html_path)
message("BCV statement discovery and PDF validation checks passed.")
