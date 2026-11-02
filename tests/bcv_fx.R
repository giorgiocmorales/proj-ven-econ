# Offline tests protect source schema, locale-aware rates, and conflicting observations.
library(tidyverse)
source("R/source_helpers.R")
source("R/bcv_fx_helpers.R")

# XLS exports are HTML, and thousands separators must not inflate or truncate rates.
intervention <- xml2::read_html(paste0(
  '<table><tr><th>Fecha de Intervenci&#243;n</th><th>Nro. de Intervenci&#243;n</th>',
  '<th>Tipo de Cambio Bs./EUR</th></tr>',
  '<tr><td>19-07-2021</td><td>029-21</td><td>4.190.665,15</td></tr>',
  '<tr><td>19-07-2021</td><td>029-21</td><td>4.055.688,42</td></tr>',
  '<tr><td>19-07-2021</td><td>029-21</td><td>4.055.688,42</td></tr></table>'))
parsed <- bcv_fx_table(intervention, "intervention")
stopifnot(nrow(parsed) == 2L, setequal(parsed$rate, c(4190665.15, 4055688.42)),
  all(parsed$date == as.Date("2021-07-19")))

# Preserve source buy/sell ordering even when the reported buy rate exceeds the sell rate.
bank_html <- paste0('<table><tr><th>Fecha del Indicador</th><th>Banco</th>',
  '<th>Compra</th><th>Venta</th></tr><tr><td>16-10-2026</td><td>Banesco</td>',
  '<td>880,3839</td><td>879,9023</td></tr></table>')
bank <- bcv_fx_table(xml2::read_html(bank_html), "banks")
stopifnot(bank$bid == 880.3839, bank$ask == 879.9023)

# Historical one-sided quotes are retained as missing, without interpolating a rate.
one_sided <- bcv_fx_table(xml2::read_html(sub("880,3839", "-", bank_html, fixed = TRUE)), "banks")
stopifnot(is.na(one_sided$bid), one_sided$ask == 879.9023)

# Error pages and malformed rates must fail before any processed extract is replaced.
blocked <- tryCatch(bcv_fx_table(xml2::read_html('<html>Access denied</html>'), "banks"),
  bcv_fx_schema_error = identity)
invalid <- tryCatch(suppressWarnings(bcv_fx_table(xml2::read_html(
  sub("880,3839", "unavailable", bank_html, fixed = TRUE)), "banks")), bcv_fx_value_error = identity)
stopifnot(inherits(blocked, "bcv_fx_schema_error"), inherits(invalid, "bcv_fx_value_error"))
partial <- tryCatch(bcv_fx_table(xml2::read_html(
  sub("880,3839", "880,3839invalid", bank_html, fixed = TRUE)), "banks"), bcv_fx_value_error = identity)
stopifnot(inherits(partial, "bcv_fx_value_error"))

# Date filters must be explicit and independent of the host's language settings.
request <- bcv_fx_export_request("https://www.bcv.org.ve/cambiaria/export/intervencion-cambiaria",
  as.Date("2019-05-01"), as.Date("2019-05-31"))
stopifnot(grepl("01%20Mayo%202019", request$url, fixed = TRUE),
  grepl("31%20Mayo%202019", request$url, fixed = TRUE))
message("BCV FX schema, quote preservation, and export-filter checks passed.")
