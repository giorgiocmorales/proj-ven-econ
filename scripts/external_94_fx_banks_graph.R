# Compare official, intervention, and all reported bank quotes during 2025-2026.
# Use cached pipeline outputs and the report cutoff; do not download or fill bank quotes.
library(tidyverse)
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  if (!nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) stop("UTF-8 locale required")
}
source("R/report_figures.R", encoding = "UTF-8")
source("R/report_inputs.R")
config <- read_report_config(path = Sys.getenv("REPORT_CONFIG", "config/report.yml"))
# The optional test variant preserves the approved comparison and exports separate files.
test_variant <- Sys.getenv("FX_BANKS_TEST", "false")
if (!test_variant %in% c("true", "false")) stop("FX_BANKS_TEST must be true or false")
test_variant <- test_variant == "true"
start_date <- as.Date(if (test_variant) "2025-07-01" else "2025-01-01")
output_suffix <- if (test_variant) "_test" else ""
cutoff <- min(as.Date(config$params$data_cutoff), as.Date("2026-12-31"))
if (cutoff < start_date) stop("Report cutoff precedes the 2025-2026 chart window")

# Keep both bank quote sides and original institution labels, including other institutions.
banks <- readr::read_csv("data/final/external_10_fx_banks_bcv.csv", show_col_types = FALSE) %>%
  filter(date >= start_date, date <= cutoff)
fx <- readr::read_csv("data/final/external_fx.csv", show_col_types = FALSE) %>%
  filter(date >= start_date, date <= cutoff) %>%
  select(date, official = rate_smc_adj, intervention = rate_int_adj, unofficial = rate_yad)
if (!test_variant) fx <- fx %>% select(-unofficial)
if (!nrow(banks) || !nrow(fx) || anyNA(banks[c("date", "bank", "currency")]) ||
    any(banks$currency != "USD") || anyNA(fx$date) || anyDuplicated(fx$date) ||
    anyDuplicated(banks[c("date", "bank", "bid", "ask")])) {
  stop("Bank comparison requires nonempty USD sources with valid dates and unique quote keys")
}
bank_lines <- banks %>%
  filter(!source_key_conflict) %>% select(date, bank, bid, ask) %>%
  pivot_longer(c(bid, ask), names_to = "quote_side", values_to = "value") %>%
  mutate(series = "Bancos: compra y venta")
reference_lines <- fx %>% pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(series = recode(series, official = "Oficial (BCV)", intervention = "Intervención (BCV)",
    unofficial = "No oficial (Yadio)"))
if (any(!is.na(c(bank_lines$value, reference_lines$value)) &
    (!is.finite(c(bank_lines$value, reference_lines$value)) | c(bank_lines$value, reference_lines$value) <= 0))) {
  stop("Bank comparison quotes must be positive and finite where reported")
}

# Keep missing quotes in the saved data; line groups prevent bridging long publication gaps.
bank_lines <- bank_lines %>% arrange(bank, quote_side, date) %>%
  group_by(bank, quote_side) %>%
  mutate(segment = cumsum(coalesce(as.integer(date - lag(date)) > 14L, FALSE))) %>% ungroup()
intervention_line <- reference_lines %>% filter(series == "Intervención (BCV)", is.finite(value)) %>%
  arrange(date) %>% mutate(segment = cumsum(coalesce(as.integer(date - lag(date)) > 14L, FALSE)))
if (!nrow(intervention_line) || !any(is.finite(fx$official))) stop("Missing highlighted FX observations")
last_date <- max(c(bank_lines$date, reference_lines$date))
date_breaks <- sort(unique(c(start_date, seq.Date(start_date, last_date, by = "6 months"), last_date)))
date_breaks <- date_breaks[date_breaks == last_date | as.integer(last_date - date_breaks) >= 60L]

# Draw every institution in grey behind the highlighted reference and intervention series.
figure <- ggplot() +
  geom_line(data = bank_lines, aes(date, value, color = series,
    group = interaction(bank, quote_side, segment)), linewidth = 0.35, na.rm = TRUE) +
  geom_line(data = filter(reference_lines, series %in% c("Oficial (BCV)", "No oficial (Yadio)")),
    aes(date, value, color = series), linewidth = 0.7) +
  geom_line(data = intervention_line, aes(date, value, color = series, group = segment), linewidth = 0.7) +
  scale_color_manual(values = c("Oficial (BCV)" = report_palette[["navy"]],
    "Intervención (BCV)" = report_palette[["cyan"]], "Bancos: compra y venta" = "#c8c8c8",
    "No oficial (Yadio)" = report_palette[["red"]]),
    breaks = c("Oficial (BCV)", "Intervención (BCV)", if (test_variant) "No oficial (Yadio)", "Bancos: compra y venta")) +
  scale_x_date(breaks = date_breaks, labels = scales::label_date("%m/%Y"),
    limits = c(start_date, last_date), expand = expansion(mult = c(0.01, 0.02))) +
  geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    limits = ~range(pretty(c(.x, 0), n = 7)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))), expand = expansion(mult = 0)) +
  labs(title = if (test_variant) "Venezuela: tipos de cambio" else "Venezuela: tipos de cambio del BCV y la banca",
    subtitle = paste0("Series diarias | ", format(start_date, "%d/%m/%Y"), "–", format(last_date, "%d/%m/%Y")),
    x = NULL, y = "Bolívares por USD",
    caption = paste0("Notas: gris: compra y venta por institución, incluida «Otras Instituciones», sin rellenar valores faltantes.\n",
      "Intervención convertida con el cruce BCV de la misma fecha valor; huecos de más de 14 días interrumpen las líneas.\n",
      "Fuentes: BCV (SMC, intervención cambiaria y tasas informativas del sistema bancario)",
      if (test_variant) " y Yadio (tipo implícito)." else ".")) +
  report_chart_theme

# Export the exact plotted inputs and both report-ready figure formats.
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(bind_rows(bank_lines, reference_lines), paste0("outputs/data/external_94_fx_banks_graph", output_suffix, ".csv"))
for (extension in c("png", "pdf")) {
  ggsave(paste0("outputs/figures/external_fx_banks_2025_2026", output_suffix, ".", extension), figure,
    width = 10, height = 5.625, units = "in", dpi = 300,
    device = if (extension == "png") ragg::agg_png else grDevices::cairo_pdf, bg = "white")
}
stopifnot(all(lubridate::year(bank_lines$date) %in% c(2025L, 2026L)),
  all(lubridate::year(reference_lines$date) %in% c(2025L, 2026L)),
  setequal(unique(bank_lines$bank), unique(banks$bank[!banks$source_key_conflict])))
message("Exported 2025-2026 FX comparison: ", n_distinct(bank_lines$bank),
  " institutions; both quote sides; cutoff ", last_date, ".")
