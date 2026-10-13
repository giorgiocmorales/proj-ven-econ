# Build compact, presentation-ready tables for the Venezuela macro report scaffold.

build_executive_table <- function(data, closing_year) {
  # Keep the executive indicator inventory explicit while forecasting is unfinished.
  indicators <- tibble::tribble(
    ~variable_id, ~Indicador, ~Unidad,
    "gdp_real_growth_bcv", "Crecimiento del PIB real", "% anual",
    "inflation_calendar_year", "Inflaci\u00f3n general", "% anual",
    "official_fx_bcv", "Tipo de cambio oficial", "Bs./USD",
    "alternative_fx_yadio", "Tipo de cambio alternativo", "Bs./USD",
    "monetary_base", "Base monetaria", "Bn Bs.",
    "monetary_liquidity", "Liquidez monetaria", "Bn Bs.",
    "oil_production_secondary", "Producci\u00f3n petrolera (fuentes secundarias)", "Mil b/d",
    "oil_price_merey", "Precio del Merey", "USD/b",
    "oil_price_brent", "Precio del Brent", "USD/b",
    "oil_price_wti", "Precio del WTI", "USD/b",
    "goods_exports_fob", "Exportaciones de bienes", "MM USD",
    "goods_imports_fob", "Importaciones de bienes", "MM USD",
    "international_reserves", "Reservas internacionales", "MM USD",
    "fx_intervention_amount", "Intervenciones cambiarias", "MM USD",
    "fiscal_revenue_gdp", "Ingresos del gobierno general", "% del PIB",
    "fiscal_expenditure_gdp", "Gastos del gobierno general", "% del PIB",
    "fiscal_balance_gdp", "Balance del gobierno general (FMI)", "% del PIB",
    "seniat_revenue", "Recaudaci\u00f3n tributaria", "MM USD"
  )

  # Stocks and prices require December observations; annual flows retain annual totals.
  closing_values <- data %>%
    filter(.data$variable_id %in% indicators$variable_id,
           lubridate::year(.data$date) == closing_year,
           .data$frequency == "annual" | lubridate::month(.data$date) == 12L) %>%
    group_by(.data$variable_id) %>%
    slice_max(.data$date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(variable_id, closing_value = scales::number(
      .data$value, accuracy = 0.1, big.mark = ".", decimal.mark = ","
    ))

  # Leave both model columns empty of numbers until forecasts are explicitly produced.
  indicators %>%
    left_join(closing_values, by = "variable_id") %>%
    transmute(Indicador, Unidad,
      !!paste(closing_year, "cierre") := tidyr::replace_na(.data$closing_value, "Pendiente"),
      !!paste(closing_year + 1L, "estimado") := "Pendiente",
      !!paste(closing_year + 2L, "proyectado") := "Pendiente")
}

build_external_table <- function(data, first_year, last_year) {
  external_data <- data %>%
    filter(
      .data$variable_id %in% c("goods_exports_fob", "goods_imports_fob"),
      lubridate::year(.data$date) >= first_year,
      lubridate::year(.data$date) <= last_year
    ) %>%
    mutate(
      year = as.character(lubridate::year(.data$date)),
      series = if_else(
        .data$variable_id == "goods_exports_fob",
        "Exports",
        "Imports"
      )
    ) %>%
    select(.data$year, .data$series, .data$value) %>%
    pivot_wider(names_from = .data$series, values_from = .data$value) %>%
    arrange(.data$year)

  if (nrow(external_data) == 0) {
    stop("Report data contains no goods-trade observations for the external table", call. = FALSE)
  }

  external_data
}
