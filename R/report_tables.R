# Build compact, presentation-ready tables for the Venezuela macro report scaffold.

build_executive_table <- function(data, first_year, last_year) {
  annual_data <- data %>%
    filter(
      .data$variable_id %in% c(
        "gdp_real_growth_bcv", "gdp_real_growth_imf", "inflation_calendar_year",
        "goods_exports_fob", "goods_imports_fob", "fiscal_balance_gdp"
      ),
      lubridate::year(.data$date) >= first_year,
      lubridate::year(.data$date) <= last_year
    ) %>%
    mutate(
      table_label = case_when(
        .data$variable_id == "gdp_real_growth_bcv" ~ "Real GDP growth (BCV)",
        .data$variable_id == "gdp_real_growth_imf" ~ "Real GDP growth (IMF reference)",
        .data$variable_id == "inflation_calendar_year" ~ "Calendar-year inflation",
        .data$variable_id == "goods_exports_fob" ~ "Goods exports f.o.b.",
        .data$variable_id == "goods_imports_fob" ~ "Goods imports f.o.b.",
        .data$variable_id == "fiscal_balance_gdp" ~ "Fiscal balance"
      ),
      year = as.character(lubridate::year(.data$date)),
      formatted_value = case_when(
        .data$unit %in% c("percent_yoy", "percent_GDP") ~ scales::number(.data$value, accuracy = 0.1, suffix = "%"),
        .data$unit == "million_USD" ~ scales::number(.data$value / 1000, accuracy = 0.1, suffix = " bn"),
        TRUE ~ scales::number(.data$value, accuracy = 0.1)
      )
    ) %>%
    select(.data$table_label, .data$year, .data$formatted_value)

  if (nrow(annual_data) == 0) {
    stop("Report data contains no annual observations for the executive table", call. = FALSE)
  }

  annual_data %>%
    distinct() %>%
    pivot_wider(
      names_from = .data$year,
      values_from = .data$formatted_value,
      values_fill = "-"
    ) %>%
    mutate(across(-.data$table_label, ~ tidyr::replace_na(.x, "-"))) %>%
    arrange(match(
      .data$table_label,
      c(
        "Real GDP growth (BCV)", "Real GDP growth (IMF reference)",
        "Calendar-year inflation", "Goods exports f.o.b.", "Goods imports f.o.b.",
        "Fiscal balance"
      )
    ))
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
