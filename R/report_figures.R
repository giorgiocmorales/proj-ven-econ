# Build source-aware report charts with the growth-scenarios visual baseline.
# Trends compare GDP sources, monthly prices, daily FX, and oil indicators.
# Annual goods and fiscal columns compare yearly magnitudes against zero.
# Word/PNG exports share plots; facets retain independent units and colors identify sources.
# Line graphs use solid lines without points; captions place notes above sources.

# Baseline: proj-ven-growth-scenarios/scripts/_presentation_theme.R.
# Keep the report self-contained; scale the presentation typography to a Word page.
report_palette <- c(
  navy = "#003a5d", red = "#d70036", cyan = "#00acc8",
  green = "#157549", yellow = "#f1ba0d", orange = "#f26122"
)

# Reuse a ggplot theme object rather than wrapping package functionality in a function.
report_chart_theme <- theme_minimal(base_family = "serif", base_size = 10) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(color = "#2b3033"),
    plot.title = element_text(size = 12, face = "bold", hjust = 0, margin = margin(b = 3)),
    plot.subtitle = element_text(size = 9, hjust = 0, margin = margin(b = 6)),
    plot.caption = element_text(size = 7.5, face = "italic", hjust = 0,
                                lineheight = 1.15, margin = margin(t = 6)),
    axis.text = element_text(size = 8, color = "#2b3033"),
    axis.title = element_text(size = 9),
    axis.line.x = element_line(color = "#2b3033", linewidth = 0.35, linetype = "solid"),
    axis.line.y = element_line(color = "#2b3033", linewidth = 0.35, linetype = "solid"),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(0, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#d6dde2", linewidth = 0.3),
    strip.text = element_text(size = 9, hjust = 0),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8.5),
    legend.key.width = grid::unit(1.1, "lines"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(6, 12, 6, 8)
  )

read_report_series <- function(file_path) {
  report_data <- readr::read_csv(file_path, show_col_types = FALSE)

  required_columns <- c(
    "variable_id", "section", "display_label", "date", "frequency", "value",
    "unit", "observation_role", "source_id", "dataset_id"
  )

  missing_columns <- setdiff(required_columns, names(report_data))

  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Report series file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Reject malformed values and ambiguous observation keys even on a direct render.
  if (!inherits(report_data$date, "Date") || anyNA(report_data$date) ||
      !is.numeric(report_data$value) || any(!is.finite(report_data$value)) ||
      anyDuplicated(report_data[c("variable_id", "date", "observation_role")]) > 0) {
    rlang::abort("Report data contains invalid dates, non-finite values, or duplicate keys", class = "report_data_error")
  }

  report_data
}

filter_report_series <- function(data, variable_ids, frequencies, observation_roles) {
  filtered_data <- data %>%
    filter(
      .data$variable_id %in% variable_ids,
      .data$frequency %in% frequencies,
      .data$observation_role %in% observation_roles
    )

  missing_variables <- setdiff(variable_ids, unique(filtered_data$variable_id))

  if (length(missing_variables) > 0) {
    stop(
      sprintf(
        "Report data is missing required variables: %s",
        paste(missing_variables, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  filtered_data
}

build_growth_figure <- function(data) {
  growth_data <- filter_report_series(
    data = data,
    variable_ids = c("gdp_real_growth_bcv", "gdp_real_growth_imf"),
    frequencies = "annual",
    observation_roles = c("actual", "reference")
  ) %>%
    mutate(series = if_else(
      .data$variable_id == "gdp_real_growth_bcv",
      "BCV observado",
      "FMI referencia"
    ))

  ggplot(growth_data, aes(x = .data$date, y = .data$value, color = .data$series)) +

    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c("BCV observado" = report_palette[["navy"]], "FMI referencia" = report_palette[["red"]])) +
    geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
      limits = ~range(scales::breaks_width(5)(range(c(.x, 0)))),
      breaks = scales::breaks_width(5),
      expand = expansion(mult = 0)
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = "Crecimiento real del PIB",
      subtitle = "Variaci\u00f3n interanual; observaciones BCV y referencia FMI",
      x = NULL,
      y = "Porcentaje",
      caption = "Notas: la serie del FMI es una referencia externa.\nFuentes: BCV y FMI."
    ) +
    report_chart_theme
}

build_inflation_figure <- function(data) {
  inflation_data <- filter_report_series(
    data = data,
    variable_ids = c("inflation_mom", "inflation_yoy"),
    frequencies = "monthly",
    observation_roles = "actual"
  ) %>%
    mutate(series = if_else(
      .data$variable_id == "inflation_yoy",
      "Inflaci\u00f3n interanual",
      "Inflaci\u00f3n mensual"
    ))

  ggplot(inflation_data, aes(x = .data$date, y = .data$value, color = .data$series)) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c(
      "Inflaci\u00f3n interanual" = report_palette[["navy"]],
      "Inflaci\u00f3n mensual" = report_palette[["red"]]
    )) +
    facet_wrap(~series, scales = "free_y", ncol = 2) +
    geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
      limits = ~range(pretty(c(.x, 0), n = 7)),
      breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
      expand = expansion(mult = 0)
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = "Inflaci\u00f3n",
      subtitle = "Variaciones del IPC; escalas independientes",
      x = NULL,
      y = "Porcentaje",
      caption = "Notas: c\u00e1lculos a partir del \u00edndice de precios al consumidor.\nFuentes: BCV."
    ) +
    report_chart_theme + theme(legend.position = "none")
}

build_fx_figure <- function(data) {
  fx_data <- filter_report_series(
    data = data,
    variable_ids = c("official_fx_bcv", "alternative_fx_yadio"),
    frequencies = "daily",
    observation_roles = "actual"
  ) %>%
    mutate(series = if_else(
      .data$variable_id == "official_fx_bcv",
      "TC oficial",
      "TC alternativo"
    ))

  ggplot(fx_data, aes(x = .data$date, y = .data$value, color = .data$series)) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = c("TC oficial" = report_palette[["navy"]], "TC alternativo" = report_palette[["red"]])) +
    geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      limits = ~range(pretty(c(.x, 0), n = 7)),
      breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
      expand = expansion(mult = 0)
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = "Tipo de cambio",
      subtitle = "Bol\u00edvares actuales por d\u00f3lar estadounidense",
      x = NULL,
      y = "VES/USD",
      caption = "Fuentes: BCV y Yadio."
    ) +
    report_chart_theme
}

# Compare annual goods flows using the same units and a common zero baseline.
build_external_figure <- function(data) {
  external_data <- filter_report_series(
    data = data,
    variable_ids = c("goods_exports_fob", "goods_imports_fob"),
    frequencies = "annual",
    observation_roles = "actual"
  ) %>%
    mutate(series = if_else(.data$variable_id == "goods_exports_fob", "Exportaciones", "Importaciones"))

  ggplot(external_data, aes(x = factor(lubridate::year(.data$date)), y = .data$value / 1000, fill = .data$series)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("Exportaciones" = report_palette[["navy"]], "Importaciones" = report_palette[["red"]])) +
    geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      limits = ~range(pretty(c(.x, 0), n = 7)),
      breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
      expand = expansion(mult = 0)
    ) +
    labs(
      title = "Comercio exterior de bienes",
      subtitle = "Exportaciones e importaciones anuales, f.o.b.",
      x = NULL, y = "Miles de millones de USD",
      caption = "Notas: importaciones expresadas como magnitudes positivas.\nFuentes: BCV."
    ) +
    report_chart_theme
}

build_oil_figure <- function(data) {
  oil_data <- filter_report_series(
    data = data,
    variable_ids = c("oil_production_secondary", "oil_price_merey"),
    frequencies = "monthly",
    observation_roles = "actual"
  ) %>%
    mutate(series = if_else(
      .data$variable_id == "oil_production_secondary",
      "Producci\u00f3n (miles de barriles diarios)",
      "Precio Merey (USD por barril)"
    ))

  ggplot(oil_data, aes(x = .data$date, y = .data$value, color = .data$series)) +
    geom_line(linewidth = 0.7, show.legend = FALSE) +
    facet_wrap(~factor(series, levels = c("Producci\u00f3n (miles de barriles diarios)", "Precio Merey (USD por barril)")), scales = "free_y", ncol = 1) +
    scale_color_manual(values = c(
      "Producci\u00f3n (miles de barriles diarios)" = report_palette[["navy"]],
      "Precio Merey (USD por barril)" = report_palette[["red"]]
    )) +
    # Annual labels keep the expanded historical corpus readable on a report page.
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    scale_y_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ","),
      limits = ~range(pretty(c(.x, 0), n = 7)),
      breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
      expand = expansion(mult = 0)
    ) +
    labs(
      title = "Producci\u00f3n petrolera y precio del crudo",
      subtitle = "Observaciones mensuales; escalas independientes",
      x = NULL,
      y = NULL,
      caption = "Notas: producci\u00f3n seg\u00fan fuentes secundarias.\nFuentes: OPEP."
    ) +
    report_chart_theme
}

build_fiscal_figure <- function(data) {
  fiscal_data <- filter_report_series(
    data = data,
    variable_ids = "fiscal_balance_gdp",
    frequencies = "annual",
    observation_roles = "reference"
  )

  ggplot(fiscal_data, aes(x = .data$date, y = .data$value)) +

    geom_col(fill = report_palette[["navy"]], width = 240) +
    geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%", big.mark = ".", decimal.mark = ","),
      limits = ~range(pretty(c(.x, 0), n = 7)),
      breaks = ~sort(unique(c(.x, pretty(.x, n = 7)))),
      expand = expansion(mult = 0)
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = "Balance fiscal del gobierno general",
      subtitle = "Estimaci\u00f3n del FMI como proporci\u00f3n del PIB",
      x = NULL,
      y = "Porcentaje del PIB",
      caption = "Notas: referencia externa; no corresponde a una estimaci\u00f3n propia.\nFuentes: FMI."
    ) +
    report_chart_theme
}
