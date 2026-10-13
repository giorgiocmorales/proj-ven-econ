# Reusable ggplot figures for the monetary multiplier analysis.

# Plot the observed multiplier together with a 52-week moving average.
build_multiplier_figure <- function(multiplier_data, updated_label) {
  plot_data <- multiplier_data %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      moving_average_52w = slider::slide_dbl(
        .data$value,
        mean,
        .before = 51,
        .complete = TRUE,
        na.rm = TRUE
      )
    )

  upper_limit <- ceiling(max(plot_data$value, na.rm = TRUE) * 2) / 2
  y_breaks <- seq(0, upper_limit, by = 0.5)

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$value, color = "Semanal"),
      linewidth = 0.35,
      alpha = 1
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$moving_average_52w, color = "Promedio m\u00f3vil 52 semanas"),
      linewidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(values = c(
      "Semanal" = "#9AA8B2",
      "Promedio m\u00f3vil 52 semanas" = "#003A5D"
    )) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ","),
      breaks = y_breaks,
      limits = c(0, upper_limit),
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    ggplot2::labs(
      title = "Multiplicador monetario observado",
      subtitle = "Liquidez monetaria/base monetaria",
      caption = paste0("Fuente: BCV; c\u00e1lculos propios. | Actualizado: ", updated_label),
      y = "Veces"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(clip = "off")
}

# Plot statutory reserve coefficients for commercial and universal banks.
build_reserve_rates_figure <- function(reserve_data, updated_label) {
  reserve_data %>%
    dplyr::filter(.data$institution_id == "commercial_and_universal_banks") %>%
    dplyr::mutate(
      series = dplyr::recode(
        .data$variable_id,
        reserve_ratio_ordinary = "Ordinario",
        reserve_ratio_marginal = "Marginal",
        reserve_ratio_special = "Especial"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$value, color = .data$series)) +
    ggplot2::geom_step(linewidth = 0.85, direction = "hv", na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c(
      "Ordinario" = "#003A5D",
      "Marginal" = "#D70036",
      "Especial" = "#B07D24"
    )) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(suffix = "%", accuracy = 1, decimal.mark = ","),
      breaks = scales::breaks_width(10),
      limits = c(0, 100),
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(
      title = "Coeficientes de encaje legal",
      subtitle = "Banca comercial y universal",
      caption = paste0("Fuente: BCV. | Actualizado: ", updated_label),
      y = "Porcentaje"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(clip = "off")
}

# Compare nominal deposits with deposits re-expressed at the latest CPI level.
build_deposits_figure <- function(deposits_data, updated_label) {
  reference_date <- max(deposits_data$cpi_reference_date, na.rm = TRUE)
  plot_data <- deposits_data %>%
    dplyr::filter(!is.na(.data$deposits_real_latest_cpi_ves))
  positive_values <- c(
    plot_data$deposits_nominal_current_ves,
    plot_data$deposits_real_latest_cpi_ves
  )
  positive_values <- positive_values[is.finite(positive_values) & positive_values > 0]
  upper_limit <- ceiling(max(positive_values) / 1e12) * 1e12
  y_breaks <- seq(0, upper_limit, length.out = 6)

  plot_data %>%
    dplyr::select(
      "date",
      "deposits_nominal_current_ves",
      "deposits_real_latest_cpi_ves"
    ) %>%
    tidyr::pivot_longer(
      cols = -"date",
      names_to = "series_id",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      series = dplyr::recode(
        .data$series_id,
        deposits_nominal_current_ves = "Nominales",
        deposits_real_latest_cpi_ves = "Reales"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$date, y = .data$value, color = .data$series)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("Nominales" = "#003A5D", "Reales" = "#D70036")) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        scale = 1e-12,
        suffix = " bill.",
        accuracy = 0.1,
        big.mark = ".",
        decimal.mark = ","
      ),
      breaks = y_breaks,
      limits = c(0, upper_limit),
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(
      title = "Dep\u00f3sitos monetarios nominales y reales",
      subtitle = paste0(
        "VES corrientes y VES constantes a precios de ",
        format(reference_date, "%m-%Y")
      ),
      caption = paste0("Fuente: BCV; c\u00e1lculos propios. | Actualizado: ", updated_label),
      y = "Billones de VES en escala monetaria actual"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(clip = "off")
}

# Compare the net liquidity and monetary-base contributions on a linear scale.
build_aggregate_decomposition_figure <- function(annual_data, updated_label) {
  contributions <- annual_data %>%
    dplyr::filter(dplyr::between(.data$year, 2014, 2025)) %>%
    dplyr::group_by(.data$year, .data$side) %>%
    dplyr::summarise(
      contribution = sum(.data$contribution),
      multiplier_log_change = dplyr::first(.data$multiplier_log_change),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      component = dplyr::recode(
        .data$side,
        liquidity = "Liquidez",
        monetary_base = "Base monetaria"
      )
    )

  total_change <- contributions %>%
    dplyr::distinct(.data$year, .data$multiplier_log_change)
  axis_limit <- ceiling(max(abs(contributions$contribution), na.rm = TRUE) / 100) * 100
  y_breaks <- seq(-axis_limit, axis_limit, by = 100)

  contributions %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$contribution)) +

    ggplot2::geom_col(
      ggplot2::aes(fill = .data$component),
      width = 0.72,
      color = "white",
      linewidth = 0.15
    ) +
    ggplot2::geom_line(
      data = total_change,
      ggplot2::aes(x = .data$year, y = .data$multiplier_log_change),
      inherit.aes = FALSE,
      color = "#D4A72C",
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = total_change,
      ggplot2::aes(x = .data$year, y = .data$multiplier_log_change),
      inherit.aes = FALSE,
      color = "#D4A72C",
      size = 1.7
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Liquidez" = "#003A5D",
      "Base monetaria" = "#D70036"
    )) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ","),
      breaks = y_breaks,
      limits = c(-axis_limit, axis_limit),
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2014, 2025, by = 2)) +
    ggplot2::labs(
      title = "Contribuci\u00f3n neta al cambio del multiplicador",
      subtitle = "Liquidez y base monetaria; dorado: cambio total del multiplicador",
      caption = paste0(
        "Fuente: BCV; c\u00e1lculos propios. La contribuci\u00f3n de la base entra con signo inverso. | Actualizado: ",
        updated_label
      ),
      y = "Puntos porcentuales logar\u00edtmicos"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 1)) +
    ggplot2::coord_cartesian(clip = "off")
}

# Decompose annual multiplier changes into detailed liquidity and base contributions.
build_decomposition_figure <- function(annual_data, updated_label) {
  # Keep the continuous period with a complete breakdown on both sides.
  contributions <- annual_data %>%
    dplyr::filter(
      dplyr::between(.data$year, 2014, 2025),
      !.data$component_id %in% c("liquidity_total", "base_total")
    ) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::filter(dplyr::n() == 8, dplyr::n_distinct(.data$side) == 2) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      component = dplyr::recode(
        .data$component_id,
        liquidity_demand_deposits = "Liq.: Vista",
        liquidity_transferable_savings = "Liq.: Ahorro",
        liquidity_other = "Liq.: Otros",
        base_external_assets = "Base: Externos",
        base_public_sector = "Base: P\u00fablico",
        base_financial_sector = "Base: Financiero",
        base_private_sector = "Base: Privado",
        base_other_internal = "Base: Otros internos"
      ),
      component = factor(
        .data$component,
        levels = c(
          "Liq.: Vista",
          "Liq.: Ahorro",
          "Liq.: Otros",
          "Base: Externos",
          "Base: P\u00fablico",
          "Base: Financiero",
          "Base: Privado",
          "Base: Otros internos"
        )
      )
    ) %>%
    dplyr::mutate(
      direction = .data$contribution >= 0,
      stack_order = -abs(.data$contribution)
    ) %>%
    dplyr::arrange(.data$year, .data$direction, .data$stack_order) %>%
    dplyr::group_by(.data$year, .data$direction) %>%
    dplyr::mutate(
      stack_end = cumsum(.data$contribution),
      stack_start = .data$stack_end - .data$contribution
    ) %>%
    dplyr::ungroup()

  total_change <- contributions %>%
    dplyr::distinct(.data$year, .data$multiplier_log_change)

  contributions %>%
    ggplot2::ggplot() +

    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$year - 0.39,
        xmax = .data$year + 0.39,
        ymin = .data$stack_start,
        ymax = .data$stack_end,
        fill = .data$component
      ),
      color = "white",
      linewidth = 0.12
    ) +
    ggplot2::geom_line(
      data = total_change,
      ggplot2::aes(x = .data$year, y = .data$multiplier_log_change),
      inherit.aes = FALSE,
      color = "#D4A72C",
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = total_change,
      ggplot2::aes(x = .data$year, y = .data$multiplier_log_change),
      inherit.aes = FALSE,
      color = "#D4A72C",
      size = 1.7
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Liq.: Vista" = "#003A5D",
      "Liq.: Ahorro" = "#5A8CA5",
      "Liq.: Otros" = "#A4C3D1",
      "Base: Externos" = "#D70036",
      "Base: P\u00fablico" = "#850022",
      "Base: Financiero" = "#B83455",
      "Base: Privado" = "#E45D78",
      "Base: Otros internos" = "#F0A3B4"
    )) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 1, big.mark = ".", decimal.mark = ","),
      breaks = function(limits) {
        candidate_breaks <- c(-10000, -1000, -100, -10, 0, 10, 100, 1000, 10000)
        interior_breaks <- candidate_breaks[
          candidate_breaks > limits[1] & candidate_breaks < limits[2]
        ]
        sort(unique(c(limits[1], interior_breaks, limits[2])))
      },
      trans = scales::pseudo_log_trans(base = 10, sigma = 10),
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2014, 2025, by = 2)) +
    ggplot2::labs(
      title = "Descomposici\u00f3n del cambio anual del multiplicador",
      subtitle = "Azules: componentes de liquidez; rojos: fuentes de la base; dorado: cambio total",
      caption = paste0(
        "Fuente: BCV; c\u00e1lculos propios. Externos = activos externos netos; otros internos = activos no clasificados + patrimonio.\n",
        "El mayor componente se ubica adyacente a cero; fuentes de la base con signo inverso; escala seudologar\u00edtmica. | Actualizado: ",
        updated_label
      ),
      y = "Puntos porcentuales logarítmicos"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 2, byrow = TRUE)) +
    ggplot2::coord_cartesian(clip = "off")
}

# Show how currency and deposit categories compose total monetary liquidity.
build_liquidity_composition_figure <- function(liquidity_data, updated_label) {
  liquidity_data %>%
    dplyr::filter(.data$variable_id %in% c(
      "monedas_billetes",
      "depositos_vista",
      "depositos_ahorro_transferibles",
      "cuasidinero"
    )) %>%
    dplyr::mutate(month = lubridate::floor_date(.data$date, unit = "month")) %>%
    dplyr::group_by(.data$month, .data$variable_id) %>%
    dplyr::slice_max(order_by = .data$date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$month) %>%
    dplyr::filter(dplyr::n() == 4, all(!is.na(.data$value))) %>%
    dplyr::mutate(share = .data$value / sum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      component = dplyr::recode(
        .data$variable_id,
        monedas_billetes = "Monedas y billetes",
        depositos_vista = "Dep\u00f3sitos a la vista",
        depositos_ahorro_transferibles = "Ahorro transferible",
        cuasidinero = "Cuasidinero"
      )
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$month, y = .data$share, fill = .data$component)) +
    ggplot2::geom_area(position = "stack", linewidth = 0) +
    ggplot2::scale_fill_manual(values = c(
      "Monedas y billetes" = "#9AA8B2",
      "Dep\u00f3sitos a la vista" = "#003A5D",
      "Ahorro transferible" = "#D70036",
      "Cuasidinero" = "#B07D24"
    )) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
      breaks = seq(0, 1, by = 0.25),
      limits = c(0, 1),
      oob = scales::squish,
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::labs(
      title = "Composici\u00f3n de la liquidez monetaria",
      subtitle = "Participaci\u00f3n de sus componentes",
      caption = paste0("Fuente: BCV; c\u00e1lculos propios. | Actualizado: ", updated_label),
      y = "Porcentaje de la liquidez"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(clip = "off")
}

# Show the currency shares of banking deposits using a residual estimate.
build_deposit_currency_composition_figure <- function(deposit_data, updated_label) {
  plot_data <- deposit_data %>%
    dplyr::filter(
      dplyr::between(.data$date, as.Date("2018-01-01"), as.Date("2025-12-31"))
    ) %>%
    dplyr::mutate(month = lubridate::floor_date(.data$date, unit = "month")) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::slice_max(order_by = .data$date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      "month",
      "deposits_in_ves_current_ves",
      "deposits_in_foreign_currency_est_current_ves"
    ) %>%
    tidyr::pivot_longer(
      cols = -"month",
      names_to = "component_id",
      values_to = "value"
    ) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::mutate(share = .data$value / sum(.data$value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      component = dplyr::recode(
        .data$component_id,
        deposits_in_ves_current_ves = "Depósitos en bolívares",
        deposits_in_foreign_currency_est_current_ves = "Depósitos en divisas (estimados)"
      ),
      component = factor(
        .data$component,
        levels = c("Depósitos en bolívares", "Depósitos en divisas (estimados)")
      )
    )

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$month, y = .data$share, fill = .data$component)) +
    ggplot2::geom_area(
      position = ggplot2::position_stack(reverse = TRUE),
      linewidth = 0
    ) +
    ggplot2::scale_fill_manual(values = c(
      "Depósitos en bolívares" = "#003A5D",
      "Depósitos en divisas (estimados)" = "#D70036"
    )) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      oob = scales::squish,
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(
      limits = c(as.Date("2018-01-01"), as.Date("2025-12-31")),
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Composición de los depósitos bancarios",
      subtitle = "Participación de los depósitos a la vista y de ahorro, por moneda",
      caption = paste0(
        "Fuente: BCV; cálculos propios. Divisas = depósitos bancarios menos depósitos en bolívares de la liquidez. | Actualizado: ",
        updated_label
      ),
      y = "Porcentaje de los depósitos"
    ) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 1)) +
    ggplot2::coord_cartesian(clip = "off")
}

# Compare USD-denominated deposit levels with their percentage composition.
build_deposit_currency_composition_panel_figure <- function(deposit_data, fx_data, updated_label) {
  plot_start <- as.Date("2020-03-30")
  plot_end <- as.Date("2025-12-31")

  monthly_deposits <- deposit_data %>%
    dplyr::filter(dplyr::between(.data$date, plot_start, plot_end)) %>%
    dplyr::left_join(
      fx_data %>%
        dplyr::select("date", "rate_smc_adj"),
      by = "date"
    ) %>%
    dplyr::filter(!is.na(.data$rate_smc_adj), .data$rate_smc_adj > 0) %>%
    dplyr::mutate(month = lubridate::floor_date(.data$date, unit = "month")) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::slice_max(order_by = .data$date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  if (nrow(monthly_deposits) < 60) {
    stop("Deposit and official exchange-rate series have insufficient common coverage.", call. = FALSE)
  }

  plot_data <- monthly_deposits %>%
    dplyr::select(
      "month",
      "rate_smc_adj",
      "deposits_in_ves_current_ves",
      "deposits_in_foreign_currency_est_current_ves"
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("deposits_"),
      names_to = "component_id",
      values_to = "value_current_ves"
    ) %>%
    dplyr::group_by(.data$month) %>%
    dplyr::mutate(
      value_usd = .data$value_current_ves / .data$rate_smc_adj,
      share = .data$value_current_ves / sum(.data$value_current_ves)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      component = dplyr::recode(
        .data$component_id,
        deposits_in_ves_current_ves = "En bolívares",
        deposits_in_foreign_currency_est_current_ves = "En divisas (estimados)"
      ),
      component = factor(
        .data$component,
        levels = c("En bolívares", "En divisas (estimados)")
      )
    )

  usd_totals <- plot_data %>%
    dplyr::group_by(.data$month) %>%
    dplyr::summarise(total_usd = sum(.data$value_usd), .groups = "drop")
  usd_upper_limit <- ceiling(max(usd_totals$total_usd, na.rm = TRUE) / 1e9) * 1e9
  usd_breaks <- seq(0, usd_upper_limit, length.out = 6)
  palette <- c(
    "En bolívares" = "#003A5D",
    "En divisas (estimados)" = "#D70036"
  )

  usd_plot <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$month,
      y = .data$value_usd,
      fill = .data$component
    )) +
    ggplot2::geom_area(
      position = ggplot2::position_stack(reverse = TRUE),
      linewidth = 0
    ) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        scale = 1e-9,
        accuracy = 0.1,
        big.mark = ".",
        decimal.mark = ","
      ),
      breaks = usd_breaks,
      limits = c(0, usd_upper_limit),
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(
      limits = c(plot_start, plot_end),
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Depósitos bancarios en USD",
      subtitle = "Conversión al tipo de cambio oficial",
      y = "Miles de millones de USD"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::coord_cartesian(clip = "off")

  share_plot <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$month,
      y = .data$share,
      fill = .data$component
    )) +
    ggplot2::geom_area(
      position = ggplot2::position_stack(reverse = TRUE),
      linewidth = 0
    ) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::geom_hline(yintercept = 0, color = "#687179", linewidth = 0.25, linetype = "solid") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
      breaks = seq(0, 1, by = 0.2),
      limits = c(0, 1),
      oob = scales::squish,
      expand = c(0, 0),
      sec.axis = ven_blank_secondary_axis()
    ) +
    ggplot2::scale_x_date(
      limits = c(plot_start, plot_end),
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Composición por moneda",
      subtitle = "Participación porcentual",
      y = "Porcentaje de los depósitos"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::coord_cartesian(clip = "off")

  # Build one common legend and source line for the two aligned panels.
  legend_plot <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$month,
      y = .data$share,
      fill = .data$component
    )) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 1, byrow = TRUE)) +
    ggplot2::theme_void(base_family = ven_chart_specs$base_family) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = ven_chart_specs$base_size - 1),
      legend.key.height = grid::unit(0.16, "in")
    )
  legend_table <- ggplot2::ggplotGrob(legend_plot)
  legend_index <- which(legend_table$layout$name == "guide-box-bottom")

  if (length(legend_index) != 1) {
    stop("Unable to extract the common deposit-composition legend.", call. = FALSE)
  }

  legend_grob <- legend_table$grobs[[legend_index]]
  panels_grob <- gridExtra::arrangeGrob(usd_plot, share_plot, ncol = 2)
  source_grob <- grid::textGrob(
    paste0("Fuente: BCV; cálculos propios. | ", updated_label),
    x = 0.5,
    hjust = 0.5,
    gp = grid::gpar(
      col = "grey35",
      fontsize = ven_chart_specs$base_size - 3,
      fontfamily = ven_chart_specs$base_family
    )
  )

  gridExtra::arrangeGrob(
    panels_grob,
    legend_grob,
    source_grob,
    ncol = 1,
    heights = grid::unit.c(
      grid::unit(1, "null"),
      grid::unit(0.34, "in"),
      grid::unit(0.22, "in")
    )
  )
}

# Join liquidity composition with nominal and real monetary deposits.
build_liquidity_deposit_summary_panel_figure <- function(
    liquidity_data,
    deposits_real_data,
    liquidity_updated_label,
    prices_updated_label) {
  plot_start <- as.Date("2011-06-24")
  plot_end <- as.Date("2025-12-31")

  composition_plot <- build_liquidity_composition_figure(
    liquidity_data %>%
      dplyr::filter(dplyr::between(.data$date, plot_start, plot_end)),
    liquidity_updated_label
  ) +
    ggplot2::labs(
      title = "Composición de la liquidez",
      subtitle = "Participación de sus componentes",
      caption = NULL
    ) +
    ggplot2::scale_x_date(
      limits = c(plot_start, plot_end),
      breaks = seq(plot_start, plot_end, by = "3 years"),
      date_labels = "%Y",
      expand = c(0, 0)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 2, byrow = TRUE))

  deposits_plot <- build_deposits_figure(
    deposits_real_data %>%
      dplyr::filter(dplyr::between(.data$date, plot_start, plot_end)),
    prices_updated_label
  ) +
    ggplot2::labs(
      title = "Depósitos nominales y reales",
      subtitle = "Depósitos monetarios de la liquidez",
      caption = NULL
    ) +
    ggplot2::scale_x_date(
      limits = c(plot_start, plot_end),
      breaks = seq(plot_start, plot_end, by = "3 years"),
      date_labels = "%Y",
      expand = c(0, 0)
    ) +
    ggplot2::guides(color = ggplot2::guide_legend(order = 1, nrow = 1, byrow = TRUE))

  panels_grob <- gridExtra::arrangeGrob(composition_plot, deposits_plot, ncol = 2)
  source_grob <- grid::textGrob(
    paste0(
      "Fuente: BCV; cálculos propios. | Composición: ",
      liquidity_updated_label,
      "; precios: ",
      prices_updated_label
    ),
    x = 0.5,
    hjust = 0.5,
    gp = grid::gpar(
      col = "grey35",
      fontsize = ven_chart_specs$base_size - 3,
      fontfamily = ven_chart_specs$base_family
    )
  )

  gridExtra::arrangeGrob(
    panels_grob,
    source_grob,
    ncol = 1,
    heights = grid::unit.c(
      grid::unit(1, "null"),
      grid::unit(0.24, "in")
    )
  )
}
