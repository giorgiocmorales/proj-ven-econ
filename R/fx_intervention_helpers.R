# Convert intervention quotes using BCV reference rates for the same value date.
convert_bcv_intervention <- function(smc, interventions) {
  # Never choose an arbitrary reference quote when a date/currency key conflicts.
  reference <- smc %>%
    dplyr::filter(.data$currency %in% c("USD", "EUR")) %>%
    dplyr::select(date = "fecha_valor", "currency", "ask") %>% dplyr::distinct()
  if (anyNA(reference) || any(!is.finite(reference$ask) | reference$ask <= 0) ||
      anyDuplicated(reference[c("date", "currency")])) {
    rlang::abort("Invalid or conflicting BCV USD/EUR reference quotes", class = "fx_cross_rate_error")
  }
  if (!all(c("USD", "EUR") %in% reference$currency) ||
      anyNA(interventions[c("date", "intervention_id", "rate", "currency")]) ||
      any(interventions$currency != "EUR") || any(!is.finite(interventions$rate) | interventions$rate <= 0)) {
    rlang::abort("Missing reference currencies or invalid intervention quotes", class = "fx_intervention_value_error")
  }
  cross <- reference %>% tidyr::pivot_wider(names_from = "currency", values_from = "ask") %>%
    dplyr::transmute(date = .data$date, bcv_bs_usd = .data$USD, bcv_bs_eur = .data$EUR,
      usd_per_eur = .data$EUR / .data$USD)
  # Retain every source quote in the audit, including conflicting observations.
  observations <- interventions %>%
    dplyr::left_join(cross, by = "date", relationship = "many-to-one") %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(conflicting_date = dplyr::n_distinct(.data$rate) > 1L) %>% dplyr::ungroup() %>%
    dplyr::mutate(rate_int = .data$rate / .data$usd_per_eur,
      # Match the existing consolidated FX series' bolivar reconversion convention.
      rate_int_adj = dplyr::if_else(.data$date <= as.Date("2021-10-03"), .data$rate_int / 1000000, .data$rate_int),
      conversion_status = dplyr::case_when(
        .data$conflicting_date ~ "source_conflict",
        is.na(.data$usd_per_eur) ~ "missing_cross_rate",
        TRUE ~ "converted"))
  # The main daily series contains observed, unambiguous conversions only; no filling.
  daily <- observations %>% dplyr::filter(.data$conversion_status == "converted") %>%
    dplyr::select("date", "rate_int", "rate_int_adj") %>% dplyr::distinct() %>% dplyr::arrange(.data$date)
  list(observations = observations, daily = daily)
}
