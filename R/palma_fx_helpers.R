# Historical FX selection helpers. Canonical BCV/Yadio quotes always outrank Palma.

#' Expand usable historical quotes within each source's own observed coverage.
#' @param observations Parsed Palma cells with explicit priorities and carry limits.
#' @return Daily candidates with source dates; monthly anchors remain excluded.
palma_fx_expand_history <- function(observations) {
  usable <- observations %>% dplyr::filter(.data$quality_status == "usable",
    .data$frequency %in% c("daily", "auction"), .data$date >= as.Date("1990-01-01"))
  keys <- c("workbook_id", "sheet", "variable_id", "quote_currency")
  conflicts <- usable %>% dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "date")))) %>%
    dplyr::summarise(values = dplyr::n_distinct(.data$rate_adjusted), .groups = "drop") %>%
    dplyr::filter(.data$values > 1L)
  if (nrow(conflicts) > 0L) {
    rlang::abort("Unquarantined same-source FX conflicts", class = "palma_fx_duplicate_error")
  }
  # Collapse only identical repeated quotes; retain the original source-key row count.
  usable <- usable %>% dplyr::arrange(.data$source_row) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, "date"))), .keep_all = TRUE) %>%
    dplyr::rename(observation_date = "date")
  calendar <- usable %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::group_modify(function(.x, .y) {
      tibble::tibble(date = seq.Date(min(.x$observation_date), max(.x$observation_date), by = "day"))
    }) %>% dplyr::ungroup()
  # Rolling joins carry row identities, so rates and metadata stay attached to their source.
  calendar %>% dplyr::left_join(usable,
    by = dplyr::join_by(workbook_id, sheet, variable_id, quote_currency, closest(date >= observation_date)),
    relationship = "many-to-one") %>%
    dplyr::filter(as.integer(.data$date - .data$observation_date) <= .data$carry_days) %>%
    dplyr::anti_join(observations %>% dplyr::filter(.data$quality_status != "usable", !is.na(.data$date)) %>%
      dplyr::select(dplyr::all_of(c(keys, "date"))) %>% dplyr::distinct(), by = c(keys, "date")) %>%
    dplyr::mutate(canonical = FALSE, source_series = .data$variable_id,
      value_method = dplyr::if_else(.data$date == .data$observation_date, "source_quote", "carried_forward"),
      carried_days = as.integer(.data$date - .data$observation_date))
}

#' Read existing canonical calendar values without changing any value or convention.
#' @param canonical Existing external_fx.csv data frame.
#' @param canonical_text The same file read as text to retain exact decimal tokens.
#' @param source_file Immutable copy of the canonical calendar file.
#' @param source_sha256 SHA-256 of that immutable copy.
#' @return Canonical series plus an explicit SMC-then-index official headline view.
palma_fx_canonical <- function(canonical, canonical_text, source_file, source_sha256) {
  mapping <- tibble::tribble(
    ~source_series, ~raw_column, ~adjusted_column, ~variable_id, ~source_id, ~market, ~priority,
    "rate_smc", "rate_smc", "rate_smc_adj", "official_bcv_smc", "bcv", "official", 0,
    "rate_indx", "rate_indx", "rate_indx_adj", "official_bcv_index", "bcv", "official", 0,
    "rate_int", "rate_int", "rate_int_adj", "official_bcv_intervention", "bcv", "official", 0,
    "rate_yad", "rate_yad", "rate_yad", "parallel", "yadio", "parallel", 0)
  required <- unique(c("date", mapping$raw_column, mapping$adjusted_column))
  if (!all(required %in% names(canonical)) || anyNA(canonical$date) || anyDuplicated(canonical$date) ||
      !identical(as.character(canonical$date), canonical_text$date)) {
    rlang::abort("Invalid canonical FX schema or date key", class = "palma_fx_canonical_error")
  }
  result <- purrr::pmap_dfr(mapping, function(source_series, raw_column, adjusted_column,
                                            variable_id, source_id, market, priority) {
    canonical %>% dplyr::transmute(date = .data$date, rate_raw = .data[[raw_column]],
      rate_adjusted = .data[[adjusted_column]], source_series = .env$source_series,
      variable_id = .env$variable_id, source_id = .env$source_id, market = .env$market,
      priority = .env$priority, value_cell_raw = canonical_text[[raw_column]],
      adjusted_cell_raw = canonical_text[[adjusted_column]]) %>% dplyr::filter(!is.na(.data$rate_adjusted))
  })
  if (anyNA(result$rate_raw) || any(!is.finite(result$rate_adjusted) | result$rate_adjusted <= 0) ||
      any(!is.finite(result$rate_raw) | result$rate_raw <= 0)) {
    rlang::abort("Invalid canonical FX values", class = "palma_fx_canonical_error")
  }
  # The canonical file owns its reconversion conventions, including its 2021 boundary.
  # Infer the source denomination from its unchanged raw/adjusted pair, not a new rule.
  result <- result %>% dplyr::mutate(source_conversion_factor = .data$rate_adjusted / .data$rate_raw,
    source_currency = dplyr::case_when(abs(.data$source_conversion_factor - 1) < 1e-12 ~ "VES_current",
      abs(.data$source_conversion_factor - 1e-6) < 1e-18 ~ "VES_2018", TRUE ~ NA_character_))
  if (anyNA(result$source_currency)) {
    rlang::abort("Unknown canonical denomination; preserve the input for review", class = "palma_fx_canonical_error")
  }
  headline <- result %>% dplyr::filter(.data$variable_id %in% c("official_bcv_smc", "official_bcv_index")) %>%
    dplyr::mutate(priority = dplyr::if_else(.data$variable_id == "official_bcv_smc", 0, 1),
      variable_id = "official_headline")
  dplyr::bind_rows(result, headline) %>% dplyr::mutate(canonical = TRUE, quote_currency = "USD",
    observation_date = .data$date, observation_id = paste("canonical", .data$source_series, .data$date, sep = "|"),
    source_file = .env$source_file, source_sha256 = .env$source_sha256,
    source_path = "data/final/external_fx.csv", workbook_id = "canonical", sheet = NA_character_,
    source_cell = NA_character_, source_row = NA_integer_, source_key_rows = 1L,
    source_key_values = 1L, quality_status = "usable", frequency = "daily",
    value_method = "canonical_calendar_value", carried_days = NA_integer_)
}

#' Select one rate per date/series while enforcing canonical precedence.
#' @param candidates Comparable daily candidates with explicit priorities.
#' @return Selected candidates, retaining every canonical series value unchanged.
palma_fx_select_daily <- function(candidates) {
  keys <- c("date", "variable_id", "quote_currency")
  if (anyNA(candidates[c(keys, "priority", "canonical", "rate_adjusted")]) ||
      any(!is.finite(candidates$rate_adjusted) | candidates$rate_adjusted <= 0)) {
    rlang::abort("Invalid FX selection candidates", class = "palma_fx_selection_error")
  }
  # Canonical status is sorted first, independently of historical priority settings.
  top <- candidates %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$canonical == any(.data$canonical)) %>%
    dplyr::filter(.data$priority == min(.data$priority)) %>% dplyr::ungroup()
  ties <- top %>% dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(values = dplyr::n_distinct(.data$rate_adjusted), .groups = "drop") %>%
    dplyr::filter(.data$values > 1L)
  if (nrow(ties) > 0L) {
    rlang::abort("Conflicting equal-priority FX candidates", class = "palma_fx_priority_error")
  }
  top %>% dplyr::arrange(.data$observation_id) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE) %>%
    dplyr::arrange(.data$date, .data$variable_id, .data$quote_currency)
}
