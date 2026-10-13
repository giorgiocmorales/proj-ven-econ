# Helpers for BCV financial-sector workbook extraction and unit harmonization.

#' Convert BCV labels to a stable ASCII comparison key.
#'
#' @param x Character vector to normalize.
#' @return Lowercase ASCII character vector with normalized whitespace.
financial_ascii <- function(x) {
  x %>%
    as.character() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", " ") %>%
    stringr::str_squish()
}

#' Convert a source label to a stable identifier fragment.
#'
#' @param x Character vector to convert.
#' @return Snake-case identifier vector.
financial_slug <- function(x) {
  financial_ascii(x) %>%
    stringr::str_replace_all(" ", "_") %>%
    dplyr::na_if("")
}

#' Map BCV worksheet names to stable institution categories.
#'
#' @param sheet_name Worksheet name supplied by the BCV workbook.
#' @return Stable institution identifier.
financial_institution_id <- function(sheet_name) {
  key <- financial_ascii(sheet_name)

  dplyr::case_when(
    stringr::str_detect(key, "universal") ~ "universal_banks",
    stringr::str_detect(key, "comercial") ~ "commercial_banks",
    stringr::str_detect(key, "microfinanc") ~ "microfinance_banks",
    stringr::str_detect(key, "hipotec") ~ "mortgage_banks",
    stringr::str_detect(key, "inversion") ~ "investment_banks",
    stringr::str_detect(key, "ahorro.*prest") ~ "savings_and_loan_entities",
    stringr::str_detect(key, "arrend") ~ "financial_leasing_entities",
    stringr::str_detect(key, "mercado.*monet") ~ "money_market_funds",
    stringr::str_detect(key, "desarrollo") ~ "development_banks",
    TRUE ~ NA_character_
  )
}

#' Return the explicit monetary-unit conversion table.
#'
#' @return Mapping from source units to comparable current units.
financial_unit_map <- function() {
  tibble::tribble(
    ~unit_profile, ~currency_group, ~currency_raw, ~unit_raw, ~conversion_factor, ~currency, ~unit,
    "veb_million", "national", "VEB", "million_VEB", 1e-8, "VES", "current_VES",
    "vef_thousand", "national", "VEF", "thousand_VEF", 1e-8, "VES", "current_VES",
    "ves2018_thousand", "national", "VES_2018", "thousand_VES_2018", 1e-3, "VES", "current_VES",
    "ves_current", "national", "VES", "VES", 1, "VES", "current_VES",
    "veb_million_usd_thousand", "national", "VEB", "million_VEB", 1e-8, "VES", "current_VES",
    "veb_million_usd_thousand", "foreign", "USD", "thousand_USD", 1e3, "USD", "USD",
    "vef_thousand_usd_thousand", "national", "VEF", "thousand_VEF", 1e-8, "VES", "current_VES",
    "vef_thousand_usd_thousand", "foreign", "USD", "thousand_USD", 1e3, "USD", "USD",
    "percent", "not_applicable", "not_applicable", "percent", 1, "not_applicable", "percent"
  )
}

#' Parse BCV month rows with year headings embedded in the same column.
#'
#' @param x Character vector containing year headings and month labels.
#' @return Date vector with one observation for each recognized month row.
financial_parse_month_rows <- function(x) {
  key <- financial_ascii(x)
  month_map <- c(
    enero = 1L,
    febrero = 2L,
    marzo = 3L,
    abril = 4L,
    mayo = 5L,
    junio = 6L,
    julio = 7L,
    agosto = 8L,
    septiembre = 9L,
    setiembre = 9L,
    octubre = 10L,
    noviembre = 11L,
    diciembre = 12L
  )
  year_heading <- dplyr::if_else(
    stringr::str_detect(key, "^\\d{4}$"),
    suppressWarnings(as.integer(key)),
    NA_integer_
  )
  year <- tidyr::fill(tibble::tibble(year = year_heading), "year", .direction = "down")$year
  month <- unname(month_map[key])
  date <- rep(as.Date(NA), length(x))
  valid <- !is.na(year) & !is.na(month)
  date[valid] <- as.Date(sprintf("%04d-%02d-01", year[valid], month[valid]))
  date
}

#' Map extracted financial labels to stable variable identifiers.
#'
#' @param dataset_family Financial dataset family.
#' @param label_raw Source label reconstructed from workbook headers.
#' @return Stable variable identifier with an explicit fallback.
financial_variable_id <- function(dataset_family, label_raw) {
  key <- financial_ascii(label_raw)
  fallback <- paste0("source_", dplyr::coalesce(financial_slug(label_raw), "unmapped"))

  dplyr::case_when(
    dataset_family == "balances" & stringr::str_detect(key, "cuentas activas") ~ "total_assets",
    dataset_family == "balances" & stringr::str_detect(key, "disponibil") ~ "cash_and_equivalents",
    dataset_family == "balances" & stringr::str_detect(key, "cartera.*credito") ~ "loan_portfolio",
    dataset_family == "balances" & stringr::str_detect(key, "inversion") ~ "investments",
    dataset_family == "balances" & stringr::str_detect(key, "cuentas pasivas") ~ "total_liabilities",
    dataset_family == "balances" & stringr::str_detect(key, "depositos a la vista") ~ "demand_deposits",
    dataset_family == "balances" & stringr::str_detect(key, "depositos de ahorro$") ~ "savings_deposits",
    dataset_family == "balances" & stringr::str_detect(key, "depositos a plazo$") ~ "time_deposits",
    dataset_family == "balances" & stringr::str_detect(key, "depositos y certificados de ahorro|captaciones y certificados") ~ "deposits_and_savings_certificates",
    dataset_family == "balances" & stringr::str_detect(key, "depositos a plazo y certificados") ~ "time_deposits_and_savings_certificates",
    dataset_family == "balances" & stringr::str_detect(key, "reservas bancarias excedentes") ~ "excess_bank_reserves",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "ordinario|ordinary") ~ "required_reserves_ordinary",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "marginal") ~ "required_reserves_marginal",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "especial|special") ~ "required_reserves_special",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "^total$") ~ "required_reserves_total",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "requerimiento.*encaje|^encaje$|required reserves") ~ "required_reserves",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "cuenta corriente|cta cte|current account") ~ "current_account_balance",
    dataset_family == "reserve_positions" & stringr::str_detect(key, "reservas bancarias excedentes|disponibilidad|excess reserves") ~ "excess_reserves",
    dataset_family == "reserve_coefficients" & stringr::str_detect(key, "ordinario|ordinary") ~ "reserve_ratio_ordinary",
    dataset_family == "reserve_coefficients" & stringr::str_detect(key, "marginal") ~ "reserve_ratio_marginal",
    dataset_family == "reserve_coefficients" & stringr::str_detect(key, "especial|special") ~ "reserve_ratio_special",
    TRUE ~ fallback
  )
}

#' Attach source and unit metadata to extracted observations.
#'
#' @param observations Extracted source observations.
#' @param source_row One-row source registry tibble.
#' @param dataset_id Stable dataset identifier.
#' @return Observations with raw and normalized monetary values.
financial_attach_metadata <- function(observations, source_row, dataset_id) {
  unit_map <- financial_unit_map()

  output <- observations %>%
    dplyr::mutate(
      unit_profile = source_row$unit_profile[[1]],
      dataset_family = source_row$dataset_family[[1]],
      dataset_id = dataset_id,
      source_id = "bcv",
      source_url = source_row$source_url[[1]],
      source_file = source_row$raw_file[[1]],
      source_vintage = source_row$vintage_label[[1]],
      source_priority = source_row$priority[[1]],
      downloaded_at = format(file.info(source_row$raw_file[[1]])$mtime, "%Y-%m-%d %H:%M:%S")
    ) %>%
    dplyr::left_join(unit_map, by = c("unit_profile", "currency_group"))

  if (any(is.na(output$unit_raw))) {
    missing_profiles <- output %>%
      dplyr::filter(is.na(.data$unit_raw)) %>%
      dplyr::distinct(.data$unit_profile, .data$currency_group)
    stop(
      sprintf(
        "Missing financial unit mapping: %s",
        paste(paste(missing_profiles$unit_profile, missing_profiles$currency_group, sep = "/"), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  output %>%
    dplyr::mutate(
      value = .data$value_raw * .data$conversion_factor,
      variable_id = financial_variable_id(.data$dataset_family, .data$series_label_raw)
    )
}
