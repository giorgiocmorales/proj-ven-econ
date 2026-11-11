# Construct sign episodes while preserving annual observations and source provenance.
# Zero is a separate state; missing observations and calendar gaps break continuity.
build_sign_episode_paths <- function(observations) {
  required_columns <- c("source", "country_code", "indicator_id", "year", "value")
  if (!all(required_columns %in% names(observations))) {
    rlang::abort("Sign episode input is missing required columns.", class = "macro_api_schema_error")
  }
  if (anyDuplicated(observations[required_columns[1:4]])) {
    rlang::abort("Sign episode input contains duplicate annual observations.", class = "macro_duplicate_error")
  }
  if (anyNA(observations[required_columns[1:4]]) ||
      any(!is.finite(observations$year)) || any(observations$year != trunc(observations$year)) ||
      any(!is.finite(observations$value) & !is.na(observations$value))) {
    rlang::abort("Sign episode input contains invalid keys, years, or values.", class = "macro_value_error")
  }

  # Segment each series independently before assigning stable source-country-year keys.
  observations %>%
    arrange(source, country_code, indicator_id, year) %>%
    group_by(source, country_code, indicator_id) %>%
    mutate(episode_sign = case_when(is.na(value) ~ "missing", value > 0 ~ "positive",
                                    value < 0 ~ "negative", TRUE ~ "zero"),
           boundary_reason = case_when(
             row_number() == 1L ~ "series_start",
             year != lag(year) + 1L ~ "calendar_gap",
             lag(episode_sign) == "missing" ~ "after_missing",
             episode_sign == "missing" ~ "missing_observation",
             episode_sign != lag(episode_sign) ~ "sign_change",
             TRUE ~ NA_character_),
           episode_number = cumsum(!is.na(boundary_reason)),
           episode_id = if_else(episode_sign == "missing", NA_character_,
                                paste(source, country_code, indicator_id, episode_number, sep = ":"))) %>%
    ungroup() %>%
    group_by(episode_id) %>%
    mutate(episode_year_number = if_else(is.na(episode_id), NA_integer_, row_number())) %>%
    ungroup()
}
