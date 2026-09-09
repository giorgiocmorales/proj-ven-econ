# Validate report-critical data extracts against explicit file contracts.

split_contract_fields <- function(value) {
  if (is.na(value) || !nzchar(value)) {
    return(character())
  }

  stringr::str_split(value, "\\|", simplify = FALSE)[[1]] %>%
    stringr::str_trim()
}

split_required_columns <- function(value) {
  if (is.na(value) || !nzchar(value)) {
    return(character())
  }

  stringr::str_split(value, ",", simplify = FALSE)[[1]] %>%
    stringr::str_trim()
}

new_validation_result <- function(dataset_id, check, status, details) {
  tibble::tibble(
    dataset_id = dataset_id,
    check = check,
    status = status,
    details = details
  )
}

read_data_contracts <- function(path) {
  contracts <- readr::read_csv(
    file = path,
    show_col_types = FALSE,
    col_types = readr::cols(
      dataset_id = readr::col_character(),
      file_path = readr::col_character(),
      required_columns = readr::col_character(),
      key_columns = readr::col_character(),
      date_column = readr::col_character(),
      min_rows = readr::col_integer(),
      max_age_days = readr::col_integer(),
      non_negative_columns = readr::col_character(),
      latest_non_missing_columns = readr::col_character()
    )
  )

  required_fields <- c(
    "dataset_id", "file_path", "required_columns", "key_columns",
    "date_column", "min_rows", "max_age_days", "non_negative_columns",
    "latest_non_missing_columns"
  )
  missing_fields <- setdiff(required_fields, names(contracts))

  if (length(missing_fields) > 0) {
    stop(
      sprintf(
        "Data contract registry is missing columns: %s",
        paste(missing_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(contracts$dataset_id) > 0) {
    stop("Data contract registry contains duplicate dataset IDs", call. = FALSE)
  }

  contracts
}

validate_dataset_contract <- function(contract, reference_date) {
  dataset_id <- contract$dataset_id[[1]]
  file_path <- contract$file_path[[1]]
  required_columns <- split_required_columns(contract$required_columns[[1]])
  key_columns <- split_contract_fields(contract$key_columns[[1]])
  date_column <- contract$date_column[[1]]
  non_negative_columns <- split_contract_fields(contract$non_negative_columns[[1]])
  latest_non_missing_columns <- split_contract_fields(contract$latest_non_missing_columns[[1]])
  results <- list()

  if (!file.exists(file_path)) {
    return(new_validation_result(
      dataset_id = dataset_id,
      check = "file_exists",
      status = "fail",
      details = sprintf("Required file is missing: %s", file_path)
    ))
  }

  data <- readr::read_csv(
    file = file_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  contract_columns <- unique(c(
    required_columns,
    key_columns,
    date_column,
    non_negative_columns,
    latest_non_missing_columns
  ))
  missing_columns <- setdiff(contract_columns, names(data))
  results[[length(results) + 1]] <- new_validation_result(
    dataset_id = dataset_id,
    check = "required_columns",
    status = if_else(length(missing_columns) == 0, "pass", "fail"),
    details = if_else(
      length(missing_columns) == 0,
      "All required columns are present",
      sprintf("Missing columns: %s", paste(missing_columns, collapse = ", "))
    )
  )

  if (length(missing_columns) > 0) {
    return(dplyr::bind_rows(results))
  }

  results[[length(results) + 1]] <- new_validation_result(
    dataset_id = dataset_id,
    check = "minimum_rows",
    status = if_else(nrow(data) >= contract$min_rows[[1]], "pass", "fail"),
    details = sprintf("Observed %s rows; required at least %s", nrow(data), contract$min_rows[[1]])
  )

  required_missing_values <- purrr::map_int(required_columns, function(column_name) {
    values <- data[[column_name]]
    sum(is.na(values) | stringr::str_trim(values) == "")
  })
  names(required_missing_values) <- required_columns
  incomplete_columns <- names(required_missing_values)[required_missing_values > 0]

  results[[length(results) + 1]] <- new_validation_result(
    dataset_id = dataset_id,
    check = "required_values",
    status = if_else(length(incomplete_columns) == 0, "pass", "fail"),
    details = if_else(
      length(incomplete_columns) == 0,
      "Required columns contain no missing values",
      sprintf("Columns with missing values: %s", paste(incomplete_columns, collapse = ", "))
    )
  )

  duplicate_keys <- data %>%
    dplyr::count(dplyr::across(dplyr::all_of(key_columns))) %>%
    dplyr::filter(.data$n > 1)

  results[[length(results) + 1]] <- new_validation_result(
    dataset_id = dataset_id,
    check = "unique_keys",
    status = if_else(nrow(duplicate_keys) == 0, "pass", "fail"),
    details = sprintf("Found %s duplicate key combinations", nrow(duplicate_keys))
  )

  parsed_dates <- suppressWarnings(as.Date(data[[date_column]]))
  invalid_dates <- sum(is.na(parsed_dates))

  results[[length(results) + 1]] <- new_validation_result(
    dataset_id = dataset_id,
    check = "valid_dates",
    status = if_else(invalid_dates == 0, "pass", "fail"),
    details = sprintf("Found %s invalid dates in %s", invalid_dates, date_column)
  )

  if (invalid_dates == 0) {
    latest_date <- max(parsed_dates)
    age_days <- as.integer(reference_date - latest_date)
    results[[length(results) + 1]] <- new_validation_result(
      dataset_id = dataset_id,
      check = "freshness",
      status = if_else(age_days <= contract$max_age_days[[1]], "pass", "fail"),
      details = sprintf(
        "Latest observation is %s (%s days old; limit %s)",
        latest_date,
        age_days,
        contract$max_age_days[[1]]
      )
    )

    latest_rows <- data[parsed_dates == latest_date, , drop = FALSE]
    latest_missing_columns <- purrr::keep(
      latest_non_missing_columns,
      function(column_name) {
        all(is.na(latest_rows[[column_name]]) | stringr::str_trim(latest_rows[[column_name]]) == "")
      }
    )
    results[[length(results) + 1]] <- new_validation_result(
      dataset_id = dataset_id,
      check = "latest_values",
      status = if_else(length(latest_missing_columns) == 0, "pass", "fail"),
      details = if_else(
        length(latest_missing_columns) == 0,
        sprintf("Latest observation contains required values for %s", paste(latest_non_missing_columns, collapse = ", ")),
        sprintf("Latest observation is missing values for %s", paste(latest_missing_columns, collapse = ", "))
      )
    )
  }

  for (column_name in non_negative_columns) {
    numeric_values <- readr::parse_number(data[[column_name]], na = c("", "NA"))
    invalid_values <- sum(is.na(numeric_values) | numeric_values < 0)
    results[[length(results) + 1]] <- new_validation_result(
      dataset_id = dataset_id,
      check = paste0("non_negative_", column_name),
      status = if_else(invalid_values == 0, "pass", "fail"),
      details = sprintf("Found %s missing or negative values in %s", invalid_values, column_name)
    )
  }

  dplyr::bind_rows(results)
}

validate_data_contracts <- function(contracts, reference_date) {
  purrr::map_dfr(
    seq_len(nrow(contracts)),
    function(row_index) {
      validate_dataset_contract(
        contract = contracts[row_index, ],
        reference_date = reference_date
      )
    }
  )
}
