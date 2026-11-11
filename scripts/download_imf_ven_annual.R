# Download all published annual IMF series involving Venezuela, including vintages.
# Run from the project root; metadata and raw responses are retained for audit.
library(tidyverse)
library(httr2)
library(xml2)
library(jsonlite)

source("R/source_helpers.R")
ensure_project_dirs()
export_dir <- "outputs/imf_ven_annual"
raw_dir <- "data/raw/imf_ven_annual"
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
api_base <- "https://api.imf.org/external/sdmx/2.1"
reuse_catalog <- Sys.getenv("IMF_REUSE_CATALOG") == "TRUE"
previous_flows <- if (reuse_catalog) read_csv(file.path(export_dir, "imf_databases.csv"), show_col_types = FALSE) else tibble()

# Keep all HTTP statuses visible; transport failures still raise their own errors.
make_request <- function(url) {
  request(url) %>%
    req_timeout(180) %>%
    req_retry(max_tries = 3) %>%
    req_error(is_error = function(response) FALSE)
}

# English labels and exact SDMX references avoid heuristic codelist matching.
english_name <- function(node) {
  names <- xml_find_all(node, "./*[local-name()='Name']")
  preferred <- names[which(xml_attr(names, "lang") == "en")]
  if (length(preferred) > 0) return(xml_text(preferred[[1]]))
  if (length(names) > 0) return(xml_text(names[[1]]))
  NA_character_
}

reference_node <- function(nodes, ref) {
  nodes[which(xml_attr(nodes, "id") == xml_attr(ref, "id") &
    xml_attr(nodes, "agencyID") == xml_attr(ref, "agencyID") &
    xml_attr(nodes, "version") == xml_attr(ref, "version"))]
}

# Resolve enumerations from the dimension or its referenced concept definition.
dimension_codes <- function(node, codelist_nodes, scheme_nodes) {
  ref <- xml_find_first(node, "./*[local-name()='LocalRepresentation']//*[local-name()='Enumeration']/*[local-name()='Ref']")
  if (inherits(ref, "xml_missing")) {
    concept_ref <- xml_find_first(node, "./*[local-name()='ConceptIdentity']/*[local-name()='Ref']")
    schemes <- scheme_nodes
    schemes <- schemes[which(xml_attr(schemes, "id") == xml_attr(concept_ref, "maintainableParentID") &
      xml_attr(schemes, "agencyID") == xml_attr(concept_ref, "agencyID") &
      xml_attr(schemes, "version") == xml_attr(concept_ref, "maintainableParentVersion"))]
    concepts <- xml_find_all(schemes, "./*[local-name()='Concept']")
    concepts <- concepts[which(xml_attr(concepts, "id") == xml_attr(concept_ref, "id"))]
    ref <- xml_find_first(concepts, "./*[local-name()='CoreRepresentation']//*[local-name()='Enumeration']/*[local-name()='Ref']")
  }
  if (length(ref) == 0 || inherits(ref, "xml_missing")) return(tibble(code = character(), code_name = character()))
  codelists <- reference_node(codelist_nodes, ref)
  if (length(codelists) != 1) rlang::abort("Referenced codelist could not be uniquely resolved.", class = "imf_structure_error")
  codes <- xml_find_all(codelists, "./*[local-name()='Code']")
  code_names <- xml_find_first(codes, "./*[local-name()='Name'][@xml:lang='en'][1]") %>% xml_text()
  missing_names <- which(is.na(code_names))
  code_names[missing_names] <- map_chr(codes[missing_names], english_name)
  tibble(code = xml_attr(codes, "id"), code_name = code_names)
}

# Fetch the live catalog before inspecting every database's exact structure.
catalog_url <- paste0(api_base, "/dataflow/all/all/latest")
catalog_response <- make_request(catalog_url) %>% req_perform()
if (resp_status(catalog_response) != 200) rlang::abort("IMF dataflow catalog failed.", class = "imf_http_error")
writeBin(resp_body_raw(catalog_response), file.path(raw_dir, "dataflows.xml"))
catalog_doc <- read_xml(resp_body_raw(catalog_response))
flows <- xml_find_all(catalog_doc, ".//*[local-name()='Dataflow']") %>%
  map_dfr(function(node) {
    tibble(flow_id = xml_attr(node, "id"), agency_id = xml_attr(node, "agencyID"),
      version = xml_attr(node, "version"), flow_name = english_name(node))
  }) %>% arrange(agency_id, flow_id, version) %>%
  mutate(is_vintage = str_detect(flow_id, "VINTAGE"),
    structure_url = sprintf("%s/dataflow/%s/%s/%s?references=all", api_base, agency_id, flow_id, version))
if (reuse_catalog && !identical(paste(flows$agency_id, flows$flow_id, flows$version),
  paste(previous_flows$agency_id, previous_flows$flow_id, previous_flows$version))) {
  rlang::abort("The live catalog changed; rerun without IMF_REUSE_CATALOG to refresh metadata.", class = "imf_stale_catalog_error")
}
write_csv(flows, file.path(export_dir, "imf_databases.csv"))

# Checkpoint each structure so interrupted downloads can resume without new dependencies.
if (!reuse_catalog) {
metadata_errors <- tibble(flow_id = character(), error_message = character())
for (batch_start in seq(1, nrow(flows), by = 8)) {
  indices <- batch_start:min(batch_start + 7, nrow(flows))
  pending <- indices[!file.exists(file.path(raw_dir, paste0(flows$flow_id[indices], "_structure.xml")))]
  if (length(pending) > 0) {
    responses <- map(flows$structure_url[pending], make_request) %>%
      req_perform_parallel(on_error = "continue", max_active = 4, progress = FALSE)
    for (j in seq_along(pending)) {
      i <- pending[[j]]
      response <- responses[[j]]
      if (inherits(response, "httr2_response") && resp_status(response) == 200) {
        writeBin(resp_body_raw(response), file.path(raw_dir, paste0(flows$flow_id[[i]], "_structure.xml")))
      } else {
        detail <- if (inherits(response, "httr2_response")) paste("HTTP", resp_status(response), substr(resp_body_string(response), 1, 500)) else conditionMessage(response)
        metadata_errors <- bind_rows(metadata_errors, tibble(flow_id = flows$flow_id[[i]], error_message = detail))
        message("STRUCTURE ERROR: ", flows$flow_id[[i]], " ", detail)
      }
    }
  }
  message("Structures checked: ", max(indices), "/", nrow(flows))
}
write_csv(metadata_errors, file.path(export_dir, "imf_metadata_errors.csv"))
if (nrow(metadata_errors) > 0) rlang::abort("Some IMF structures failed; see imf_metadata_errors.csv and rerun to retry.", class = "imf_metadata_error")

# Inspect all indicator and geographic enumerations before constructing data queries.
dimensions <- list()
codes <- list()
queries <- list()
coverage <- list()
for (i in seq_len(nrow(flows))) {
  flow <- flows[i, ]
  message("Inspecting indicator definitions: ", flow$flow_id)
  doc <- read_xml(file.path(raw_dir, paste0(flow$flow_id, "_structure.xml")))
  flow_node <- xml_find_all(doc, ".//*[local-name()='Dataflow']")
  flow_node <- flow_node[which(xml_attr(flow_node, "id") == flow$flow_id)]
  dsd_ref <- xml_find_first(flow_node, "./*[local-name()='Structure']/*[local-name()='Ref']")
  dsd <- reference_node(xml_find_all(doc, ".//*[local-name()='DataStructure']"), dsd_ref)
  if (length(dsd) != 1) rlang::abort(paste("Unresolved structure:", flow$flow_id), class = "imf_structure_error")
  nodes <- xml_find_all(dsd, ".//*[local-name()='DimensionList']/*[local-name()='Dimension']")
  nodes <- nodes[order(as.integer(xml_attr(nodes, "position")))]
  ids <- xml_attr(nodes, "id")
  node_codes <- map(nodes, dimension_codes,
    codelist_nodes = xml_find_all(doc, ".//*[local-name()='Codelist']"),
    scheme_nodes = xml_find_all(doc, ".//*[local-name()='ConceptScheme']"))
  names(node_codes) <- ids
  dimensions[[i]] <- tibble(flow_id = flow$flow_id, dimension_id = ids, key_position = seq_along(ids))
  codes[[i]] <- imap_dfr(node_codes, function(tbl, id) mutate(tbl, flow_id = flow$flow_id, dimension_id = id, .before = 1))
  frequency_ids <- ids[str_detect(ids, "^(FREQ|FREQUENCY)$")]
  geo_ids <- ids[map_lgl(node_codes, function(tbl) any(str_detect(coalesce(tbl$code_name, ""), regex("Venezuela", ignore_case = TRUE))))] %>%
    setdiff(c("UNIT", "UNIT_MEASURE", "CURRENCY"))
  annual_available <- length(frequency_ids) == 0 || all(map_lgl(node_codes[frequency_ids], function(tbl) "A" %in% tbl$code))
  status <- if (!annual_available) "no_annual_frequency" else if (length(geo_ids) == 0) "no_venezuela_geography" else "pending"
  coverage[[i]] <- tibble(flow_id = flow$flow_id, status = status, query_count = 0L, observations = 0L)
  if (status != "pending") next
  for (geo_id in geo_ids) {
    geo_codes <- node_codes[[geo_id]] %>% filter(str_detect(coalesce(code_name, ""), regex("Venezuela", ignore_case = TRUE))) %>% pull(code)
    # Prefer the exact ISO country code over institutions or named regional groups.
    if ("VEN" %in% geo_codes) geo_codes <- "VEN"
    key <- rep("", length(ids))
    key[match(geo_id, ids)] <- paste(geo_codes, collapse = "+")
    key[match(frequency_ids, ids)] <- "A"
    query_key <- paste(key, collapse = ".")
    query_id <- paste(flow$flow_id, geo_id, sep = "__")
    queries[[length(queries) + 1]] <- tibble(flow_id = flow$flow_id, query_id = query_id,
      venezuela_dimension = geo_id, venezuela_codes = paste(geo_codes, collapse = "+"), key = query_key,
      source_url = sprintf("%s/data/%s,%s,%s/%s", api_base, flow$agency_id, flow$flow_id, flow$version, query_key))
  }
}
dimension_catalog <- bind_rows(dimensions)
code_catalog <- bind_rows(codes)
query_catalog <- bind_rows(queries)
coverage <- bind_rows(coverage)
} else {
  # Reuse only catalogs whose database identities still match the live API catalog.
  dimension_catalog <- read_csv(file.path(export_dir, "imf_dimensions.csv"), show_col_types = FALSE)
  code_catalog <- read_csv(file.path(export_dir, "imf_indicator_and_dimension_codes.csv"),
    show_col_types = FALSE, col_types = cols(.default = col_character()))
  query_catalog <- read_csv(file.path(export_dir, "imf_queries.csv"), show_col_types = FALSE) %>%
    filter(!venezuela_dimension %in% c("UNIT", "UNIT_MEASURE", "CURRENCY"))
  annual_flows <- code_catalog %>% filter(dimension_id %in% c("FREQ", "FREQUENCY"), code == "A") %>% pull(flow_id)
  coverage <- tibble(flow_id = flows$flow_id,
    status = case_when(!flow_id %in% annual_flows ~ "no_annual_frequency",
      !flow_id %in% query_catalog$flow_id ~ "no_venezuela_geography", TRUE ~ "pending"),
    query_count = 0L, observations = 0L)
}
write_csv(dimension_catalog, file.path(export_dir, "imf_dimensions.csv"))
write_csv(code_catalog, file.path(export_dir, "imf_indicator_and_dimension_codes.csv"))
write_csv(query_catalog, file.path(export_dir, "imf_queries.csv"))
message("Catalog complete: ", nrow(flows), " databases; ", nrow(query_catalog), " annual Venezuela queries.")

# Download country and partner queries with bounded concurrency and explicit status logs.
request_log <- list()
previous_log <- if (file.exists(file.path(export_dir, "imf_download_log.csv"))) {
  read_csv(file.path(export_dir, "imf_download_log.csv"), show_col_types = FALSE, col_types = cols(.default = col_character(), http_status = col_integer()))
} else tibble(query_id = character(), source_url = character(), http_status = integer())
for (batch_start in seq(1, nrow(query_catalog), by = 8)) {
  indices <- batch_start:min(batch_start + 7, nrow(query_catalog))
  # Only a matching successful request and its saved response qualify for reuse.
  reusable <- map_lgl(indices, function(i) {
    any(previous_log$query_id == query_catalog$query_id[[i]] &
      previous_log$source_url == query_catalog$source_url[[i]] &
      previous_log$http_status %in% c(200, 204, 404)) &&
      file.exists(file.path(raw_dir, paste0(query_catalog$query_id[[i]], "_data.xml")))
  })
  for (i in indices[reusable]) {
    request_log[[i]] <- previous_log %>% filter(query_id == query_catalog$query_id[[i]], source_url == query_catalog$source_url[[i]]) %>% slice_tail(n = 1)
  }
  pending <- indices[!reusable]
  responses <- map(query_catalog$source_url[pending], make_request) %>%
    req_perform_parallel(on_error = "continue", max_active = 4, progress = FALSE)
  for (j in seq_along(pending)) {
    i <- pending[[j]]
    query <- query_catalog[i, ]
    response <- responses[[j]]
    status <- if (inherits(response, "httr2_response")) resp_status(response) else NA_integer_
    detail <- if (is.na(status)) conditionMessage(response) else if (status %in% c(200, 204, 404)) "" else substr(resp_body_string(response), 1, 500)
    if (status %in% c(200, 204, 404)) {
      writeBin(resp_body_raw(response), file.path(raw_dir, paste0(query$query_id, "_data.xml")))
    } else message("DATA ERROR: ", query$query_id, " ", detail)
    request_log[[i]] <- mutate(query, http_status = status, error_message = detail,
      downloaded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  write_csv(bind_rows(request_log), file.path(export_dir, "imf_download_log.csv"))
  message("Data queries checked: ", max(indices), "/", nrow(query_catalog))
}
request_log <- bind_rows(request_log)

# Serialize all dimensions and attributes losslessly while keeping common fields readable.
as_json <- function(values) as.character(toJSON(as.list(values), auto_unbox = TRUE, na = "null"))
observations <- list()
for (i in seq_len(nrow(request_log))) {
  query <- request_log[i, ]
  if (is.na(query$http_status) || query$http_status != 200) next
  doc <- read_xml(file.path(raw_dir, paste0(query$query_id, "_data.xml")))
  datasets <- xml_find_all(doc, ".//*[local-name()='DataSet']")
  if (length(datasets) == 0) rlang::abort(paste("No dataset in HTTP 200 response:", query$query_id), class = "imf_data_format_error")
  dim_ids <- dimension_catalog %>% filter(flow_id == query$flow_id) %>% pull(dimension_id)
  labels <- code_catalog %>% filter(flow_id == query$flow_id)
  label_lookup <- setNames(labels$code_name, paste(labels$dimension_id, labels$code, sep = "\037"))
  flow <- flows %>% filter(flow_id == query$flow_id)
  observations[[i]] <- map_dfr(datasets, function(dataset) {
    dataset_attrs <- xml_attrs(dataset)
    groups <- xml_find_all(dataset, "./*[local-name()='Group']") %>% map(xml_attrs)
    # Index group keys once so bilateral series need only matching metadata groups.
    group_dim_ids <- map(groups, function(group) intersect(dim_ids, names(group)))
    group_keys <- map_chr(seq_along(groups), function(index) {
      ids <- group_dim_ids[[index]]
      paste(ids, groups[[index]][ids], sep = "=", collapse = "\037")
    })
    group_sets <- group_dim_ids[!duplicated(map_chr(group_dim_ids, paste, collapse = "\037"))]
    map_dfr(xml_find_all(dataset, "./*[local-name()='Series']"), function(series) {
      attrs <- c(dataset_attrs[names(dataset_attrs) %in% dim_ids], xml_attrs(series))
      attrs <- attrs[!duplicated(names(attrs), fromLast = TRUE)]
      dims <- attrs[intersect(dim_ids, names(attrs))]
      if (length(dims) != length(dim_ids)) rlang::abort(paste("Incomplete series dimensions:", query$query_id), class = "imf_series_key_error")
      # Reject an API response that violates the requested country or frequency filter.
      requested_codes <- str_split(query$venezuela_codes, fixed("+"))[[1]]
      if (!query$venezuela_dimension %in% names(dims) || !dims[[query$venezuela_dimension]] %in% requested_codes) {
        rlang::abort(paste("Country filter mismatch:", query$query_id), class = "imf_country_filter_error")
      }
      freq_dims <- intersect(c("FREQ", "FREQUENCY"), names(dims))
      if (length(freq_dims) > 0 && any(dims[freq_dims] != "A")) {
        rlang::abort(paste("Frequency filter mismatch:", query$query_id), class = "imf_frequency_filter_error")
      }
      expected_keys <- map_chr(group_sets, function(ids) paste(ids, dims[ids], sep = "=", collapse = "\037"))
      matched_groups <- groups[which(group_keys %in% expected_keys & lengths(group_dim_ids) > 0)]
      group_attrs <- map(matched_groups, function(group) group[!names(group) %in% c(dim_ids, "type")]) %>% unlist(use.names = TRUE)
      attributes <- c(dataset_attrs, group_attrs, attrs[!names(attrs) %in% dim_ids])
      attributes <- attributes[!duplicated(names(attributes), fromLast = TRUE)]
      dim_labels <- unname(label_lookup[paste(names(dims), dims, sep = "\037")])
      dim_labels[is.na(dim_labels)] <- dims[is.na(dim_labels)]
      names(dim_labels) <- names(dims)
      obs <- xml_find_all(series, "./*[local-name()='Obs']")
      # Read observation columns in vectors; preserve any additional attributes as JSON.
      obs_attrs <- xml_attrs(obs)
      extra_attrs <- setdiff(unique(unlist(map(obs_attrs, names))), c("TIME_PERIOD", "OBS_VALUE"))
      obs_json <- if (length(extra_attrs) == 0) rep("{}", length(obs)) else {
        # Encode each distinct attribute combination once instead of once per year.
        extra_values <- map(obs_attrs, function(values) values[names(values) %in% extra_attrs])
        signatures <- map_chr(extra_values, function(values) paste(names(values), values, sep = "=", collapse = "\037"))
        first_indices <- which(!duplicated(signatures))
        encoded <- map_chr(extra_values[first_indices], as_json)
        encoded[match(signatures, signatures[first_indices])]
      }
      obs_tbl <- tibble(TIME_PERIOD = xml_attr(obs, "TIME_PERIOD"),
        OBS_VALUE = xml_attr(obs, "OBS_VALUE"), OBS_STATUS = xml_attr(obs, "OBS_STATUS"),
        observation_attributes_json = obs_json)
      if (nrow(obs_tbl) == 0) return(tibble())
      if (!"TIME_PERIOD" %in% names(obs_tbl)) rlang::abort("Missing TIME_PERIOD.", class = "imf_data_format_error")
      # A four-digit period is required even for datasets without a frequency dimension.
      obs_tbl <- obs_tbl %>% filter(str_detect(TIME_PERIOD, "^[0-9]{4}$"))
      if (nrow(obs_tbl) == 0) return(tibble())
      if (!"OBS_VALUE" %in% names(obs_tbl)) obs_tbl$OBS_VALUE <- NA_character_
      indicator_id <- intersect(c("INDICATOR", "SERIES", "SERIES_CODE"), names(dims))
      # Composite indicators use their source dimension codes and labels as the name.
      metric_ids <- setdiff(names(dims), c("COUNTRY", "REF_AREA", "COUNTERPART_COUNTRY",
        "COUNTERPART_AREA", "JURISDICTION", "FREQ", "FREQUENCY"))
      indicator <- if (length(indicator_id) > 0) dims[[indicator_id[[1]]]] else paste(dims[metric_ids], collapse = ".")
      indicator_name <- if (length(indicator_id) > 0) dim_labels[[indicator_id[[1]]]] else paste(dim_labels[metric_ids], collapse = " | ")
      # Expose common country, unit, scale and status fields for ordinary CSV users.
      shared <- c(dims, attributes)
      field_value <- function(candidates) {
        present <- intersect(candidates, names(shared))
        if (length(present) > 0) unname(shared[[present[[1]]]]) else NA_character_
      }
      unit <- field_value(c("UNIT", "UNIT_MEASURE"))
      reported_name <- field_value(c("SERIES_NAME", "INDICATOR_NAME"))
      if (!is.na(reported_name)) indicator_name <- reported_name
      tibble(database = flow$flow_id, database_name = flow$flow_name, agency = flow$agency_id,
        database_version = flow$version, is_vintage = flow$is_vintage,
        series_key = paste(dims, collapse = "."), indicator_code = indicator, indicator_name = indicator_name,
        year = as.integer(obs_tbl$TIME_PERIOD), value = obs_tbl$OBS_VALUE,
        reporting_country = field_value(c("COUNTRY", "REF_AREA", "REPORTER", "JURISDICTION")),
        counterpart_country = field_value(c("COUNTERPART_COUNTRY", "COUNTERPART_AREA", "COUNTERPART", "PARTNER")),
        frequency = "A", unit_code = unit, scale = field_value(c("SCALE", "UNIT_MULT")),
        last_actual_year = field_value(c("LATEST_ACTUAL_ANNUAL_DATA", "LAST_ACTUAL_YEAR")),
        observation_status = if ("OBS_STATUS" %in% names(obs_tbl)) obs_tbl$OBS_STATUS else NA_character_,
        dimensions_json = as_json(dims), dimension_labels_json = as_json(dim_labels),
        attributes_json = as_json(attributes),
        observation_attributes_json = obs_tbl$observation_attributes_json,
        venezuela_dimension = query$venezuela_dimension, source_url = query$source_url,
        downloaded_at_utc = query$downloaded_at_utc)
    })
  })
  message("Parsed: ", query$query_id, " (", nrow(observations[[i]]), " annual observations)")
}

# Deduplicate overlapping reporter/partner queries without combining distinct vintages.
annual_raw <- bind_rows(observations)
conflicts <- annual_raw %>%
  distinct(database, database_version, series_key, year, value) %>%
  count(database, database_version, series_key, year) %>% filter(n > 1)
if (nrow(conflicts) > 0) rlang::abort("Overlapping API responses disagree on observation values.", class = "imf_conflicting_observation_error")
annual <- annual_raw %>%
  distinct(database, database_version, series_key, year, .keep_all = TRUE) %>%
  arrange(database, series_key, year)
if (nrow(annual) == 0) rlang::abort("No annual Venezuela observations downloaded.", class = "imf_empty_export_error")
# Store repeated descriptive metadata once per series, keeping all values in the main CSV.
write_csv(annual %>% select(-dimension_labels_json, -attributes_json),
  file.path(export_dir, "imf_venezuela_annual.csv"), na = "")
series_catalog <- annual %>%
  group_by(database, database_name, database_version, is_vintage, series_key,
    indicator_code, indicator_name, dimensions_json, dimension_labels_json, attributes_json) %>%
  summarise(first_year = min(year), last_year = max(year), observations = n(), .groups = "drop")
write_csv(series_catalog, file.path(export_dir, "imf_venezuela_series.csv"), na = "")
coverage <- coverage %>%
  select(-query_count, -observations) %>%
  left_join(request_log %>% group_by(flow_id) %>% summarise(query_count = n(),
    failed_queries = sum(is.na(http_status) | !http_status %in% c(200, 204, 404)), .groups = "drop"), by = "flow_id") %>%
  left_join(annual %>% count(database, name = "observations"), by = c("flow_id" = "database")) %>%
  mutate(across(c(query_count, failed_queries, observations), ~ replace_na(.x, 0L)),
    status = case_when(failed_queries > 0 ~ "download_error", observations > 0 ~ "downloaded",
      status == "pending" ~ "no_annual_observations", TRUE ~ status)) %>%
  left_join(flows %>% select(-structure_url), by = "flow_id")
write_csv(coverage, file.path(export_dir, "imf_coverage.csv"))
message("Saved ", nrow(annual), " observations across ", nrow(series_catalog), " series and ", n_distinct(annual$database), " databases.")
if (any(coverage$failed_queries > 0)) rlang::abort("Export is incomplete: failed data queries are recorded in imf_download_log.csv.", class = "imf_incomplete_export_error")
