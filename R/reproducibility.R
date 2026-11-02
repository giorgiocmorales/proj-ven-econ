# Reproducibility storage and orchestration. All paths are relative to an explicit
# project root; historical objects and run directories are never overwritten.

# Hash file bytes rather than serialized R objects, including empty source files.
repro_hash <- function(path) {
  # Small-file byte reads avoid repeated Windows owner lookups inside file hashing.
  # Large artifacts retain streaming hashing with bounded memory consumption.
  size <- file.info(path, extra_cols = FALSE)$size
  if (size > 64 * 1024^2) return(digest::digest(file = path, algo = "sha256", serialize = FALSE))
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  digest::digest(readBin(connection, what = "raw", n = size), algo = "sha256", serialize = FALSE)
}

# Fail before copying outside a run, including traversal through existing symlinks.
repro_relative <- function(path) {
  if (anyNA(path) || any(!nzchar(path)) ||
      any(grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", path))) {
    rlang::abort("Expected a project-relative path without traversal", class = "repro_path_error")
  }
  path
}

# Enumerate original inputs only; the new object store must never inventory itself.
repro_files <- function(root) {
  # Prune package junctions before traversing, including nested artifact runtimes.
  walk <- function(relative) {
    entries <- list.files(file.path(root, relative), recursive = FALSE,
      full.names = FALSE, all.files = TRUE, no.. = TRUE)
    entries <- setdiff(entries, c(".git", ".Rproj.user", ".quarto", "renv", "node_modules", ".venv", "__pycache__"))
    if (relative == "data") entries <- setdiff(entries, c("reproducibility", "pipeline"))
    unlist(purrr::map(entries, function(entry) {
      path <- if (nzchar(relative)) file.path(relative, entry) else entry
      if (dir.exists(file.path(root, path))) walk(path) else path
    }), use.names = FALSE)
  }
  paths <- gsub("\\\\", "/", walk(""))
  sort(paths[!grepl("^tmp/", paths) | grepl("\\.(R|r|md)$", paths)])
}

# Check the lock without rewriting it or installing into a global library.
repro_dependencies <- function(root) {
  lock_path <- file.path(root, "renv.lock")
  if (!jsonlite::validate(readr::read_file(lock_path))) {
    rlang::abort("renv.lock is invalid JSON", class = "repro_dependency_error")
  }
  lock <- jsonlite::read_json(lock_path, simplifyVector = FALSE)
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  packages <- trimws(gsub("\\s*\\([^)]*\\)", "", unlist(strsplit(description[1, "Imports"], ","))))
  result <- purrr::map_dfr(packages, function(package) {
    installed <- requireNamespace(package, quietly = TRUE)
    version <- if (installed) as.character(utils::packageVersion(package)) else NA_character_
    record <- lock$Packages[[package]]
    pinned <- if (is.null(record)) NA_character_ else record$Version
    priority <- if (installed) utils::packageDescription(package)$Priority else NULL
    base <- !is.null(priority) && priority %in% c("base", "recommended")
    tibble::tibble(package = package, installed_version = version, locked_version = pinned,
      matches = installed && (base || (!is.na(pinned) && utils::compareVersion(version, pinned) == 0L)))
  })
  if (any(!result$matches) || !identical(as.character(getRversion()), lock$R$Version)) {
    print(result %>% dplyr::filter(!.data$matches))
    rlang::abort("R/dependency versions differ from renv.lock; restore the project environment", class = "repro_dependency_error")
  }
  result
}

# Retain the exact official terms pages separately from statistical source bytes.
# Reviewed policy summaries are evidence, not a grant of third-party permission.
repro_policies <- function(root, store, manifest_dir) {
  policies <- readr::read_csv(file.path(root, "registry/reproducibility_licenses.csv"),
    show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  dir.create(file.path(store, "policies"), recursive = TRUE, showWarnings = FALSE)
  directory <- tempfile("policies-", tmpdir = file.path(store, "policies"))
  dir.create(directory)
  options(repro.command = list(entry_point = "scripts/run_reproducibility.R", arguments = "policies"))
  fetch <- repro_transport("update", file.path(directory, "http"), "", store)
  rows <- purrr::map_dfr(seq_len(nrow(policies)), function(i) {
    path <- file.path(directory, paste0(policies$provider[i], ".html"))
    fetch(policies$policy_url[i], path, 2L, 45L)
    text <- rvest::html_text2(xml2::read_html(path))
    verified <- grepl(policies$expected_marker[i], text, fixed = TRUE)
    tibble::tibble(provider = policies$provider[i], policy_url = policies$policy_url[i],
      sha256 = repro_hash(path), marker_verified = verified, snapshot_file = path)
  })
  readr::write_csv(rows, file.path(manifest_dir, "policy_snapshots.csv"))
  if (any(!rows$marker_verified)) {
    rlang::abort("Policy response lacks expected text; inspect preserved snapshot", class = "repro_policy_error")
  }
  message("Preserved ", nrow(rows), " official policy snapshots")
  invisible(rows)
}

# Extract exact metadata cells separately from lexical evidence. URL templates in
# code cannot be promoted into retrieval commands by this audit.
repro_source_records <- function(root, paths) {
  candidates <- paths[grepl("^(data/raw/|registry/)", paths) & grepl("\\.csv$", paths) &
    !grepl("^registry/reproducibility/", paths)]
  purrr::map_dfr(candidates, function(path) {
    value <- readr::read_csv(file.path(root, path), show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()), name_repair = "unique")
    readr::stop_for_problems(value)
    fields <- names(value)[grepl("url|license|retriev|download|vintage|version|sha256|md5|access|origin|request|source_(file|id)",
                                 names(value), ignore.case = TRUE)]
    if (!length(fields)) return(tibble::tibble(evidence_file = character(), field = character(), value = character()))
    value %>% dplyr::select(dplyr::all_of(fields)) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "field", values_to = "value") %>%
      dplyr::filter(!is.na(.data$value), nzchar(.data$value)) %>%
      dplyr::distinct(.data$field, .data$value) %>%
      dplyr::mutate(evidence_file = path, .before = 1)
  })
}

# Bind origins to physical files only when existing records or constant importer
# assignments establish that relationship. Unresolved provenance remains missing.
repro_file_origins <- function(root, paths, records) {
  candidates <- paths[grepl("(download_(log|receipt)\\.csv$|registry/financial_bcv_sources\\.csv$)", paths)]
  receipts <- purrr::map_dfr(candidates, function(path) {
    value <- readr::read_csv(file.path(root, path), show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()))
    readr::stop_for_problems(value)
    urls <- intersect(c("source_url", "pdf_url", "export_url", "url"), names(value))
    files <- intersect(c("raw_file", "file_path", "pdf_path", "local_path"), names(value))
    if (!length(urls)) return(tibble::tibble())
    if (!length(files) && basename(path) == "download_receipt.csv" &&
        file.exists(file.path(root, dirname(path), "momr.pdf"))) {
      value$raw_file <- file.path(dirname(path), "momr.pdf")
      files <- "raw_file"
    }
    if (!length(files)) return(tibble::tibble())
    tibble::tibble(path = value[[files[1]]], origin_url = value[[urls[1]]],
      origin_evidence = path, origin_status = "existing_receipt_or_registry_not_live_reverified")
  })
  # An API extract can contain multiple exact query URLs; retain all of them.
  api <- records %>% dplyr::filter(.data$field == "source_url", startsWith(.data$evidence_file, "data/raw/")) %>%
    dplyr::transmute(path = .data$evidence_file, origin_url = .data$value,
      origin_evidence = .data$evidence_file, origin_status = "embedded_query_url_original_response_may_be_unavailable")
  # Resolve only literal strings and file.path() with literal string arguments.
  # No arbitrary expression from the research code is evaluated here.
  scripts <- paths[grepl("^scripts/.*\\.R$", paths)]
  constants <- purrr::map_dfr(scripts, function(script) {
    expressions <- parse(file.path(root, script), encoding = "UTF-8")
    values <- list()
    for (expression in expressions) {
      if (is.call(expression) && identical(expression[[1]], as.name("<-")) && is.symbol(expression[[2]])) {
        right <- expression[[3]]
        if (is.character(right) && length(right) == 1L) values[[as.character(expression[[2]])]] <- right
        else if (is.call(right) && identical(right[[1]], as.name("file.path")) &&
            all(vapply(as.list(right)[-1], is.character, logical(1)))) {
          values[[as.character(expression[[2]])]] <- do.call(file.path, as.list(right)[-1])
        }
      }
    }
    if (is.null(values$source_url) || is.null(values$raw_path)) return(tibble::tibble())
    tibble::tibble(path = values$raw_path, origin_url = values$source_url,
      origin_evidence = script, origin_status = "constant_importer_assignment_not_live_reverified")
  })
  dplyr::bind_rows(receipts, api, constants) %>%
    dplyr::filter(!is.na(.data$path), !is.na(.data$origin_url), nzchar(.data$origin_url)) %>%
    dplyr::distinct()
}

# A byte manifest covers raw, intermediate and final data as well as source documents.
repro_inventory <- function(root, paths) {
  tibble::tibble(path = paths, bytes = as.double(file.info(file.path(root, paths), extra_cols = FALSE)$size),
    sha256 = purrr::map_chr(file.path(root, paths), repro_hash),
    observed_mtime = as.double(file.info(file.path(root, paths), extra_cols = FALSE)$mtime))
}

# Copy a content-addressed object once and verify both existing and new objects.
repro_store <- function(path, store) {
  hash <- repro_hash(path)
  repro_store_known(path, hash, store)
}

# Reuse a just-computed source hash when snapshotting a large input inventory.
repro_store_known <- function(path, hash, store) {
  target <- file.path(store, "objects", substr(hash, 1, 2), hash)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  # Concurrent source updates share the object store. Serialize publication of each
  # hash and rename a verified temporary copy so readers never see partial bytes.
  lock <- paste0(target, ".lock")
  acquired <- FALSE
  for (attempt in seq_len(100L)) {
    acquired <- dir.create(lock, showWarnings = FALSE)
    if (acquired) break
    Sys.sleep(0.1)
  }
  if (!acquired) rlang::abort(paste("Object writer lock is busy:", lock), class = "repro_lock_error")
  store_absolute <- normalizePath(store, winslash = "/", mustWork = TRUE)
  lock_absolute <- normalizePath(lock, winslash = "/", mustWork = TRUE)
  if (!startsWith(lock_absolute, paste0(store_absolute, "/objects/"))) {
    rlang::abort("Object lock escaped its store", class = "repro_path_error")
  }
  on.exit(unlink(lock_absolute, recursive = TRUE), add = TRUE)
  candidate <- tempfile(".stage-", tmpdir = dirname(target))
  on.exit(unlink(candidate), add = TRUE)
  if (!file.exists(target)) {
    if (!file.copy(path, candidate, overwrite = FALSE)) {
      rlang::abort(paste("Could not stage", path), class = "repro_copy_error")
    }
    if (!identical(repro_hash(candidate), hash)) {
      rlang::abort(paste("Source changed while staging", path), class = "repro_integrity_error")
    }
    if (!file.rename(candidate, target)) {
      rlang::abort(paste("Could not publish object", target), class = "repro_copy_error")
    }
  }
  if (!identical(repro_hash(target), hash)) {
    rlang::abort(paste("Corrupt object", target), class = "repro_integrity_error")
  }
  hash
}

# Write immutable receipts. Repeated deterministic inventories must match exactly.
repro_write_once <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  candidate <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(candidate), add = TRUE)
  readr::write_csv(value, candidate, na = "")
  if (file.exists(path)) {
    if (!identical(repro_hash(path), repro_hash(candidate))) {
      rlang::abort(paste("Refusing to replace immutable manifest", path), class = "repro_immutable_error")
    }
  } else if (!file.copy(candidate, path, overwrite = FALSE)) {
    rlang::abort(paste("Could not write", path), class = "repro_copy_error")
  }
  invisible(path)
}

# Classify by reviewed path selectors. Unmapped files remain explicit audit gaps.
repro_classify <- function(paths, sources) {
  purrr::map_chr(paths, function(path) {
    matches <- sources$source_id[vapply(sources$input_pattern, grepl,
      x = path, FUN.VALUE = logical(1), perl = TRUE)]
    paste(sort(unique(matches)), collapse = ";")
  })
}

# Audit literal evidence without treating URL templates or comments as verified URLs.
# Include ad hoc history, scripts, registries, raw ledgers, and source sidecars.
repro_evidence <- function(root, paths) {
  candidates <- paths[grepl("(\\.(R|r|qmd|md|csv|json|yml|yaml|js|txt|ps1)$|^\\.Rhistory$)", paths) &
    !grepl("^(registry/reproducibility/|data/(interim|processed|final)/)", paths)]
  purrr::map_dfr(candidates, function(path) {
    lines <- readLines(file.path(root, path), warn = FALSE, encoding = "UTF-8")
    hits <- stringr::str_extract_all(lines,
      "https?://[^\\s\"'<>),]+|[A-Za-z]:[/\\\\][^\"'<>]+|data/(?:raw|manual_fix)/[^\"'<>),]+")
    tibble::tibble(evidence_file = path, line = rep(seq_along(lines), lengths(hits)),
      literal = as.character(unlist(hits, use.names = FALSE))) %>%
      dplyr::distinct(.data$evidence_file, .data$literal, .keep_all = TRUE) %>%
      dplyr::mutate(evidence_kind = dplyr::case_when(
        grepl("^https?://", .data$literal) & grepl("%[sdi]|\\{|\\$", .data$literal) ~ "url_template_unverified",
        grepl("^https?://", .data$literal) ~ "url_literal_unverified",
        grepl("^[A-Za-z]:", .data$literal) ~ "external_local_reference",
        TRUE ~ "raw_path_literal"))
  })
}

# Parse every R source for package, source() and file literals without executing it.
# The expression text and source checksum identify transformations, even when paths
# are constructed dynamically and cannot safely be inferred by a static inventory.
repro_code_inventory <- function(root, paths) {
  candidates <- paths[grepl("\\.[Rr]$", paths)]
  purrr::map_dfr(candidates, function(path) {
    parsed <- parse(file.path(root, path), keep.source = TRUE, encoding = "UTF-8")
    tokens <- utils::getParseData(parsed)
    strings <- tokens$text[tokens$token == "STR_CONST"]
    tibble::tibble(script = path, sha256 = repro_hash(file.path(root, path)),
      statements = length(parsed),
      literals = paste(sort(unique(strings)), collapse = " | "))
  })
}

# Include URLs from reachable Git source blobs, including removed/ad hoc importers.
# Git object IDs preserve the exact historical context; literals remain unverified.
repro_git_evidence <- function(root) {
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  setwd(root)
  objects <- system2("git", c("rev-list", "--objects", "--all"), stdout = TRUE)
  if (!is.null(attr(objects, "status"))) rlang::abort("Cannot inspect Git history", class = "repro_git_error")
  objects <- objects[grepl(" (R/|scripts/|.*\\.(R|r|Rmd|qmd|md)$)", objects)]
  purrr::map_dfr(objects, function(object) {
    blob <- substr(object, 1, 40)
    path <- substring(object, 42)
    if (!grepl("\\.(R|r|Rmd|qmd|md)$", path)) return(tibble::tibble())
    lines <- system2("git", c("cat-file", "-p", blob), stdout = TRUE)
    if (!is.null(attr(lines, "status"))) rlang::abort(paste("Cannot inspect", blob), class = "repro_git_error")
    hits <- stringr::str_extract_all(lines, "https?://[^\\s\"'<>]+")
    tibble::tibble(git_blob = blob, historical_path = path,
      line = rep(seq_along(lines), lengths(hits)), literal = as.character(unlist(hits, use.names = FALSE))) %>%
      dplyr::distinct(.data$git_blob, .data$literal, .keep_all = TRUE)
  })
}

# Snapshot all existing inputs and code once. Unknown origin stays unknown; file
# modification time is never relabelled as a historical retrieval timestamp.
repro_baseline <- function(root, store, manifest_dir) {
  if (file.exists(file.path(manifest_dir, "baseline_files.csv"))) {
    rlang::abort("Baseline already exists; use audit or an incremental run", class = "repro_immutable_error")
  }
  paths <- repro_files(root)
  paths <- paths[!grepl("^registry/reproducibility/", paths)]
  message("Hashing ", length(paths), " baseline files")
  inventory <- repro_inventory(root, paths)
  sources <- readr::read_csv(file.path(root, "registry/reproducibility_sources.csv"),
                            show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  inventory <- inventory %>% dplyr::mutate(source_ids = repro_classify(.data$path, sources),
    role = dplyr::case_when(grepl("^data/raw/", .data$path) ~ "raw_or_legacy_api_extract",
      grepl("^data/manual_fix/", .data$path) ~ "manual_conversion",
      grepl("^data/", .data$path) ~ "derived_baseline",
      TRUE ~ "code_configuration_or_document"))
  # Preserve data and project source documents plus the exact parser/configuration.
  message("Preserving verified baseline objects")
  purrr::walk2(file.path(root, inventory$path), inventory$sha256, repro_store_known, store = store)
  repro_write_once(inventory, file.path(manifest_dir, "baseline_files.csv"))
  jsonlite::write_json(list(schema_version = 1L,
    inventoried_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    historical_retrieval_time = "unknown unless original sidecar records it",
    object_store = "data/reproducibility", files = nrow(inventory)),
    file.path(manifest_dir, "baseline.json"), pretty = TRUE, auto_unbox = TRUE)
  invisible(inventory)
}

# Report changes rather than accepting them into the historical baseline.
repro_audit <- function(root, store, manifest_dir) {
  paths <- repro_files(root)
  paths <- paths[!grepl("^registry/reproducibility/", paths)]
  evidence <- repro_evidence(root, paths)
  code <- repro_code_inventory(root, paths)
  historical <- repro_git_evidence(root)
  records <- repro_source_records(root, paths)
  origins <- repro_file_origins(root, paths, records)
  dependencies <- repro_dependencies(root)
  readr::write_csv(records, file.path(manifest_dir, "source_records.csv"))
  readr::write_csv(origins, file.path(manifest_dir, "file_origins.csv"))
  readr::write_csv(dependencies, file.path(manifest_dir, "dependencies.csv"))
  readr::write_csv(historical, file.path(manifest_dir, "git_source_evidence.csv"))
  readr::write_csv(evidence, file.path(manifest_dir, "source_evidence.csv"))
  readr::write_csv(code, file.path(manifest_dir, "code_inventory.csv"))
  baseline <- readr::read_csv(file.path(manifest_dir, "baseline_files.csv"),
    show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character(), bytes = readr::col_double()))
  current <- repro_inventory(root, paths)
  comparison <- dplyr::full_join(baseline %>% dplyr::select(path, baseline_sha256 = sha256),
    current %>% dplyr::select(path, current_sha256 = sha256), by = "path") %>%
    dplyr::mutate(status = dplyr::case_when(is.na(.data$baseline_sha256) ~ "added",
      is.na(.data$current_sha256) ~ "missing", .data$baseline_sha256 != .data$current_sha256 ~ "changed",
      TRUE ~ "unchanged"))
  readr::write_csv(comparison, file.path(manifest_dir, "baseline_comparison.csv"))
  external <- evidence %>% dplyr::filter(.data$evidence_kind == "external_local_reference") %>%
    dplyr::mutate(exists = file.exists(.data$literal),
      sha256 = purrr::map_chr(.data$literal, function(path) {
        if (file.exists(path) && !dir.exists(path)) repro_hash(path) else NA_character_
      }))
  readr::write_csv(external, file.path(manifest_dir, "external_references.csv"))
  sources <- readr::read_csv(file.path(root, "registry/reproducibility_sources.csv"),
    show_col_types = FALSE, na = character(), col_types = readr::cols(.default = readr::col_character()))
  raw <- current %>% dplyr::filter(grepl("^data/(raw|manual_fix)/", .data$path)) %>%
    dplyr::mutate(source_ids = repro_classify(.data$path, sources),
      origin_status = ifelse(nzchar(.data$source_ids), "profile_and_evidence_require_review", "unmapped_origin"),
      historical_retrieval_time = "unknown unless original sidecar records it")
  origin_summary <- origins %>% dplyr::group_by(.data$path) %>%
    dplyr::summarise(origin_urls = paste(sort(unique(.data$origin_url)), collapse = " | "),
      origin_evidence_files = paste(sort(unique(.data$origin_evidence)), collapse = " | "), .groups = "drop")
  raw <- raw %>% dplyr::left_join(origin_summary, by = "path")
  readr::write_csv(raw, file.path(manifest_dir, "input_provenance.csv"))
  repro_run_status(store, sources, manifest_dir)
  jsonlite::write_json(list(audited_at_utc = format(base::Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    files = nrow(current), r_scripts = nrow(code), source_profiles = nrow(sources),
    source_literals = nrow(evidence), raw_and_manual_files = nrow(raw),
    changed_original_data = sum(startsWith(comparison$path, "data/") & comparison$status != "unchanged")),
    file.path(manifest_dir, "audit_summary.json"), auto_unbox = TRUE, pretty = TRUE)
  message(sprintf("Audited %s files, %s R scripts, %s source literals; %s baseline differences",
    nrow(current), nrow(code), nrow(evidence), sum(comparison$status != "unchanged")))
  invisible(comparison)
}

# Distinguish implemented capabilities from the sources actually verified here.
repro_run_status <- function(store, sources, manifest_dir) {
  directories <- list.dirs(file.path(store, "runs"), recursive = FALSE, full.names = TRUE)
  runs <- purrr::map_dfr(directories, function(directory) {
    path <- file.path(directory, "result.json")
    config_path <- file.path(directory, "config.rds")
    if (file.exists(path)) result <- jsonlite::read_json(path, simplifyVector = TRUE)
    else if (file.exists(config_path)) {
      config <- readRDS(config_path)
      result <- list(status = "incomplete_controller_run", source_id = config$source_id, mode = config$mode)
    } else result <- list(status = "incomplete_workspace", source_id = "unknown", mode = "unknown")
    tibble::tibble(run_id = basename(directory), source_id = result$source_id,
      mode = result$mode, status = result$status,
      changed_files = if (is.null(result$changed_files)) NA_integer_ else result$changed_files,
      http_responses = length(list.files(file.path(directory, "http"), pattern = "\\.json$")))
  })
  readr::write_csv(runs, file.path(manifest_dir, "runs.csv"))
  status <- sources %>% dplyr::mutate(
    verified_update = purrr::map_lgl(.data$source_id, function(id) any(runs$source_id == id & runs$mode == "update" & runs$status == "success")),
    verified_replay = purrr::map_lgl(.data$source_id, function(id) any(runs$source_id == id & runs$mode == "replay" & runs$status == "success")),
    verified_rebuild = purrr::map_lgl(.data$source_id, function(id) any(runs$source_id == id & runs$mode == "rebuild" & runs$status == "success")))
  readr::write_csv(status, file.path(manifest_dir, "source_status.csv"))
  invisible(runs)
}

# Run only reviewed regression suites that keep their mutations in temporary files.
repro_checks <- function(root, store, manifest_dir, sources) {
  suites <- c("reproducibility", "bcv_statements", "fx_intervention_conversion",
    "report_pipeline", "opec_downloads", "historical_inflation", "reuters_exports")
  dir.create(file.path(store, "checks"), recursive = TRUE, showWarnings = FALSE)
  directory <- tempfile("checks-", tmpdir = file.path(store, "checks"))
  dir.create(directory)
  checks <- purrr::map_dfr(suites, function(suite) {
    config_path <- file.path(directory, paste0(suite, ".rds"))
    saveRDS(list(mode = "test", workspace = root, library_paths = .libPaths(),
      script = paste0("tests/", suite, ".R")), config_path, version = 3)
    status <- system2(file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", shQuote(file.path(root, "scripts/reproducibility_worker.R")), shQuote(config_path)),
      stdout = file.path(directory, paste0(suite, ".stdout.log")),
      stderr = file.path(directory, paste0(suite, ".stderr.log")))
    tibble::tibble(suite = suite, exit_code = status, passed = status == 0L, log_directory = directory)
  })
  readr::write_csv(checks, file.path(manifest_dir, "checks.csv"))
  repro_run_status(store, sources, manifest_dir)
  if (any(!checks$passed)) rlang::abort(paste("Regression checks failed; see", directory), class = "repro_test_error")
  message("All ", nrow(checks), " regression suites passed; logs: ", directory)
  invisible(checks)
}

# Verify every published blob, including HTTP payloads and prior parser versions.
# Corrupt data are reported rather than silently repaired from a convenience copy.
repro_check_objects <- function(store, manifest_dir) {
  paths <- list.files(file.path(store, "objects"), recursive = TRUE, full.names = TRUE,
    pattern = "^[a-f0-9]{64}$")
  objects <- tibble::tibble(sha256 = basename(paths),
    actual_sha256 = purrr::map_chr(paths, repro_hash),
    bytes = as.double(file.info(paths, extra_cols = FALSE)$size)) %>%
    dplyr::mutate(valid = .data$sha256 == .data$actual_sha256)
  readr::write_csv(objects, file.path(manifest_dir, "objects.csv"))
  if (any(!objects$valid)) {
    rlang::abort(paste("Archive objects failed SHA-256 validation:",
      paste(objects$sha256[!objects$valid], collapse = ", ")), class = "repro_integrity_error")
  }
  message("Verified ", nrow(objects), " content-addressed objects")
  invisible(objects)
}

# Validate staged bytes with format-aware readers; dataset semantics belong to the
# registered importer, which also validates source-specific sheets, units and keys.
repro_validate_file <- function(path, format) {
  if (!file.exists(path) || dir.exists(path) || file.info(path, extra_cols = FALSE)$size == 0) {
    rlang::abort(paste("Missing or empty staged input", path), class = "repro_input_error")
  }
  if (format == "json") jsonlite::fromJSON(path, simplifyVector = FALSE)
  else if (format == "csv") {
    value <- readr::read_csv(path, show_col_types = FALSE,
                             col_types = readr::cols(.default = readr::col_character()))
    readr::stop_for_problems(value)
  } else if (format %in% c("xls", "xlsx")) {
    if (!length(readxl::excel_sheets(path))) {
      rlang::abort("Workbook has no worksheets", class = "repro_schema_error")
    }
  } else if (format == "pdf") pdftools::pdf_info(path)
  else if (!format %in% c("png", "jpg", "jpeg", "html", "txt", "js", "xml", "binary")) {
    rlang::abort(paste("Unsupported input format", format), class = "repro_config_error")
  }
  invisible(path)
}

# Stage a supplied file or directory with an explicit origin and publication vintage.
# No URL or original author is inferred from the filename or its observed values.
repro_ingest <- function(source_id, input, origin, vintage, store, sources) {
  if (!source_id %in% sources$source_id || !nzchar(origin) || !nzchar(vintage)) {
    rlang::abort("Ingestion requires a registered source, origin and vintage", class = "repro_config_error")
  }
  if (!file.exists(input)) rlang::abort("Supplied input is missing", class = "repro_input_error")
  paths <- if (dir.exists(input)) list.files(input, recursive = TRUE, full.names = TRUE) else input
  if (!length(paths)) rlang::abort("Supplied directory is empty", class = "repro_input_error")
  root <- if (dir.exists(input)) normalizePath(input, winslash = "/") else dirname(normalizePath(input, winslash = "/"))
  rows <- purrr::map_dfr(paths, function(path) {
    format <- tolower(tools::file_ext(path))
    repro_validate_file(path, format)
    tibble::tibble(source_id = source_id,
      relative_path = substring(normalizePath(path, winslash = "/"), nchar(root) + 2L),
      sha256 = repro_store(path, store), bytes = as.double(file.info(path, extra_cols = FALSE)$size),
      format = format, origin = origin, vintage = vintage)
  }) %>% dplyr::arrange(.data$relative_path)
  manifest_id <- digest::digest(rows, algo = "sha256", serialize = TRUE)
  target <- file.path(store, "ingestions", source_id, paste0(manifest_id, ".csv"))
  repro_write_once(rows, target)
  message("Staged ingestion manifest: ", target)
  invisible(target)
}

# Capture exact HTTP bytes and response metadata. Replay uses only the exact URL
# recorded in a chosen run; an absent request fails instead of using the network.
repro_transport <- function(mode, capture_dir, replay_dir, store) {
  force(mode); force(capture_dir); force(replay_dir); force(store)
  function(url, dest_path, max_attempts, timeout_seconds) {
    key <- digest::digest(url, algo = "sha256", serialize = FALSE)
    receipt_path <- file.path(capture_dir, paste0(key, ".json"))
    if (mode == "replay") {
      original <- file.path(replay_dir, paste0(key, ".json"))
      if (!file.exists(original)) {
        rlang::abort(paste("No captured response for", url), class = "repro_replay_missing")
      }
      receipt <- jsonlite::read_json(original, simplifyVector = TRUE)
      if (!identical(receipt$url, url) || receipt$status < 200L || receipt$status >= 300L) {
        rlang::abort("Replay request/status mismatch", class = "repro_integrity_error")
      }
      object <- file.path(store, "objects", substr(receipt$sha256, 1, 2), receipt$sha256)
      if (!file.exists(object) || !identical(repro_hash(object), receipt$sha256)) {
        rlang::abort("Replay response failed its SHA-256 check", class = "repro_integrity_error")
      }
      dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(object, dest_path, overwrite = TRUE)) {
        rlang::abort("Could not restore response", class = "repro_copy_error")
      }
    } else {
      response <- httr2::request(url) %>% httr2::req_timeout(timeout_seconds) %>%
        httr2::req_retry(max_tries = max_attempts) %>%
        httr2::req_error(is_error = function(response) FALSE) %>% httr2::req_perform()
      candidate <- tempfile(fileext = ".response")
      on.exit(unlink(candidate), add = TRUE)
      writeBin(httr2::resp_body_raw(response), candidate)
      hash <- repro_store(candidate, store)
      receipt <- list(url = url, final_url = httr2::resp_url(response),
        status = httr2::resp_status(response), sha256 = hash,
        retrieved_at_utc = format(base::Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        etag = httr2::resp_header(response, "etag"),
        last_modified = httr2::resp_header(response, "last-modified"),
        content_type = httr2::resp_header(response, "content-type"),
        command = getOption("repro.command"))
      dir.create(capture_dir, recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(receipt, receipt_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
      if (receipt$status < 200L || receipt$status >= 300L) {
        rlang::abort(paste("HTTP", receipt$status, url), class = "repro_http_error")
      }
      dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(candidate, dest_path, overwrite = TRUE)) {
        rlang::abort("Could not save response", class = "repro_copy_error")
      }
    }
    dir.create(capture_dir, recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(receipt, receipt_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    # Source readers using file mtime see the same captured retrieval instant.
    if (!is.null(receipt$retrieved_at_utc)) {
      Sys.setFileTime(dest_path, as.POSIXct(receipt$retrieved_at_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
    }
    dest_path
  }
}

# Build the request through the installed rsdmx provider's own formatter/handler.
# This retains provider compatibility without inventing an IMF URL convention.
repro_imf_request <- function(flow_id, key, start_year, end_year, transport) {
  provider <- rsdmx::findSDMXServiceProvider("IMF_DATA")
  builder <- provider@builder
  params <- rsdmx::SDMXRequestParams(regUrl = builder@regUrl, repoUrl = builder@repoUrl,
    accessKey = NULL, providerId = "IMF_DATA", agencyId = NULL, resource = "data",
    resourceId = NULL, version = NULL, flowRef = flow_id, key = key,
    start = start_year, end = end_year, references = NULL, compliant = builder@compliant)
  url <- builder@handler$data(builder@formatter$data(params))
  path <- tempfile(fileext = ".xml")
  on.exit(unlink(path), add = TRUE)
  transport(url, path, 3L, 120L)
  rsdmx::readSDMX(file = path, isURL = FALSE, verbose = FALSE)
}

# Restore a verified snapshot into a private workspace. Existing workspaces are
# rejected; updates and manual overlays cannot mutate the original project tree.
repro_restore <- function(manifest, workspace, store) {
  if (dir.exists(workspace)) rlang::abort("Run workspace already exists", class = "repro_immutable_error")
  dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
  purrr::pwalk(manifest %>% dplyr::select(path, sha256), function(path, sha256) {
    repro_relative(path)
    object <- file.path(store, "objects", substr(sha256, 1, 2), sha256)
    if (!file.exists(object) || !identical(repro_hash(object), sha256)) {
      rlang::abort(paste("Missing/corrupt object for", path), class = "repro_integrity_error")
    }
    target <- file.path(workspace, path)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(object, target, overwrite = FALSE)) {
      rlang::abort(paste("Could not restore", path), class = "repro_copy_error")
    }
    if ("observed_mtime" %in% names(manifest)) {
      observed <- as.double(manifest$observed_mtime[manifest$path == path])
      if (!is.na(observed)) Sys.setFileTime(target, as.POSIXct(observed, origin = "1970-01-01", tz = "UTC"))
    }
  })
  invisible(workspace)
}

# Run registered importers in a fresh R process: rm(list=ls()) in legacy scripts
# cannot erase the controller. A failed run retains its log and never publishes.
repro_run <- function(root, store, sources, source_id, run_id, mode, parent_run, ingestion) {
  if (!grepl("^[A-Za-z0-9_-]+$", run_id) || !source_id %in% sources$source_id) {
    rlang::abort("Invalid run ID or unknown source", class = "repro_config_error")
  }
  profile <- sources[sources$source_id == source_id, ]
  if (mode == "update" && source_id == "imf_annual_archive") {
    rlang::abort("Automated IMF bulk-download permission is not recorded; preserve/use the existing archive or ingest an authorized export. See registry/reproducibility_licenses.csv",
      class = "repro_source_access_error")
  }
  if (mode == "replay" && !nzchar(parent_run)) {
    rlang::abort("Replay requires a successful parent run", class = "repro_config_error")
  }
  if (mode == "rebuild" && !source_id %in% c("opec_cached", "reuters_exports", "bcv_smc_sales",
      "interventions_workbook", "seniat_research", "economia_workbook", "mercantil_qa", "cross_country_macro",
      "energy_institute_workbook", "pib_hist_workbook", "interest_rate_documents", "seniat_supplements",
      "economia_oil_audit", "un_comtrade", "palma_fx", "wikipedia_minimum_wage")) {
    rlang::abort("Use update or replay for this online source", class = "repro_unsupported_error")
  }
  if (mode == "replay" && profile$replay != "supported") {
    rlang::abort(paste("Replay needs source-specific work:", profile$gaps), class = "repro_unsupported_error")
  }
  if (!nzchar(profile$scripts)) {
    rlang::abort(paste("No verified importer:", profile$gaps), class = "repro_unsupported_error")
  }
  run_dir <- file.path(store, "runs", run_id)
  if (dir.exists(run_dir)) rlang::abort("Run ID already exists", class = "repro_immutable_error")
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  # Record controller failures without catching or replacing the original error.
  withCallingHandlers({
  manifest_path <- if (nzchar(parent_run)) file.path(store, "runs", parent_run, "files.csv") else
    file.path(root, "registry/reproducibility/baseline_files.csv")
  if (nzchar(parent_run)) {
    repro_relative(parent_run)
    status <- jsonlite::read_json(file.path(store, "runs", parent_run, "result.json"))
    if (!identical(status$status, "success")) rlang::abort("Parent run did not succeed", class = "repro_input_error")
    if (mode == "replay" && !identical(status$source_id, source_id)) {
      rlang::abort("Replay source differs from parent", class = "repro_input_error")
    }
  }
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character(), bytes = readr::col_double()))
  # A parent contains its source workspace, not unrelated raw corpora. Supply other
  # original inputs from the immutable baseline when beginning a different source.
  if (nzchar(parent_run) && mode != "replay") {
    baseline <- readr::read_csv(file.path(root, "registry/reproducibility/baseline_files.csv"),
      show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character(), bytes = readr::col_double()))
    manifest <- dplyr::bind_rows(manifest, dplyr::anti_join(baseline, manifest, by = "path"))
  }
  # Stage source-matched inputs and literal file dependencies, not unrelated large
  # corpora. Dynamically constructed source paths are declared in input_pattern.
  scripts <- strsplit(profile$scripts, ";", fixed = TRUE)[[1]]
  literals <- unlist(purrr::map(c(scripts, list.files(file.path(root, "R"), pattern = "\\.R$",
      full.names = FALSE) %>% file.path("R", .)), function(script) {
    tokens <- utils::getParseData(parse(file.path(root, script), keep.source = TRUE, encoding = "UTF-8"))
    strings <- tokens$text[tokens$token == "STR_CONST"]
    vapply(strings, function(value) eval(parse(text = value)), FUN.VALUE = character(1))
  }), use.names = FALSE)
  literals <- literals[grepl("^(data|outputs|output)/", literals)]
  # Current paths can refer to original extracts stored under the retired layer.
  migration <- readr::read_csv(file.path(root, "registry/data_layer_migration.csv"), show_col_types = FALSE)
  literals <- unique(c(literals, migration$legacy_path[migration$file_path %in% literals]))
  required <- !grepl("^(data/|outputs/|output/|tmp/)", manifest$path) |
    grepl(profile$input_pattern, manifest$path, perl = TRUE) | manifest$path %in% literals
  if (mode != "replay") manifest <- manifest[required, ]
  workspace <- file.path(run_dir, "workspace")
  repro_restore(manifest, workspace, store)
  # Capture the current implementation/configuration separately from the old data.
  code_paths <- repro_files(root)
  code_paths <- code_paths[grepl("^(R/|scripts/|registry/|config/|tests/)|^(DESCRIPTION|renv.lock)$", code_paths) &
    !grepl("^registry/reproducibility/", code_paths)]
  if (mode == "replay") code_paths <- manifest$path[manifest$path %in% code_paths]
  for (path in if (mode == "replay") character() else code_paths) {
    target <- file.path(workspace, path)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(file.path(root, path), target, overwrite = TRUE)) {
      rlang::abort(paste("Could not stage code", path), class = "repro_copy_error")
    }
  }
  if (mode != "replay") {
    source(file.path(root, "R/pipeline_helpers.R"))
    pipeline_restore_legacy_paths(workspace)
  }
  if (nzchar(ingestion)) {
    rows <- readr::read_csv(ingestion, show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character(), bytes = readr::col_double()))
    if (any(rows$source_id != source_id) || !nzchar(profile$staging_target)) {
      rlang::abort("Ingestion source/target mismatch", class = "repro_config_error")
    }
    if (profile$staging_kind == "file" && nrow(rows) != 1L) {
      rlang::abort("Single-file source requires exactly one input", class = "repro_config_error")
    }
    for (i in seq_len(nrow(rows))) {
      destination <- if (profile$staging_kind == "file") profile$staging_target else
        file.path(profile$staging_target, repro_relative(rows$relative_path[i]))
      object <- file.path(store, "objects", substr(rows$sha256[i], 1, 2), rows$sha256[i])
      if (!identical(repro_hash(object), rows$sha256[i])) {
        rlang::abort("Ingestion checksum mismatch", class = "repro_integrity_error")
      }
      target <- file.path(workspace, repro_relative(destination))
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(object, target, overwrite = TRUE)) rlang::abort("Overlay failed", class = "repro_copy_error")
    }
  }
  # Replaying restores the exact code and input snapshot of the named successful run.
  # A replay cannot accidentally accept new code under an old response manifest.
  if (mode == "replay" && nzchar(parent_run)) {
    for (path in code_paths) {
      row <- manifest[manifest$path == path, ]
      if (nrow(row) != 1L) rlang::abort("Parent code snapshot is incomplete", class = "repro_input_error")
      object <- file.path(store, "objects", substr(row$sha256, 1, 2), row$sha256)
      if (!file.copy(object, file.path(workspace, path), overwrite = TRUE)) {
        rlang::abort("Could not restore parent code", class = "repro_copy_error")
      }
    }
  }
  input_manifest <- repro_inventory(workspace, repro_files(workspace))
  readr::write_csv(input_manifest, file.path(run_dir, "inputs.csv"))
  # The configuration records every argument, environment override and lock hash.
  config <- list(root = root, store = store, run_dir = run_dir, workspace = workspace,
    mode = mode, source_id = source_id, parent_run = parent_run, ingestion = ingestion,
    scripts = strsplit(profile$scripts, ";", fixed = TRUE)[[1]],
    script_arguments = if ("arguments" %in% names(profile)) profile$arguments[[1]] else "",
    environment = strsplit(profile$environment, ";", fixed = TRUE)[[1]],
    lock_sha256 = repro_hash(file.path(workspace, "renv.lock")),
    library_paths = .libPaths(),
    clock = as.double(base::Sys.time()),
    r_version = as.character(getRversion()))
  # Retain all source-related overrides inherited by the worker, including empty ones.
  environment <- Sys.getenv()
  config$inherited_environment <- environment[grepl("^(BCV_|OPEC_|IMF_|CROSS_COUNTRY_|START_YEAR$|END_YEAR$|REPORT_CONFIG$)", names(environment))]
  if (mode == "replay") {
    parent_config <- readRDS(file.path(store, "runs", parent_run, "config.rds"))
    config$clock <- parent_config$clock
    config$scripts <- parent_config$scripts
    config$environment <- parent_config$environment
    config$script_arguments <- parent_config$script_arguments
    config$inherited_environment <- parent_config$inherited_environment
  }
  config$inherited_environment <- as.list(unclass(config$inherited_environment))
  config_path <- file.path(run_dir, "config.rds")
  saveRDS(config, config_path, version = 3)
  jsonlite::write_json(config, file.path(run_dir, "config.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    args = c("--no-init-file", shQuote(file.path(root, "scripts/reproducibility_worker.R")), shQuote(config_path)),
    stdout = file.path(run_dir, "stdout.log"), stderr = file.path(run_dir, "stderr.log"))
  output <- repro_inventory(workspace, repro_files(workspace))
  purrr::walk2(file.path(workspace, output$path), output$sha256, repro_store_known, store = store)
  repro_write_once(output, file.path(run_dir, "files.csv"))
  changes <- dplyr::full_join(input_manifest %>% dplyr::select(path, input_sha256 = sha256),
    output %>% dplyr::select(path, output_sha256 = sha256), by = "path") %>%
    dplyr::filter(is.na(.data$input_sha256) | is.na(.data$output_sha256) | .data$input_sha256 != .data$output_sha256)
  readr::write_csv(changes, file.path(run_dir, "changes.csv"))
  jsonlite::write_json(list(status = if (status == 0L) "success" else "failed", exit_code = status,
    source_id = source_id, mode = mode, parent_run = parent_run, changed_files = nrow(changes)),
    file.path(run_dir, "result.json"), pretty = TRUE, auto_unbox = TRUE)
  if (status != 0L) rlang::abort(paste("Importer failed; see", run_dir), class = "repro_import_error")
  message("Successful isolated run: ", run_dir)
  invisible(output)
  }, error = function(error) {
    writeLines(conditionMessage(error), file.path(run_dir, "controller_error.log"))
    jsonlite::write_json(list(status = "failed", source_id = source_id, mode = mode,
      error_class = class(error), message = conditionMessage(error)),
      file.path(run_dir, "result.json"), auto_unbox = TRUE, pretty = TRUE)
  })
}
