# Dataset-oriented dependency planning, isolated builds and recoverable publication.

# Split the explicit semicolon-delimited fields in the procedure inventory.
pipeline_fields <- function(value) {
  if (is.na(value) || !nzchar(value)) return(character())
  strsplit(value, ";", fixed = TRUE)[[1]]
}

# Resolve dataset aliases and dependencies, rejecting ambiguous targets and cycles.
pipeline_plan <- function(target, procedures, datasets) {
  required <- c("step_id", "domain", "script_path", "depends_on", "input_files", "output_files", "arguments")
  if (!all(required %in% names(procedures)) || anyDuplicated(procedures$step_id)) {
    rlang::abort("Invalid pipeline procedure inventory", class = "pipeline_registry_error")
  }
  outputs <- unlist(lapply(procedures$output_files, pipeline_fields), use.names = FALSE)
  if (anyDuplicated(outputs)) rlang::abort("Multiple producers for an output", class = "pipeline_registry_error")
  if (target == "all") {
    selected <- procedures$step_id
  } else if (target %in% procedures$domain) {
    selected <- procedures$step_id[procedures$domain == target]
  } else {
    selected <- procedures$step_id[procedures$step_id == target]
    if (!length(selected)) {
      selected <- datasets$producer_step[datasets$dataset_id == target & !is.na(datasets$producer_step)]
    }
    if (!length(selected)) {
      selected <- procedures$step_id[vapply(procedures$output_files, function(value) {
        target %in% tools::file_path_sans_ext(basename(pipeline_fields(value)))
      }, logical(1))]
    }
    selected <- unique(selected[nzchar(selected)])
    if (length(selected) != 1L) rlang::abort(paste("Unknown or ambiguous dataset", target), class = "pipeline_target_error")
  }
  ordered <- character()
  visiting <- character()
  visit <- function(step) {
    if (step %in% visiting) rlang::abort(paste("Dependency cycle at", step), class = "pipeline_dependency_error")
    if (step %in% ordered) return(invisible(NULL))
    row <- procedures[procedures$step_id == step, , drop = FALSE]
    if (nrow(row) != 1L || !file.exists(row$script_path)) {
      rlang::abort(paste("Missing procedure", step), class = "pipeline_dependency_error")
    }
    visiting <<- c(visiting, step)
    for (dependency in pipeline_fields(row$depends_on)) visit(dependency)
    visiting <<- setdiff(visiting, step)
    ordered <<- c(ordered, step)
    invisible(NULL)
  }
  for (step in selected) visit(step)
  procedures[match(ordered, procedures$step_id), , drop = FALSE]
}

# Inventory exact bytes; raw inputs and published versions remain inspectable.
pipeline_inventory <- function(root, directories) {
  paths <- unique(unlist(lapply(directories, function(directory) {
    if (file.exists(file.path(root, directory)) && !dir.exists(file.path(root, directory))) {
      return(file.path(root, directory))
    }
    list.files(file.path(root, directory), recursive = TRUE, full.names = TRUE, all.files = TRUE,
      no.. = TRUE)
  }), use.names = FALSE))
  paths <- paths[file.exists(paths) & !dir.exists(paths)]
  tibble::tibble(path = substring(gsub("\\\\", "/", paths), nchar(gsub("\\\\", "/", root)) + 2L),
    bytes = file.info(paths)$size,
    sha256 = vapply(paths, digest::digest, character(1), algo = "sha256", file = TRUE))
}

# Copy independent files, including source timestamps, without links to mutable originals.
pipeline_copy <- function(root, destination, paths) {
  for (path in unique(paths)) {
    source <- file.path(root, path)
    if (!file.exists(source)) rlang::abort(paste("Missing staged input", path), class = "pipeline_input_error")
    target <- file.path(destination, path)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source, target, overwrite = FALSE, copy.date = TRUE)) {
      rlang::abort(paste("Cannot stage", path), class = "pipeline_copy_error")
    }
  }
  invisible(NULL)
}

# Bridge preserved historical workspaces to current paths without changing old manifests.
pipeline_restore_legacy_paths <- function(root) {
  mapping_path <- file.path(root, "registry/data_layer_migration.csv")
  mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)
  for (index in seq_len(nrow(mapping))) {
    original <- file.path(root, mapping$legacy_path[[index]])
    destination <- file.path(root, mapping$file_path[[index]])
    if (!file.exists(destination) && file.exists(original)) {
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(original, destination, copy.date = TRUE)) {
        rlang::abort(paste("Cannot migrate historical input", original), class = "pipeline_copy_error")
      }
    }
  }
  invisible(NULL)
}

# Validate every declared publication and all matching contracts before any promotion.
pipeline_validate <- function(plan, reference_date, mode, previous_root) {
  outputs <- unique(unlist(lapply(plan$output_files, pipeline_fields), use.names = FALSE))
  contracts <- read_data_contracts("registry/data_contracts.csv") %>%
    dplyr::filter(.data$file_path %in% outputs)
  checks <- vector("list", length(outputs))
  for (index in seq_along(outputs)) {
    path <- outputs[[index]]
    if (!file.exists(path) || file.info(path)$size == 0) {
      rlang::abort(paste("Missing publication", path), class = "pipeline_output_error")
    }
    data <- readr::read_csv(path, show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()))
    readr::stop_for_problems(data)
    if (!nrow(data) || anyDuplicated(names(data)) ||
        any(vapply(data, function(column) any(column %in% c("Inf", "-Inf", "NaN")), logical(1)))) {
      rlang::abort(paste("Empty or invalid publication", path), class = "pipeline_output_error")
    }
    checks[[index]] <- new_validation_result(tools::file_path_sans_ext(basename(path)),
      "publication_schema", "pass", paste(nrow(data), "parsed rows in", path))
    # A narrower request or incomplete source must not silently discard published history.
    contract <- contracts[contracts$file_path == path, , drop = FALSE]
    previous_path <- file.path(previous_root, path)
    if (nrow(contract) == 1L && file.exists(previous_path)) {
      keys <- split_contract_fields(contract$key_columns[[1]])
      previous <- readr::read_csv(previous_path, show_col_types = FALSE,
        col_types = readr::cols(.default = readr::col_character()))
      if (all(keys %in% names(previous)) && all(keys %in% names(data)) &&
          nrow(dplyr::anti_join(previous, data, by = keys))) {
        rlang::abort(paste("Publication would remove existing observation keys:", path),
          class = "pipeline_coverage_error")
      }
    }
  }
  missing_contracts <- setdiff(outputs[startsWith(outputs, "data/final/")], contracts$file_path)
  if (length(missing_contracts)) {
    rlang::abort(paste("Final publications lack contracts:", paste(missing_contracts, collapse = ", ")),
      class = "pipeline_validation_error")
  }
  results <- dplyr::bind_rows(dplyr::bind_rows(checks), validate_data_contracts(contracts, reference_date))
  # Cached reconstructions must expose staleness without claiming to refresh provider coverage.
  failures <- results %>% dplyr::filter(.data$status == "fail", .data$check != "freshness")
  if (nrow(failures)) {
    rlang::abort(paste(failures$dataset_id, failures$check, failures$details, collapse = "\n"),
      class = "pipeline_validation_error")
  }
  if (any(results$check == "freshness" & results$status == "fail")) {
    message("Some inputs are stale; see validation.csv. Observation coverage is not imputed or refreshed by a rebuild.")
  }
  results
}

# Build selected dependencies in a private workspace, then publish as a recoverable group.
pipeline_run <- function(target, mode, parent_run) {
  if (!mode %in% c("rebuild", "resume", "update")) {
    rlang::abort("Specify rebuild, resume or update", class = "pipeline_mode_error")
  }
  if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]] &&
      !nzchar(Sys.setlocale("LC_CTYPE", ".UTF-8"))) {
    rlang::abort("UTF-8 is required for source labels", class = "pipeline_locale_error")
  }
  root <- normalizePath(".", winslash = "/", mustWork = TRUE)
  parent_workspace <- NULL
  if (nzchar(parent_run)) {
    if (!grepl("^[A-Za-z0-9_-]+$", parent_run)) {
      rlang::abort("Expected a failed run ID without path separators", class = "pipeline_parent_error")
    }
    parent_directory <- file.path(root, "data/pipeline/runs", parent_run)
    if (!file.exists(file.path(parent_directory, "result.json"))) {
      rlang::abort("Parent run has no completed result receipt", class = "pipeline_parent_error")
    }
    parent <- jsonlite::read_json(file.path(parent_directory, "result.json"))
    if (!identical(parent$status, "failed") || !identical(parent$target, target)) {
      rlang::abort("Continuation requires a failed run for the same target", class = "pipeline_parent_error")
    }
    parent_workspace <- file.path(parent_directory, "workspace")
  }
  for (setting in c("START_YEAR", "END_YEAR")) {
    value <- Sys.getenv(setting, "")
    if (nzchar(value) && is.na(suppressWarnings(as.integer(value)))) {
      rlang::abort(paste(setting, "must be an integer year"), class = "pipeline_configuration_error")
    }
  }
  procedures <- readr::read_csv("registry/pipeline_steps.csv", show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()), na = character())
  datasets <- readr::read_csv("registry/series_registry.csv", show_col_types = FALSE)
  plan <- pipeline_plan(target, procedures, datasets)
  if ("source_profiles" %in% names(plan)) {
    profiles <- readr::read_csv("registry/reproducibility_sources.csv", show_col_types = FALSE)
    referenced <- unique(unlist(lapply(plan$source_profiles, pipeline_fields), use.names = FALSE))
    if (length(setdiff(referenced, profiles$source_id))) {
      rlang::abort("A procedure references an unregistered source profile", class = "pipeline_registry_error")
    }
  }
  run_dir <- tempfile(pattern = paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "_"),
    tmpdir = file.path(root, "data/pipeline/runs"))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  workspace <- file.path(run_dir, "workspace")
  dir.create(workspace)
  jsonlite::write_json(list(target = target, mode = mode, parent_run = parent_run, steps = plan$step_id,
    scripts = stats::setNames(vapply(plan$script_path, digest::digest, character(1),
      algo = "sha256", file = TRUE), plan$step_id), r_version = as.character(getRversion()),
    source_settings = as.list(Sys.getenv(c("START_YEAR", "END_YEAR", "OPEC_START_DATE", "OPEC_END_DATE",
      "OPEC_REFRESH_PDFS", "BCV_FX_REFRESH_HISTORY", "REPORT_CONFIG"), unset = "")),
    offline_flag_policy = "Explicit runner mode overrides legacy per-script offline flags"),
    file.path(run_dir, "config.json"), pretty = TRUE, auto_unbox = TRUE)
  # Dependencies declare source locations; retained final/interim tables provide historical snapshots.
  patterns <- unique(unlist(lapply(plan$input_files, pipeline_fields), use.names = FALSE))
  source_paths <- unique(unlist(lapply(patterns, Sys.glob), use.names = FALSE))
  directory_paths <- source_paths[dir.exists(source_paths)]
  source_paths <- unique(c(source_paths[!dir.exists(source_paths)], unlist(lapply(directory_paths,
    list.files, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE), use.names = FALSE)))
  code <- pipeline_inventory(root, c("R", "scripts", "config", "registry"))$path
  retained <- pipeline_inventory(root, c("data/final", "data/interim", "data/manual_fix"))$path
  pipeline_copy(root, workspace, unique(c(code, retained, source_paths[!dir.exists(source_paths)], "renv.lock", "DESCRIPTION")))
  declared <- unique(unlist(lapply(plan$output_files, pipeline_fields), use.names = FALSE))
  registry_outputs <- declared[startsWith(declared, "registry/")]
  publication_baseline <- pipeline_inventory(workspace, c("data/final", "data/interim", "data/raw", registry_outputs))
  if (!is.null(parent_workspace)) {
    # Resume acquisition from the failed workspace only while its original inputs remain current.
    inputs <- readr::read_csv(file.path(parent_directory, "inputs.csv"), show_col_types = FALSE)
    original <- inputs %>% dplyr::filter(file.exists(file.path(root, .data$path)))
    actual_hashes <- vapply(file.path(root, original$path), digest::digest, character(1), algo = "sha256", file = TRUE)
    if (any(actual_hashes != original$sha256)) {
      rlang::abort("Current inputs changed since the failed run; start a new build", class = "pipeline_parent_error")
    }
    acquired <- pipeline_inventory(parent_workspace, "data/raw")
    for (index in seq_len(nrow(acquired))) {
      path <- acquired$path[[index]]
      if (!path %in% inputs$path && file.exists(file.path(root, path)) &&
          digest::digest(file = file.path(root, path), algo = "sha256") != acquired$sha256[[index]]) {
        rlang::abort(paste("A newer source conflicts with failed-run cache", path), class = "pipeline_parent_error")
      }
      destination <- file.path(workspace, path)
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(file.path(parent_workspace, path), destination, overwrite = TRUE, copy.date = TRUE)) {
        rlang::abort(paste("Cannot retain failed-run acquisition", path), class = "pipeline_copy_error")
      }
    }
  }
  before <- pipeline_inventory(workspace, c("data/final", "data/interim", "data/raw", registry_outputs))
  readr::write_csv(before, file.path(run_dir, "inputs.csv"))
  readr::write_csv(pipeline_inventory(workspace, c("R", "scripts", "config", "registry", "renv.lock", "DESCRIPTION")),
    file.path(run_dir, "code.csv"))
  previous_mode <- getOption("pipeline.mode")
  options(pipeline.mode = mode)
  on.exit(options(pipeline.mode = previous_mode), add = TRUE)
  # The explicit runner mode owns acquisition; legacy per-script flags cannot override it.
  offline_flags <- c("BCV_CPI_GROUPS_OFFLINE", "BCV_CPI_CARACAS_OFFLINE", "BCV_MONETARY_OFFLINE", "MINIMUM_WAGE_OFFLINE")
  previous_flags <- Sys.getenv(offline_flags, unset = NA_character_)
  do.call(Sys.setenv, as.list(stats::setNames(rep("false", length(offline_flags)), offline_flags)))
  on.exit({
    Sys.unsetenv(names(previous_flags)[is.na(previous_flags)])
    if (any(!is.na(previous_flags))) do.call(Sys.setenv, as.list(previous_flags[!is.na(previous_flags)]))
  }, add = TRUE)
  on.exit(setwd(root), add = TRUE)
  # A failed script retains its workspace and original error; no current publication has changed.
  withCallingHandlers({
    setwd(workspace)
    for (index in seq_len(nrow(plan))) {
      message("Building ", plan$step_id[[index]], " [", mode, "]")
      environment <- new.env(parent = globalenv())
      arguments <- pipeline_fields(plan$arguments[[index]])
      arguments <- gsub("{mode}", mode, arguments, fixed = TRUE)
      environment$commandArgs <- local({ values <- arguments; function(trailingOnly) values })
      sys.source(plan$script_path[[index]], envir = environment, keep.source = TRUE)
    }
    validation <- pipeline_validate(plan, Sys.Date(), mode, root)
    readr::write_csv(validation, file.path(run_dir, "validation.csv"))
    after <- pipeline_inventory(workspace, c("data/final", "data/interim", "data/raw", registry_outputs))
    readr::write_csv(after, file.path(run_dir, "outputs.csv"))
    changes <- dplyr::anti_join(after, publication_baseline, by = c("path", "sha256"))
    unexpected <- setdiff(changes$path[startsWith(changes$path, "data/final/")], declared)
    if (length(unexpected)) {
      rlang::abort(paste("Producer wrote undeclared final outputs:", paste(unexpected, collapse = ", ")),
        class = "pipeline_registry_error")
    }
    if (mode == "rebuild" && !nzchar(parent_run)) {
      changes <- changes %>% dplyr::filter(!startsWith(.data$path, "data/raw/"))
    }
    # Acquire all rollback copies before replacing the first current file.
    setwd(root)
    previous <- changes$path[file.exists(changes$path)]
    pipeline_copy(root, file.path(run_dir, "previous"), previous)
    readr::write_csv(changes, file.path(run_dir, "publication.csv"))
    committed <- FALSE
    published <- character()
    on.exit({
      if (!committed) for (path in published) {
        backup <- file.path(run_dir, "previous", path)
        if (file.exists(backup)) {
          if (!file.copy(backup, file.path(root, path), overwrite = TRUE, copy.date = TRUE)) {
            warning(paste("Rollback failed; restore from", backup), call. = FALSE)
          }
        } else unlink(file.path(root, path))
      }
    }, add = TRUE)
    for (index in seq_len(nrow(changes))) {
      path <- changes$path[[index]]
      destination <- file.path(root, path)
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      published <- c(published, path)
      if (!file.copy(file.path(workspace, path), destination, overwrite = TRUE, copy.date = TRUE) ||
          digest::digest(file = destination, algo = "sha256") != changes$sha256[[index]]) {
        rlang::abort(paste("Publication failed for", path), class = "pipeline_publication_error")
      }
    }
    committed <- TRUE
    jsonlite::write_json(list(status = "success", target = target, mode = mode,
      published_files = nrow(changes)), file.path(run_dir, "result.json"), auto_unbox = TRUE, pretty = TRUE)
    message("Validated publication: ", run_dir)
    invisible(run_dir)
  }, error = function(error) {
    jsonlite::write_json(list(status = "failed", target = target, mode = mode,
      error_class = class(error), message = conditionMessage(error)),
      file.path(run_dir, "result.json"), auto_unbox = TRUE, pretty = TRUE)
  }, message = function(message) {
    cat(conditionMessage(message), file = file.path(run_dir, "messages.log"), append = TRUE)
  }, warning = function(warning) {
    cat(conditionMessage(warning), "\n", file = file.path(run_dir, "warnings.log"), append = TRUE)
  })
}
