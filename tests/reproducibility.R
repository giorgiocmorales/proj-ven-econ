# Offline regression tests for preservation, explicit provenance and response replay.
library(dplyr)
source("R/reproducibility.R")
source("R/source_helpers.R")

# Keep fixture bytes, corruptions and manifests entirely outside the source tree.
fixture <- tempfile("repro-test-")
dir.create(fixture)
store <- file.path(fixture, "store")
raw <- file.path(fixture, "input.csv")
readr::write_csv(tibble::tibble(date = "2026-01-01", value = 1), raw)
original <- repro_hash(raw)

# Check typed failures instead of accepting any exception as a passing test.
expect_repro_error <- function(expression, class) {
  error <- tryCatch({ force(expression); NULL }, error = identity)
  stopifnot(inherits(error, class))
}

# Deduplication is deterministic, and a corrupt existing object is never accepted.
hash <- repro_store(raw, store)
stopifnot(identical(hash, original), identical(repro_store(raw, store), hash))
object <- file.path(store, "objects", substr(hash, 1, 2), hash)
writeLines("corrupt", object)
expect_repro_error(repro_store(raw, store), "repro_integrity_error")
stopifnot(file.copy(raw, object, overwrite = TRUE))

# Parallel writers must publish a complete shared object exactly once.
check_parallel_store <- function(path, store) {
  # Pass pinned library paths below without loading ambient worker startup profiles.
  cluster <- parallel::makePSOCKcluster(2L, rscript_args = "--vanilla")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterCall(cluster, function(paths) { .libPaths(paths); NULL }, .libPaths())
  parallel::clusterExport(cluster, c("repro_hash", "repro_store", "repro_store_known"), envir = globalenv())
  values <- parallel::parLapply(cluster, rep(path, 4L), function(path, store) {
    repro_store(path, store)
  }, store = store)
  stopifnot(length(unique(unlist(values))) == 1L,
    identical(values[[1]], repro_hash(path)))
}
parallel_input <- file.path(fixture, "parallel.bin")
writeBin(rep(as.raw(1:200), 25000L), parallel_input)
check_parallel_store(parallel_input, store)

# A manual revision creates a new manifest and never rewrites the original bytes.
sources <- tibble::tibble(source_id = "manual")
first <- repro_ingest("manual", raw, "User supplied; author unknown", "v1", store, sources)
stopifnot(identical(first, repro_ingest("manual", raw, "User supplied; author unknown", "v1", store, sources)))
readr::write_csv(tibble::tibble(date = "2026-02-01", value = 2), raw)
second <- repro_ingest("manual", raw, "User supplied; author unknown", "v2", store, sources)
stopifnot(first != second, identical(repro_hash(object), original), file.exists(first))
expect_repro_error(repro_ingest("manual", raw, "", "v3", store, sources), "repro_config_error")
expect_repro_error(repro_relative("../baseline.csv"), "repro_path_error")
expect_repro_error(repro_relative("D:/baseline.csv"), "repro_path_error")

# Replay returns captured bytes and errors on an unknown URL without a network call.
http <- file.path(fixture, "http")
dir.create(http)
url <- "https://example.invalid/captured-only"
key <- digest::digest(url, algo = "sha256", serialize = FALSE)
jsonlite::write_json(list(url = url, status = 200L, sha256 = original),
  file.path(http, paste0(key, ".json")), auto_unbox = TRUE)
fetch <- repro_transport("replay", file.path(fixture, "replayed"), http, store)
destination <- file.path(fixture, "result.csv")
fetch(url, destination, 1L, 1L)
stopifnot(identical(repro_hash(destination), original))
expect_repro_error(fetch("https://example.invalid/missing", destination, 1L, 1L), "repro_replay_missing")

# Immutable manifests reject changed rows and snapshot restoration rejects reuse.
manifest <- tibble::tibble(path = "data/raw/input.csv", sha256 = original)
receipt <- file.path(fixture, "manifest.csv")
repro_write_once(manifest, receipt)
expect_repro_error(repro_write_once(tibble::tibble(path = "changed"), receipt), "repro_immutable_error")
workspace <- file.path(fixture, "workspace")
repro_restore(manifest, workspace, store)
stopifnot(identical(repro_hash(file.path(workspace, manifest$path)), original))
expect_repro_error(repro_restore(manifest, workspace, store), "repro_immutable_error")

# Incomplete API results fail before a data writer can replace the old extract.
expect_repro_error(write_api_source_result(list(data = tibble::tibble(value = 1),
  errors = tibble::tibble(error_message = "fixture failure")), "fixture"), "source_api_incomplete_error")
expect_repro_error(write_api_source_result(list(data = tibble::tibble(),
  errors = tibble::tibble()), "fixture"), "source_api_incomplete_error")

# Every registered importer exists and the project's direct dependencies are pinned.
profiles <- readr::read_csv("registry/reproducibility_sources.csv", na = character(),
  show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
scripts <- unlist(strsplit(profiles$scripts[nzchar(profiles$scripts)], ";", fixed = TRUE))
stopifnot(!anyDuplicated(profiles$source_id), all(file.exists(scripts)),
  all(repro_dependencies(getwd())$matches))
expect_repro_error(repro_run(getwd(), store, profiles, "imf_annual_archive", "blocked-bulk",
  "update", "", ""), "repro_source_access_error")
for (script in unique(scripts)) parse(script, encoding = "UTF-8")
# Confirm the resolved fixture remains directly inside R's temporary directory.
stopifnot(identical(dirname(normalizePath(fixture, winslash = "/", mustWork = TRUE)),
  normalizePath(tempdir(), winslash = "/", mustWork = TRUE)))
unlink(fixture, recursive = TRUE)
message("All reproducibility regression checks passed")
