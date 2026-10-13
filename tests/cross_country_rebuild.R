# Run after a successful extract to verify the complete runner's cache-only rebuild.
source("R/source_helpers.R")
output_dir <- "data/final/cross_country_macro"
raw_dir <- "data/raw/cross_country_macro"
outputs <- list.files(output_dir, pattern = "\\.csv$", full.names = TRUE)
caches <- list.files(raw_dir, pattern = "\\.(json|xml)$", full.names = TRUE)
if (!length(outputs) || !length(caches)) {
  rlang::abort("Build the cross-country extracts before testing cache reuse.", class = "macro_cache_error")
}
before <- tools::md5sum(outputs)
cache_times <- file.info(caches)$mtime

# Force every provider offline, restoring the caller's environment after the check.
flag_names <- c("CROSS_COUNTRY_REFRESH", "CROSS_COUNTRY_REFRESH_WDI",
                "CROSS_COUNTRY_REFRESH_DATA360", "CROSS_COUNTRY_REFRESH_IMF")
original_flags <- Sys.getenv(flag_names, unset = NA_character_)
do.call(Sys.setenv, setNames(as.list(rep("false", length(flag_names))), flag_names))
run_script_isolated("scripts/run_cross_country.R")
Sys.unsetenv(flag_names[is.na(original_flags)])
if (any(!is.na(original_flags))) {
  do.call(Sys.setenv, as.list(original_flags[!is.na(original_flags)]))
}
stopifnot(identical(before, tools::md5sum(outputs)),
          identical(cache_times, file.info(caches)$mtime))
message("Cross-country cached runner rebuild is byte-identical; raw API caches were reused.")
