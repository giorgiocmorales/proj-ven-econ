# Restore exact locked dependencies into the project library on a fresh machine.
# renv's tracked bootstrap installs its own runtime locally when it is unavailable.
if (!file.exists("renv.lock")) stop("Missing project lockfile: renv.lock")
source("renv/activate.R")
renv::restore(project = getwd(), lockfile = "renv.lock", prompt = FALSE)
