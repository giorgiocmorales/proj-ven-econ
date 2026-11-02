# Archive and extract the full BCV intervention history through monthly official exports.
library(tidyverse)
source("R/source_helpers.R")
source("R/bcv_fx_helpers.R")
ensure_project_dirs()

import_bcv_fx(
  source_url = "https://www.bcv.org.ve/politica-cambiaria/intervencion-cambiaria",
  view_selector = ".view-intervenciones-cambiarias",
  kind = "intervention",
  raw_dir = "data/raw/bcv_fx_intervention",
  processed_path = "data/final/external_03_fx_int_bcv.csv"
)
