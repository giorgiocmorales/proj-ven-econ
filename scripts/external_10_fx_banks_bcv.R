# Archive and extract bank-level USD buy/sell exchange rates and other institutions.
library(tidyverse)
source("R/source_helpers.R")
source("R/bcv_fx_helpers.R")
ensure_project_dirs()

import_bcv_fx(
  source_url = "https://www.bcv.org.ve/tasas-informativas-sistema-bancario",
  view_selector = ".view-tasas-sistema-bancario-full",
  kind = "banks",
  raw_dir = "data/raw/bcv_fx_banks",
  processed_path = "data/final/external_10_fx_banks_bcv.csv"
)
