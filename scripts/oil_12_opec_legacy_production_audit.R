# Audit the legacy all-country OPEC production parser on a cached 2001 report.

library(tidyverse)
library(pdftools)

source("R/source_helpers.R")
source("R/opec_helpers.R")
source("R/opec_historical_helpers.R")

production_data <- extract_opec_legacy_production(
  pdf_path = "data/raw/opec/2001-01/momr.pdf",
  report_date = as.Date("2000-12-01"),
  source_type = "secondary",
  y_tolerance = 4,
  country_max_x = 250,
  value_match_tolerance = 25
)

audit_opec_production(
  production_data,
  expected_observations = 1L,
  require_total_opec = TRUE
)
write_processed_dataset(production_data, "oil_12_opec_legacy_production_audit")
