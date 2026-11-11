# Regression: separate-page tables must not reuse the secondary row as direct data.
library(tidyverse)
source("R/opec_helpers.R")

secondary_table <- paste(
  "Table 5 - 7: DoC crude oil production based on secondary sources, tb/d",
  "Venezuela 760 868 892 908 926 933 928 896 -32", sep = "\n"
)
direct_table <- paste(
  "Table 5 - 8: OPEC crude oil production based on direct communication, tb/d",
  "Venezuela 783 921 933 982 1035 1048 1051 1066 15", sep = "\n"
)
both_tables <- paste(secondary_table, direct_table, sep = "\n\f\n")
secondary <- extract_opec_venezuela_line(both_tables, "secondary_sources", as.Date("2025-05-01"))
direct <- extract_opec_venezuela_line(both_tables, "direct_communication", as.Date("2025-05-01"))
stopifnot(secondary$value[secondary$period == "m_current"] == 896,
          direct$value[direct$period == "m_current"] == 1066)

# A missing direct table must raise an error instead of returning secondary values.
missing_direct <- tryCatch(
  extract_opec_venezuela_line(secondary_table, "direct_communication", as.Date("2025-05-01")),
  error = identity
)
stopifnot(inherits(missing_direct, "error"))
message("OPEC production source regression checks passed.")
