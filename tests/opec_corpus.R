# Offline regression fixtures exercise column movement, year rollover, missing cells and source isolation.
library(tidyverse)
source("R/opec_corpus_helpers.R")

production_table <- paste(
  "Table 5: OPEC crude oil production based on secondary sources, tb/d",
  sprintf("%-20s%10s%10s%10s%10s%10s", "", "1999", "2000", "Nov.00", "Dec.00", "Change"),
  sprintf("%-20s%10s%10s%10s%10s%10s", "Algeria", "800", "810", "820", "830", "10"),
  sprintf("%-20s%10s%10s%10s%10s%10s", "Venezuela", "2800", "2900", "3000", "3043*", "43"),
  sprintf("%-20s%10s%10s%10s%10s%10s", "Total OPEC", "3600", "3710", "3820", "3873", "53"),
  "Note: synthetic test data.", sep = "\n"
)
production <- extract_opec_monthly_table(production_table, as.Date("2001-01-01"), "secondary")
stopifnot(identical(sort(unique(production$date)), as.Date(c("2000-11-01", "2000-12-01"))),
          production$value[production$label == "Venezuela" & production$date == as.Date("2000-12-01")] == 3043,
          production$footnote_marker[production$label == "Venezuela" & production$date == as.Date("2000-12-01")] == "*")

# Absent direct data cannot be inferred from a secondary-source table.
absent <- tryCatch(extract_opec_monthly_table(production_table, as.Date("2001-01-01"), "direct"), error = identity)
stopifnot(inherits(absent, "opec_table_absent"))

# Spot rows may carry negative values, footnotes, and explicit missing-value markers.
price_table <- paste(
  "Table 1: OPEC Reference Basket and selected crudes, US$/b",
  sprintf("%-20s%10s%10s%10s%10s%10s", "", "Mar 20", "Apr 20", "Change", "2019", "2020"),
  sprintf("%-20s%10s%10s%10s%10s%10s", "ORB", "30", "20", "-10", "60", "25"),
  sprintf("%-20s%10s%10s%10s%10s%10s", "WTI", "25", "-1.5", "-26.5", "50", "20"),
  sprintf("%-20s%10s%10s%10s%10s%10s", "Merey", "20", "..", "..", "na", "15a"),
  "Sources: synthetic test data.", sep = "\n"
)
prices <- extract_opec_monthly_table(price_table, as.Date("2020-05-01"), "price")
stopifnot(prices$value[prices$label == "WTI" & prices$date == as.Date("2020-04-01")] == -1.5,
          prices$value_status[prices$label == "Merey" & prices$date == as.Date("2020-04-01")] == "not_reported")

# A missing numeric token must fail, rather than shift later values to earlier months.
broken <- str_replace(price_table, "-26\\.5", " ")
failure <- tryCatch(extract_opec_monthly_table(broken, as.Date("2020-05-01"), "price"), error = identity)
stopifnot(inherits(failure, "error"))

# Missing markers must be recognized before footnote letters are removed.
for (marker in c("n/a", "n.a.", "NA", "na", "-", "\u2026")) {
  marked <- str_replace(price_table, fixed(".."), marker)
  parsed <- extract_opec_monthly_table(marked, as.Date("2020-05-01"), "price")
  stopifnot(is.na(parsed$value[parsed$label == "Merey" & parsed$date == as.Date("2020-04-01")]))
}

# A newly changed unit must not be silently interpreted as thousands of barrels.
wrong_unit <- str_replace(production_table, fixed("tb/d"), "mb/d")
failure <- tryCatch(extract_opec_monthly_table(wrong_unit, as.Date("2001-01-01"), "secondary"), error = identity)
stopifnot(inherits(failure, "opec_unit_error"))

# Two source tables on one page must retain their separate Venezuela rows.
direct_table <- production_table %>%
  str_replace_all(fixed("secondary sources"), "direct communication") %>%
  str_replace(fixed("3043*"), "3100**")
both <- paste(production_table, direct_table, sep = "\n")
direct <- extract_opec_monthly_table(both, as.Date("2001-01-01"), "direct")
secondary <- extract_opec_monthly_table(both, as.Date("2001-01-01"), "secondary")
stopifnot(direct$value[direct$label == "Venezuela" & direct$date == as.Date("2000-12-01")] == 3100,
          secondary$value[secondary$label == "Venezuela" & secondary$date == as.Date("2000-12-01")] == 3043,
          direct$footnote_marker[direct$label == "Venezuela" & direct$date == as.Date("2000-12-01")] == "**",
          all(str_detect(direct$source_table_notes, fixed("synthetic test data"))))
message("OPEC corpus regression checks passed.")
