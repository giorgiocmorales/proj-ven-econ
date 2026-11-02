# Verify cross-rate arithmetic, historical denomination, and exclusion of ambiguous dates.
library(tidyverse)
source("R/fx_intervention_helpers.R")

# Operation dates deliberately differ: conversion must use the published value date.
smc <- tibble(
  fecha_valor = rep(as.Date(c("2021-07-19", "2021-09-30", "2026-10-16")), each = 2L),
  fecha_operacion = fecha_valor - 1,
  currency = rep(c("USD", "EUR"), 3L),
  ask = c(3000000, 3600000, 800000, 1000000, 880.1159, 985.729808))
interventions <- tibble(
  date = as.Date(c("2019-05-13", "2021-07-19", "2021-07-19", "2021-09-30", "2026-10-16")),
  intervention_id = c("014-19", "029-21", "029-21", "039-21", "032-26"),
  rate = c(5849.72, 4190665.15, 4055688.42, 1000000, 985.73), currency = "EUR")
result <- convert_bcv_intervention(smc, interventions)
stopifnot(nrow(result$observations) == 5L, nrow(result$daily) == 2L,
  sum(result$observations$conversion_status == "source_conflict") == 2L,
  sum(result$observations$conversion_status == "missing_cross_rate") == 1L,
  result$daily$rate_int[[1]] == 800000,
  result$daily$rate_int_adj[[1]] == 0.8,
  abs(result$daily$rate_int[[2]] - 985.73 * 880.1159 / 985.729808) < 1e-10,
  result$daily$rate_int_adj[[2]] == result$daily$rate_int[[2]])

# A missing reference currency on a date must remain missing, without carrying rates forward.
incomplete <- convert_bcv_intervention(smc %>% filter(!(fecha_valor == as.Date("2026-10-16") & currency == "EUR")), interventions)
stopifnot(nrow(incomplete$daily) == 1L,
  incomplete$observations$conversion_status[[5]] == "missing_cross_rate")

# Conflicting reference quotes must fail instead of multiplying or arbitrarily selecting rows.
invalid <- tryCatch(convert_bcv_intervention(bind_rows(smc, smc[1, ] %>% mutate(ask = 1)), interventions),
  fx_cross_rate_error = identity)
stopifnot(inherits(invalid, "fx_cross_rate_error"))
message("Intervention USD conversion, value-date matching, gaps, and conflict checks passed.")
