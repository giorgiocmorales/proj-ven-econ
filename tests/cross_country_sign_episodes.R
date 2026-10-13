# Verify reversals, zero periods, missing observations, gaps, and source isolation.
library(tidyverse)
source("R/cross_country_episode_helpers.R")
observations <- tibble(source = "WB_WDI", country_code = "VEN", indicator_id = "test",
                       year = c(2000:2007, 2009:2010),
                       value = c(2, 3, -1, -2, 0, 0, NA, 1, 2, -3))
paths <- build_sign_episode_paths(bind_rows(observations,
                                          observations %>% mutate(source = "IMF_WEO")))
wb <- paths %>% filter(source == "WB_WDI")
stopifnot(identical(wb$episode_number, c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L, 6L, 7L)),
          is.na(wb$episode_id[7]),
          identical(wb$episode_year_number, c(1L, 2L, 1L, 2L, 1L, 2L, NA_integer_, 1L, 1L, 1L)),
          wb$boundary_reason[8] == "after_missing", wb$boundary_reason[9] == "calendar_gap",
          n_distinct(na.omit(paths$episode_id)) == 12L,
          identical(wb$value, observations$value))

# Duplicate series-year keys must fail explicitly rather than creating extra years.
duplicate_failed <- tryCatch({ build_sign_episode_paths(bind_rows(observations, observations)); FALSE },
                            macro_duplicate_error = function(e) TRUE)
stopifnot(duplicate_failed)
message("Sign episode continuity and source-isolation checks passed.")
