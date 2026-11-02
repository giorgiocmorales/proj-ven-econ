# Baseline exchange-rate forecasts, separate from source data and report estimates.
# Run from the repository root: Rscript scripts/external_100_fx_arima.R
# Models use current bolivars per USD from the existing consolidated local vintage.
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)
library(ggplot2)

# Keep all modelling choices explicit; no source refresh or report cutoff is used.
input_path <- "data/final/external_fx.csv"
output_dir <- "outputs/forecasts/fx_arima"
training_start <- as.Date("2024-01-01")
forecast_end <- as.Date("2026-12-31")
analysis_date <- as.Date("2026-10-21")
interval_levels <- c(80, 95)
backtest_origins <- 3L
series_labels <- c(official = "Official (BCV)", unofficial = "Unofficial (Yadio)")
model_ids <- c("auto_arima", "random_walk_drift")

# Validate the input before selecting series or removing unreported endpoints.
fx <- read_csv(input_path, show_col_types = FALSE)
if (!all(c("date", "rate_smc_adj", "rate_yad") %in% names(fx)) ||
    !inherits(fx$date, "Date") || anyNA(fx$date) || anyDuplicated(fx$date)) {
  rlang::abort("FX input requires the expected columns and unique valid dates",
    class = "fx_arima_schema_error")
}
fx <- fx %>% select(date, official = rate_smc_adj, unofficial = rate_yad) %>%
  pivot_longer(-date, names_to = "series", values_to = "rate") %>% arrange(series, date)
if (any(!is.na(fx$rate) & (!is.finite(fx$rate) | fx$rate <= 0)) ||
    any(fx$date > analysis_date)) {
  rlang::abort("FX input contains invalid rates or dates after the analysis date",
    class = "fx_arima_data_error")
}

# Remove deterministic weekend repetitions from the consolidated daily calendar.
# Weekday holidays and other upstream carried-forward values remain unchanged.
# The model clock is Monday-Friday, not an exchange-specific trading calendar.
training <- fx %>% filter(date >= training_start, wday(date, week_start = 1) <= 5L) %>%
  group_by(series) %>% filter(between(date, min(date[!is.na(rate)]), max(date[!is.na(rate)]))) %>%
  ungroup()
for (series_id in names(series_labels)) {
  observed <- training %>% filter(series == series_id)
  expected_dates <- tibble(date = seq.Date(min(observed$date), max(observed$date), by = "day")) %>%
    filter(wday(date, week_start = 1) <= 5L)
  if (nrow(observed) < 300L || anyNA(observed$rate) ||
      !identical(observed$date, expected_dates$date) || max(observed$date) >= forecast_end) {
    rlang::abort(paste("Insufficient, gapped, or out-of-horizon weekday history:", series_id),
      class = "fx_arima_coverage_error")
  }
}

# Use the package's exact-likelihood, nonseasonal AICc search on log exchange rates.
# KPSS chooses d (up to two); p and q are bounded at three, total order at five.
# The comparison model is ARIMA(0,1,0) with drift, also on logs.
fit_model <- function(values, model_id) {
  series_ts <- ts(values, frequency = 1)
  if (model_id == "auto_arima") {
    fitted <- forecast::auto.arima(series_ts, d = NA, D = 0, max.p = 3L, max.q = 3L,
      max.P = 0L, max.Q = 0L, max.order = 5L, max.d = 2L, max.D = 0L,
      start.p = 0L, start.q = 0L, stationary = FALSE, seasonal = FALSE,
      ic = "aicc", stepwise = FALSE, approximation = FALSE, method = "ML",
      test = "kpss", allowdrift = TRUE, allowmean = TRUE, lambda = 0, biasadj = TRUE,
      parallel = FALSE, trace = FALSE)
  } else if (model_id == "random_walk_drift") {
    fitted <- forecast::Arima(series_ts, order = c(0L, 1L, 0L),
      seasonal = c(0L, 0L, 0L), include.mean = FALSE, include.drift = TRUE,
      lambda = 0, biasadj = TRUE, method = "ML")
  } else {
    rlang::abort(paste("Unknown model:", model_id), class = "fx_arima_model_error")
  }
  if (fitted$code != 0L || any(!is.finite(coef(fitted)))) {
    rlang::abort(paste("Model did not converge:", model_id), class = "fx_arima_fit_error")
  }
  fitted
}

# Turn forecast objects into auditable rows. Bias-adjusted means and all bounds
# are back-transformed by forecast; medians are retained separately for clarity.
forecast_rows <- function(model, dates) {
  prediction <- forecast::forecast(model, h = length(dates), level = interval_levels,
    fan = FALSE, bootstrap = FALSE, biasadj = TRUE)
  median_prediction <- forecast::forecast(model, h = length(dates), level = interval_levels,
    fan = FALSE, bootstrap = FALSE, biasadj = FALSE)
  tibble(date = dates, horizon_weekdays = seq_along(dates),
    point_mean = as.numeric(prediction$mean), point_median = as.numeric(median_prediction$mean),
    lower_80 = as.numeric(prediction$lower[, "80%"]),
    upper_80 = as.numeric(prediction$upper[, "80%"]),
    lower_95 = as.numeric(prediction$lower[, "95%"]),
    upper_95 = as.numeric(prediction$upper[, "95%"]))
}

# Evaluate three non-overlapping recent holdouts, each as long as the final horizon.
# Every origin refits and repeats model selection using only its training history.
backtest <- list()
forecasts <- list()
diagnostics <- list()
models <- list()
for (series_id in names(series_labels)) {
  observed <- training %>% filter(series == series_id)
  future_dates <- tibble(date = seq.Date(max(observed$date) + 1, forecast_end, by = "day")) %>%
    filter(wday(date, week_start = 1) <= 5L) %>% pull(date)
  horizon <- length(future_dates)
  origins <- nrow(observed) - rev(seq_len(backtest_origins)) * horizon
  if (min(origins) < 250L) {
    rlang::abort("Backtest requires at least 250 initial weekday observations",
      class = "fx_arima_backtest_error")
  }
  for (model_id in model_ids) {
    key <- paste(series_id, model_id, sep = "_")
    for (origin in origins) {
      fitted <- fit_model(observed$rate[seq_len(origin)], model_id)
      holdout <- observed %>% slice((origin + 1L):(origin + horizon))
      backtest[[paste(key, origin, sep = "_")]] <- forecast_rows(fitted, holdout$date) %>%
        mutate(series = series_id, model = model_id, origin = observed$date[origin],
          actual = holdout$rate, .before = 1)
    }
    fitted <- fit_model(observed$rate, model_id)
    forecasts[[key]] <- forecast_rows(fitted, future_dates) %>%
      mutate(series = series_id, model = model_id, origin = max(observed$date),
        last_observed = last(observed$rate), .before = 1)
    models[[key]] <- fitted
    # Check serial dependence in log-scale residuals; rejection remains visible.
    order <- forecast::arimaorder(fitted)
    residuals_log <- as.numeric(residuals(fitted))
    fit_df <- sum(order[c("p", "q")])
    lb_test <- Box.test(residuals_log, lag = 20L, type = "Ljung-Box", fitdf = fit_df)
    diagnostics[[key]] <- tibble(series = series_id, model = model_id,
      specification = paste0("ARIMA(", paste(order[c("p", "d", "q")], collapse = ","), ")",
        if ("drift" %in% names(coef(fitted))) " with drift" else "",
        if ("intercept" %in% names(coef(fitted))) " with mean" else ""),
      training_start = min(observed$date),
      training_end = max(observed$date), n_weekdays = nrow(observed),
      forecast_horizon_weekdays = horizon, aicc = fitted$aicc,
      log_residual_sd = sqrt(fitted$sigma2), ljung_box_lag = 20L,
      ljung_box_fitdf = fit_df, ljung_box_p = lb_test$p.value,
      residual_autocorrelation_flag = lb_test$p.value < 0.05,
      converged = fitted$code == 0L)
  }
}

# Validate forecast positivity, ordering, finite values and the requested endpoint.
forecasts <- bind_rows(forecasts)
backtest <- bind_rows(backtest)
diagnostics <- bind_rows(diagnostics)
numeric_forecasts <- forecasts %>% select(point_mean, point_median, lower_80, upper_80, lower_95, upper_95)
if (any(!is.finite(as.matrix(numeric_forecasts))) || any(as.matrix(numeric_forecasts) <= 0) ||
    any(forecasts$lower_95 > forecasts$lower_80 | forecasts$lower_80 > forecasts$point_median |
      forecasts$point_median > forecasts$upper_80 | forecasts$upper_80 > forecasts$upper_95) ||
    any((forecasts %>% group_by(series, model) %>% summarise(end = max(date), .groups = "drop"))$end != forecast_end)) {
  rlang::abort("Forecasts failed finite-value, interval, or endpoint validation",
    class = "fx_arima_output_error")
}
backtest_metrics <- backtest %>% group_by(series, model) %>%
  summarise(origins = n_distinct(origin), predictions = n(),
    rmse = sqrt(mean((actual - point_mean)^2)), mae = mean(abs(actual - point_mean)),
    mape_pct = 100 * mean(abs(actual - point_mean) / actual),
    coverage_80_pct = 100 * mean(actual >= lower_80 & actual <= upper_80),
    coverage_95_pct = 100 * mean(actual >= lower_95 & actual <= upper_95), .groups = "drop")

# Month-end snapshots refer to the last weekday, not to monthly average rates.
month_end <- forecasts %>% mutate(month = floor_date(date, "month")) %>%
  group_by(series, model, month) %>% slice_max(date, n = 1L, with_ties = FALSE) %>%
  ungroup() %>% mutate(month_end = ceiling_date(month, "month") - days(1)) %>%
  select(series, model, month_end, forecast_date = date, everything(), -month)

# Save the source checksum, model objects and exact numerical outputs together.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(training, file.path(output_dir, "training_weekdays.csv"))
write_csv(forecasts, file.path(output_dir, "forecast_weekdays.csv"))
write_csv(month_end, file.path(output_dir, "forecast_month_end.csv"))
write_csv(diagnostics, file.path(output_dir, "model_diagnostics.csv"))
write_csv(backtest, file.path(output_dir, "backtest_predictions.csv"))
write_csv(backtest_metrics, file.path(output_dir, "backtest_metrics.csv"))
saveRDS(models, file.path(output_dir, "models.rds"))
jsonlite::write_json(list(analysis_date = as.character(analysis_date),
  source_path = input_path, source_md5 = unname(tools::md5sum(input_path)),
  training_start = as.character(training_start), forecast_end = as.character(forecast_end),
  units = "Current bolivars per USD", point_estimate = "forecast package bias-adjusted mean",
  transform = "log (Box-Cox lambda = 0)", interval_levels = interval_levels,
  interval_method = "Gaussian ARIMA prediction intervals, conditional on fitted parameters",
  calendar = "Monday-Friday; upstream weekday carry-forwards retained",
  backtest = "Three non-overlapping fixed-origin holdouts, each matching the final horizon",
  primary_model = "auto_arima", benchmark = "ARIMA(0,1,0) with drift on logs",
  model_selection = "KPSS differencing; exhaustive exact-ML AICc search; p,q <= 3, p+q <= 5",
  methodology_links = c("https://pkg.robjhyndman.com/forecast/reference/auto.arima.html",
    "https://pkg.robjhyndman.com/forecast/reference/forecast.Arima.html"),
  caveats = c("Local input vintage; no live source refresh",
    "2024 onward is a recent-history baseline, not a validated optimal window",
    "Conditional intervals omit parameter, model-selection and structural-break uncertainty",
    "80/95 percent intervals describe future rates, not confidence intervals for a mean",
    "Official and unofficial rates fitted independently; no spread constraint"),
  package_versions = map_chr(c("forecast", "dplyr", "ggplot2"), ~as.character(packageVersion(.x)))),
  file.path(output_dir, "run_metadata.json"), auto_unbox = TRUE, pretty = TRUE)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "session_info.txt"))

# Plot both primary ARIMA forecasts with nested bands and visible source cutoffs.
history_plot <- training %>% filter(date >= as.Date("2025-01-01")) %>%
  mutate(series_label = factor(series_labels[series], levels = unname(series_labels)))
forecast_plot <- forecasts %>% filter(model == "auto_arima") %>%
  mutate(series_label = factor(series_labels[series], levels = unname(series_labels)))
figure <- ggplot() +
  geom_ribbon(data = forecast_plot, aes(date, ymin = lower_95, ymax = upper_95), fill = "#d9e2ec") +
  geom_ribbon(data = forecast_plot, aes(date, ymin = lower_80, ymax = upper_80), fill = "#9fb3c8") +
  geom_line(data = history_plot, aes(date, rate, color = series_label), linewidth = 0.65) +
  geom_line(data = forecast_plot, aes(date, point_mean, color = series_label), linewidth = 0.8, linetype = "dashed") +
  geom_vline(data = distinct(forecast_plot, series_label, origin), aes(xintercept = origin),
    color = "#687179", linewidth = 0.3, linetype = "dotted") +
  facet_wrap(~series_label, ncol = 2L, scales = "fixed") +
  scale_color_manual(values = c("Official (BCV)" = "#17365d", "Unofficial (Yadio)" = "#c83c3c"), guide = "none") +
  scale_x_date(breaks = as.Date(c("2025-01-01", "2025-07-01", "2026-01-01", "2026-07-01", "2026-12-31")),
    date_labels = "%b %Y", limits = c(as.Date("2025-01-01"), forecast_end)) +
  scale_y_continuous(labels = scales::label_comma(), limits = ~range(pretty(c(.x, 0), n = 6)),
    breaks = ~sort(unique(c(.x, pretty(.x, n = 6)))), expand = expansion(mult = 0)) +
  labs(title = "Exchange-rate ARIMA baselines through December 2026",
    subtitle = "Weekday log-rate models | Dashed: point mean | Bands: 80% (dark), 95% (light) prediction intervals",
    x = NULL, y = "Current bolivars per USD",
    caption = "Source: consolidated BCV / Yadio local data. Training: January 2024 onward.\nConditional intervals exclude structural breaks and parameter uncertainty; inspect backtest coverage.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.caption = element_text(hjust = 0),
    axis.ticks = element_blank(),
    axis.line = element_line(color = "#2b3033", linewidth = 0.35))
ggsave(file.path(output_dir, "fx_arima_forecast.png"), figure, width = 12, height = 6,
  units = "in", dpi = 200, device = ragg::agg_png, bg = "white")
ggsave(file.path(output_dir, "fx_arima_forecast.pdf"), figure, width = 12, height = 6,
  units = "in", device = grDevices::cairo_pdf, bg = "white")
print(month_end %>% filter(model == "auto_arima") %>%
  select(series, month_end, point_mean, lower_80, upper_80, lower_95, upper_95))
print(backtest_metrics)
print(diagnostics)
