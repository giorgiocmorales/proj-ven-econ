# Course basis for baseline forecasting in R

Status: preliminary source review, 22 October 2026 (America/Caracas). Implementation choices remain open for discussion. No forecast models were fitted or changed for this review.

## Source hierarchy and review scope

The IMF course is the primary methodological reference. DataCamp is the secondary reference for practical R implementation and complementary forecasting methods. Existing project data contracts and source provenance govern the Venezuela data used in any eventual implementation.

- [IMF course archive](D:\Documents\2_Educación\Cursos en Línea\edX\IMFx\MFx Macroeconometric Forecasting)
- [DataCamp course archive](D:\Documents\2_Educación\Cursos en Línea\DataCamp\Quantitative Analyst in R Career Track)

The inventory covers every file in both folders, including ZIP member listings and SHA-256 duplicate checks. Text was extracted from all 79 PDFs. Review concentrated on course structure, the forecasting lessons, selected substantive pages, representative EViews programs, and the existing project FX script and saved diagnostics. Two representative PDF pages were rendered and visually checked. This is not an exhaustive reading of every slide or a replication of the exercises. EViews workfiles were inventoried, not executed or decoded; spreadsheets were inventoried, not audited cell by cell.

| Archive | Files | PDFs | Other material |
| --- | ---: | ---: | --- |
| IMF | 138 | 16 | 63 EViews programs, 13 EViews workfiles, 27 CSVs, 7 Excel workbooks, 7 ZIPs, 4 `.crdownload` files, 1 shortcut |
| DataCamp | 68 | 63 | 5 trading-course transcripts; no standalone R scripts or datasets found |

Counts include duplicates. IMF Modules 1 and 2 contain identical EViews introduction PDFs; Modules 3 and 3B contain identical time-series slides. Module 7 has many duplicate programs. No archive files were moved, renamed, deleted, or altered.

## IMF material to use

PDF page references below are one-based file pages, which can differ from the slide's printed number.

| Local material | Verified content | Role in our forecasting work |
| --- | --- | --- |
| Modules 1-2: `M2_Intro_to_EViews.pdf`; `M2_Fiji.prg` | Data inspection, workfile construction, dates, samples, transformations and regression workflow | Translate the data and sample discipline into the existing R pipeline; retain EViews as a teaching reference |
| Module 3: `MFx_Module_3_Properties_of_Time_Series.pdf` | Stationarity, AR/MA/ARMA identification, ACF/PACF, information criteria, residual checks, deterministic versus stochastic trends, random walks, ADF/PP/KPSS tests | Core specification framework; particularly pp. 32-36, 44-49 and 58-74 |
| Module 4: `M4_All_Slides.pdf`; `m4_evalstats.prg`; `m4_fanchart.prg` | Forecast uncertainty, bias and error metrics, benchmark comparison, expanding and rolling estimation windows, evaluation at each horizon, fan charts | Central evaluation framework; pp. 17-28, 31-36 and 71-75 are especially relevant |
| Module 5: `MF_M5_Pt1_VAR_slides.pdf`; `MFx_M5_Pt.2_SVARs_slides.pdf`; `mfx_m5_pt1_var.prg` | VAR/SVAR specification, lag selection, system diagnostics, forecasts, simulations and scenarios | Later multivariate extension after checking compatible frequencies, sample length and stability; structural identification needs its own economic rationale |
| Module 6: `MF_M6_VECM_slides.pdf`; session programs | Cointegration, error correction, restricted/unrestricted VECMs and comparison with differenced VARs | Later extension where plausible long-run relationships and integration properties are supported; pp. 3-5 establish the module's scope |
| Module 7: `MF_M7_slides.pdf`; break and error-irregularity programs | Autocorrelation, heteroskedasticity, normality, parameter instability, Chow/Quandt-Andrews tests and forecasting remedies | Core diagnostics and robustness work, especially pp. 3, 41, 53 and 71 onward |
| Module 8: `M1_Household_Saving_Rate_and_the_Great_Recession.pdf` | Applied US saving/consumption assignment linking long-run relationships and short-run dynamics | Useful worked macroeconomic workflow; the file's internal M1 label does not match its folder number |
| Module 9: `Combination_Forecasts.pdf`; `mf_combination_forecasts.prg`; Stock-Watson paper | Motivation for forecast combinations, weighting methods and implementation issues | Evaluate a simple equal-weight combination alongside individual models; estimate any learned weights without access to the final evaluation outcomes |

The most useful immediate sequence is **3 -> 4 -> 7**, with Module 9 as a controlled extension. Modules 5-6 provide a route to economic systems once the univariate baseline and data availability are established.

## DataCamp material to use

| Local course folder | Verified content | Role |
| --- | --- | --- |
| `3.1_Manipulating Time Series Data with xts and zoo in R`; `3.2_Manipulating Time Series Data in R` | Time indexes, windows, periodicity and aggregation | Support calendar and frequency handling while keeping the project's tidy tables as the main data representation |
| `4_Importing and Managing Financial Data in R` | Source imports, regular/irregular dates, OHLC conventions, aggregation and CSV input | Useful supporting mechanics; use the project's source-specific ingestion and provenance rather than copy old provider examples unchanged |
| `5_Time Series Analysis in R` | Trends, dependence, AR/MA processes and forecasts | Practical foundation and small diagnostic examples |
| `6_ARIMA Models in R` | ARMA/ARIMA/SARIMA identification, residual analysis and forecasts using `astsa::sarima`/`sarima.for` | Supplementary manual specification and seasonal-model examples; a second modeling package is not required just to reproduce its teaching interface |
| `7_Case Study_Analysing City Time Series Data in R` | Merging, missingness, lags, differences and period operations | Useful workflow examples; interpolation and carry-forwards must be justified and flagged, not applied automatically |
| `8_Forecasting in R` | Hyndman's forecasting course: benchmarks, forecast distributions, residuals, train/test separation, time-series cross-validation, ETS, ARIMA and dynamic regression | Main implementation bridge: chapter 2 pp. 17, 23-36; chapter 3 pp. 23-32; chapter 4 pp. 12-25; chapter 5 pp. 2-9 |
| `9_Visualizing Time Series Data in R` | Univariate and multivariate displays and diagnostic plots | Supporting diagnostic communication; match existing project plotting conventions |
| `16_GARCH models in R` | Chapter 1 covers conditional volatility and `rugarch`; pp. 39-40 distinguish volatility and mean forecasts | Optional variance-model extension if residual volatility warrants it; the local archive contains chapter 1 and an xts cheat sheet only |

Portfolio construction, bond valuation, trading and credit-risk lessons have lower priority for this macroeconomic baseline. The intermediate-portfolio and bond folders are empty. `14_Quantitative Risk Management in R/chapter4.pdf` is byte-identical to the credit-risk chapter 4 and actually contains credit-risk cut-off material. Folder labels alone therefore do not establish course completeness or content.

## R translation and preliminary baseline design

This is a proposal for discussion, not a chosen model specification.

1. **Define each target before fitting.** Specify source vintage, units, transformation, frequency, release timing, last usable observation, forecast horizon and whether the output is an end-period level, period average or growth rate. Keep observed data, forecasts and scenario assumptions distinct. Existing FX work is one application; this review does not assume it is the only target.
2. **Inspect series properties.** Examine plots, ACF/PACF, seasonality, level/log and difference behavior, unit-root/stationarity evidence and potential breaks. Do not treat a single test as a mechanical specification decision.
3. **Compare a small candidate set.** Start with naive/random-walk forecasts, drift where appropriate, seasonal naive where justified, parsimonious ARIMA/SARIMA and ETS including a possible damped trend. The candidate set depends on each series; daily FX and quarterly GDP do not require identical models.
4. **Evaluate forecasts at their intended horizons.** Refit across historical forecast origins and compare expanding windows with plausible fixed rolling windows. Keep final evaluation outcomes outside window selection, transformation selection and combination-weight estimation. If historical release vintages are unavailable, label evaluation as pseudo-out-of-sample on revised/current-vintage data rather than real-time forecasting performance.
5. **Evaluate point forecasts and distributions separately.** Report bias, MAE/RMSE, suitable scaled comparisons, interval coverage and interval width by target and horizon. MAPE is unsuitable for zero/near-zero or sign-changing targets. AICc is an estimation criterion, not the final forecast-performance ranking. Overlapping forecast errors require care in inferential comparisons.
6. **Check instability and uncertainty.** Retain residual dependence and variance diagnostics, window sensitivity and explicit break scenarios. Prediction intervals describe possible future outcomes; they do not automatically include model selection, future regime changes or uncertainty in assumed predictor paths.
7. **Add complexity conditionally.** Dynamic regression requires defensible predictors and future predictor paths; VAR/VECM requires compatible data and supported system structure. GARCH addresses changing conditional variance. Combinations are candidates to test, not guaranteed improvements.

The existing `forecast` dependency already supports the main univariate families and rolling-origin evaluation. Keep tidyverse preparation, `%>%`, explicit modeling choices, typed errors and the existing `renv` environment. Additional packages and any change of modeling framework can be decided when implementation is scoped; no dependency changes are needed for this review.

Current primary R references confirm the relevant interfaces: [ARIMA selection](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html), [ETS](https://pkg.robjhyndman.com/forecast/reference/ets.html), and [rolling-origin evaluation](https://pkg.robjhyndman.com/forecast/reference/tsCV.html). Online documentation can describe a different package version from the project's lockfile; verify against the installed version before coding.

## Relationship to the existing FX baseline

- [Existing R baseline](D:\Documents\6_Data Science\Portfolio\proj-ven-econ\scripts\external_100_fx_arima.R)
- [Saved backtest metrics](D:\Documents\6_Data Science\Portfolio\proj-ven-econ\outputs\forecasts\fx_arima\backtest_metrics.csv)
- [Saved model diagnostics](D:\Documents\6_Data Science\Portfolio\proj-ven-econ\outputs\forecasts\fx_arima\model_diagnostics.csv)

The script independently fits BCV and Yadio weekday log-rate models from January 2024. It compares AICc-selected ARIMA against a log random walk with drift, refits at three non-overlapping holdout origins, saves 80%/95% prediction intervals and checks residual serial correlation. Its configured analysis date is 21 October 2026. This review reads the saved results; it does not refresh their source data or rerun the models.

| Series | ARIMA RMSE | Drift RMSE | ARIMA 95% coverage | Drift 95% coverage |
| --- | ---: | ---: | ---: | ---: |
| Official | 60.94 | 44.87 | 87.4% | 71.7% |
| Unofficial | 101.36 | 69.07 | 100.0% | 100.0% |

RMSE is in current bolivars per USD. These are pooled results from only three holdouts, with 159 official and 162 unofficial prediction rows; they are not independent repeated estimates of interval reliability. Lower point error does not establish better uncertainty calibration. The drift fits also carry residual-autocorrelation flags in the saved diagnostics. Neither method is established as the production winner by this evidence alone.

Relative to the course framework, the immediate gaps are the narrow model comparison, only three origins, pooled rather than horizon-specific metrics, an unvalidated fixed start date, limited break/variance diagnostics, and separate modeling of the two rates without a joint economic restriction. These are discussion items rather than authorization to change the code. A useful first implementation would extend the current evaluation and candidate set before building a large multivariate system.

## Exercise portability and cautions

CSV exports are available in ZIPs for the introductory, Module 3/3B, Module 4, Module 5 and Module 7 exercises. Module 7 also has unpacked CSVs. This permits selected R replication without requiring EViews to estimate those examples. No matching CSV/Excel exports were found locally for the Module 6, 8 and 9 workfiles; direct replication may need an export or another verified source.

Treat the EViews programs as teaching specifications. For example, Module 9's combination program calculates inverse-MSE weights using the 2008Q1-2009Q4 outcomes and evaluates the weighted combination over that same period. That illustrates a weighting calculation, but its reported comparison cannot be reused as an untouched out-of-sample evaluation of estimated weights. An R implementation should separate weight estimation from final assessment. Module 6's forecast comparison program also explicitly comments on evaluation periods that include pre-forecast observations; use the genuinely held-out dates in any translation.

Four `.crdownload` files and some differently named program variants are present. Three `.crdownload` files are byte-identical to completed `.prg` copies, so their names alone do not establish whether useful content is missing. None should be treated as a canonical input without checking its completed counterpart.

## Decisions for the coding discussion

The next discussion should settle the initial target variables, reporting frequency and horizon, preferred point statistic and loss, available historical release vintages, candidate estimation windows and the role of conditional economic scenarios. Then specify the smallest reproducible R extension, outputs and acceptance criteria. This review proposes the methodological basis; it leaves those substantive choices open.
