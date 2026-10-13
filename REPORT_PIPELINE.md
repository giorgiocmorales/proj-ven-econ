# Venezuela Macro Report Pipeline

## Objective

Build a concise, repeatable six-section Venezuela macroeconomic report in R and Quarto. Public-data extractors, transformations, forecasts, charts, tables, and PDF rendering should be automated while the analytical prose remains easy to edit for each publication.

The report should be reproducible from a fresh GitHub clone on another computer and should fail explicitly when source data are missing, stale, incomplete, or structurally different from expectations.

## Report structure

The PDF will use one standardized page or section for each topic.

### 1. Executive summary

- One or two short paragraphs.
- A short list of key considerations and risks.
- A table with major variables by year.
- Forecast years visually distinguished from historical observations.

The initial table should include real GDP growth, inflation, the exchange rate, oil production, the Merey oil price, exports or the current account, the fiscal balance, and SENIAT tax revenue when available.

### 2. Growth

- One analytical paragraph.
- Annual real GDP growth and forecast chart.
- A second paragraph or compact table explaining recovery scenarios.

Recovery scenarios will compound alternative GDP paths against a configurable reference peak and report the first year in which each path recovers that level.

### 3. Inflation and exchange rate

- One analytical paragraph.
- Monthly and annual inflation chart.
- Official and alternative exchange rate chart.

Monthly inflation will use the published BCV change when valid. Annual inflation will be calculated consistently from the CPI level or compounded monthly changes.

### 4. Oil

- One analytical paragraph.
- A two-panel figure with Venezuelan oil production and oil prices.

OPEC secondary-source production will be the preferred production series. Direct communication can be retained as a comparison. Merey will be the preferred Venezuelan price, with WTI or the OPEC Reference Basket as an international comparator.

### 5. External sector

- One analytical paragraph.
- A chart covering exports and imports together with exchange rate intervention timing or rates.

The existing BCV intervention source reports intervention dates, identifiers, and EUR rates, but not intervention amounts. The report must label this information accurately unless an additional source for intervention amounts is added.

### 6. Fiscal

- One analytical paragraph.
- A two-panel figure with the IMF fiscal balance estimate and SENIAT tax revenue.

The IMF general government balance as a share of GDP will be the initial fiscal balance series. A SENIAT revenue extractor or a controlled manual-input contract must be added before this page is complete.

## Pipeline architecture

The intended dependency flow is:

```text
Public extractors
  -> data contracts and quality checks
  -> harmonized report series
  -> forecasts and scenarios
  -> charts and executive table
  -> Quarto PDF
  -> rendered-page quality assurance
```

The initial implementation should use the following structure:

```text
_targets.R
_quarto.yml
config/report.yml
report/venezuela_macro.qmd
registry/report_series.csv
R/report_inputs.R
R/report_figures.R
R/report_tables.R
R/forecast_growth.R
R/forecast_prices.R
R/forecast_external.R
R/forecast_fiscal.R
tests/testthat/
output/pdf/
```

The current domain runners should be wrapped rather than rewritten during the first iteration. The `targets` package will provide dependency tracking, caching, and selective reruns. Quarto should consume only validated report-ready outputs, not source extracts directly.

## Initial data selection

| Report variable | Preferred source | Current status |
|---|---|---|
| Real GDP growth | BCV annual GDP | Available |
| GDP forecast benchmark | IMF WEO | Available |
| GDP recovery scenarios | Chained BCV GDP level or index | Transformation required |
| Monthly and annual inflation | BCV CPI | Annual transformation required |
| Official exchange rate | BCV combined FX series | Available |
| Alternative exchange rate | Yadio | Available |
| Oil production | OPEC secondary-source estimate | Available |
| Oil price | OPEC Merey with WTI or ORB comparator | Available |
| Exports and imports | BCV annual balance of payments | Exact series mapping required |
| FX interventions | BCV intervention page | Dates and rates only |
| Fiscal balance | IMF balance as a share of GDP | Available |
| Tax revenue | SENIAT | Extractor missing |

`registry/report_series.csv` will map report variables to dataset IDs, stable series identifiers, frequencies, units, transformations, preferred sources, and fallbacks. Report code should not depend on repeated free-text matching of source labels.

## Execution phases

### Phase 1: Reproducible foundation

- Initialize a project-local `renv` environment.
- Commit `renv.lock` for cross-computer reproducibility.
- Add the required Quarto, `targets`, forecasting, table, and testing packages to project configuration.
- Add explicit report settings for publication date, data cutoff, forecast horizon, historical window, and recovery baseline.
- Document external Quarto and TeX requirements in the main README.

### Phase 2: Extractor verification

Run each domain independently before running the complete pipeline. A dataset will count as successful only when it passes all applicable checks:

- The expected file exists and is non-empty.
- Required columns and units are present.
- Series and date keys are unique.
- Dates parse correctly.
- The latest observation satisfies a source-specific freshness rule.
- Values pass plausible range and missingness checks.
- No critical extractor errors remain.

The current IMF, World Bank, and OPEC logic can produce partial or empty datasets without failing the domain runner. Those cases must be promoted to explicit validation errors when they affect report variables.

The external-sector outputs also require a clean rerun because some local files retain older dataset IDs than the current scripts and registry.

### Phase 3: Report-ready data layer

- Create the report-series registry.
- Standardize all selected observations into a long schema.
- Preserve source, download vintage, provisional status, unit, frequency, and transformation metadata.
- Produce annual summary values for the executive table.
- Add explicit `actual`, `forecast`, and `scenario` roles.

A target report schema should contain at least:

```text
variable_id
date
frequency
value
unit
source_id
dataset_id
vintage
observation_role
scenario
```

### Phase 4: Quarto template and visual system

- Create the six-section Quarto document with controlled page breaks.
- Retain and minimally extend the existing `ven_theme()` chart style.
- Use consistent dimensions, typography, colors, forecast shading, captions, and source notes.
- Keep prose in clearly marked analyst-editable blocks.
- Keep calculations and chart construction outside the prose document.
- Render every meaningful PDF revision to images and inspect all pages for clipping, spacing, alignment, readability, page transitions, headers, and footers.

### Phase 5: Forecasts and scenarios

Begin with transparent benchmarks and only retain additional complexity when rolling-origin evaluation shows a practical improvement.

#### GDP

- Compare naive, drift, ETS, and ARIMA benchmarks.
- Use IMF WEO as an external reference rather than an unquestioned model output.
- Build explicit baseline, upside, and downside paths.
- Calculate recovery years from each scenario.

#### Inflation

- Model monthly log changes rather than the extreme CPI level.
- Compare seasonal naive, ETS, ARIMA, and exchange-rate-assisted specifications.
- Evaluate recent-window alternatives because structural breaks can make the full historical sample misleading.

#### Exchange rate

- Retain a random-walk or drift benchmark.
- Add inflation-consistent scenario paths.
- Present scenario uncertainty rather than false point-estimate precision.

#### Oil

- Use constrained scenarios for Venezuelan production.
- Anchor price assumptions to external oil-price benchmarks.
- Avoid fitting complex models to the short extracted Merey history.

#### External and fiscal variables

- Prefer transparent accounting and scenario relationships tied to GDP, oil, inflation, and exchange rate assumptions.
- Retain IMF estimates as references for fiscal forecasts.
- Evaluate forecast accuracy with rolling-origin backtests where sufficient history exists.

## Model orchestration

Work should be assigned according to the complexity and value of the decision:

- **Terra:** repository inspection, repetitive transformations, data-contract tests, chart implementation, and Quarto styling iterations.
- **Luna:** data mapping, moderate-complexity report components, and documentation-quality work when the model is available.
- **Sol:** architecture, econometric choices, structural-break treatment, difficult extractor failures, and final integration review.

If Luna is unavailable in a working environment, its tasks should normally be routed to Terra. Sol should be reserved for work where deeper reasoning is likely to materially change the result.

## GitHub and portability

The repository should track source code, registries, configuration, tests, the dependency lockfile, and the report template. Downloaded data can remain ignored when it can be regenerated, but small frozen test fixtures should be committed so parser and contract tests do not depend on live websites.

A fresh-computer workflow should eventually be:

```r
renv::restore()
targets::tar_make()
```

The complete pipeline should stop with a specific error when a required source cannot be refreshed or a report variable fails validation. It should never silently publish a report from incomplete critical inputs.
