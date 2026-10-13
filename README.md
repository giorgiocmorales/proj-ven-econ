# Venezuela Economy Data Pipeline

R-first repository for collecting Venezuelan macroeconomic data from official and secondary sources, preserving source-level extracts, and preparing a base layer for consultancy reporting and later forecast modules.

## Current Scope

- `FX`: official BCV and alternative sources, with a combined final dataset.
- `Prices`: BCV CPI source extract.
- `Monetary`: BCV monetary base and liquidity source extracts.
- `Financial`: BCV financial institution balance sheets, reserve requirements, and reserve positions.
- `Oil`: OPEC production and price tables plus IMF WEO oil price benchmarks.
- `Real activity`: BCV quarterly and annual GDP workbooks plus IMF/WB GDP and per-capita indicators.
- `External`: BCV balance of payments, BCV reserves, and IMF/WB reserve proxies.
- `Fiscal`: IMF/WB public expenditure and fiscal balance indicators.

## Structure

- `scripts/`: flat script layout with one script per dataset/source extract plus domain runners.
- `data/raw/`: downloaded source files; OPEC reports are retained by publication month.
- `data/interim/`: source-level extracts that preserve the original published figures and metadata.
- `data/processed/`: processed outputs for existing domains.
- `data/final/`: combined or estimated datasets.
- `registry/series_registry.csv`: minimal dataset registry.

## Naming

- Script pattern: `<domain>_<nn>_<variable>_<source>.R`
- The sequence number is unique within each sector.
- Variable names should stay short.
- Examples: `prices_01_cpi_bcv.R`, `external_02_fx_ind_bcv.R`, `real_02_gdp_a_s_bcv.R`

## Runners

- Install R dependencies on a fresh machine with:

```r
source("scripts/install_dependencies.R")
```

- `scripts/run_prices.R`
- `scripts/run_monetary.R`
- `scripts/monetary_92_aggregates_graphs.R`: creates standalone weekly monetary-base/liquidity charts from 2020, retaining original observation dates and annual x-axis labels. Real levels use estimated daily headline CPI on each observation date, allocating published monthly inflation by constant daily compounding; the reference is the latest common observation's month. Nominal growth compares against 364 days earlier, using a preceding source observation within seven days only when the exact date is absent. Uses grouped headline CPI when available, otherwise the original headline extract, and the configured report cutoff. Saves exact data in `data/final/monetary_92_aggregates.csv`, weekly coverage in `data/final/monetary_92_weekly_coverage.csv`, and PNG/PDF in `outputs/figures/`.
- `scripts/run_financial.R`
- `scripts/run_oil.R`
- `scripts/run_external.R`
- `scripts/external_99_fx_report_graphs.R`: after FX consolidation, exports standalone official/unofficial daily rates and the percentage gap with a trailing 30-calendar-day average, using the approved report theme. Reads the recent-history window and cutoff from `config/report.yml` (or `REPORT_CONFIG`). Saves PNG/PDF charts under `outputs/figures/` and plotted values/coverage under `data/final/external_99_fx_graph*.csv`. The average requires 30 finite daily values and uses the consolidated pipeline's carried-forward rates.
- `scripts/external_96_bop_goods.R`: creates report-ready annual and quarterly BCV goods exports and imports, with import magnitudes shown as positive values.
- `scripts/run_real.R`
- `scripts/real_91_gdp_quarterly_graphs.R`: exports standardized BCV quarterly year-over-year GDP growth bars (full history and 2019 onward) to PNG/PDF in `outputs/figures/`. Run after the quarterly GDP downloader. If the annual chart data from `scripts/real_90_gdp_growth_graphs.R` exists, also exports a side-by-side annual/quarterly comparison with a shared percentage scale. Exact plotted observations are retained in `data/final/real_91_gdp_*graph.csv`; internal quarterly gaps stop generation.
- `scripts/run_fiscal.R`
- `scripts/fiscal_05_components_imf.R`: downloads IMF WEO general-government revenue, expenditure, and net lending/borrowing as percentages of GDP together; failures preserve the existing extract.
- `scripts/fiscal_90_balance_graph.R`: validates the three IMF fiscal components and exports annual revenue as positive navy bars, expenditure as negative red bars, and balance as a yellow line. Uses the configured history start and completed calendar years before the report cutoff year; preserves the IMF values and vintage metadata in `data/final/fiscal_05_components_imf.csv` and the selected observations in `data/final/fiscal_90_balance_graph.csv`. PNG/PDF assets are saved in `outputs/figures/`.
- `scripts/run_all.R`
- `scripts/run_validate.R`: validates report-critical outputs after extraction and stops on missing files, schema drift, duplicate keys, invalid values, or stale observations.
- `scripts/convert_smc_manual_workbooks.R`: converts the six legacy BCV SMC workbooks with Windows Excel before a full historical SMC build.
- `scripts/prices_02_inflation_bcv.R`: derives monthly, year-over-year, and calendar-year inflation from the BCV CPI index.
- `scripts/prices_03_cpi_groups_bcv.R` and `scripts/prices_04_inflation_groups_bcv.R`: download the national CPI group table and derive headline/food monthly and year-over-year rates. Food includes non-alcoholic beverages. Both run in `run_prices.R`; set `BCV_CPI_GROUPS_OFFLINE=true` to reprocess the cached workbook. The separate headline extract is preserved and its overlap is audited in `data/interim/prices_03_headline_overlap_check.csv`.
- `scripts/prices_90_inflation_fx_graphs.R`: after price extraction and FX consolidation, exports standalone headline/food year-over-year inflation and a grouped-column monthly inflation/FX comparison. FX changes use complete calendar-month averages of consolidated daily rates; the comparison ends at the latest complete month common to all four series. Uses the report configuration, saves exact plotted data in `data/final/prices_90_*`, and exports PNG/PDF to `outputs/figures/`.
- `scripts/report_00_availability.R`: audits monthly, weekly, daily, quarterly, and fiscal input availability by series using `registry/report_availability.csv`; saves latest observations and eligible month-end coverage to `output/reports/data_availability.csv`.
- `scripts/report_02_graphs.R`: runs the standardized domain graph scripts listed in `registry/report_graphs.csv` and writes a checksum receipt for their inputs and exported assets.
- `scripts/report_01_data_layer.R`: assembles validated report variables into a standard long data layer using `registry/report_series.csv`.
- `report/venezuela_macro.qmd`: editable Quarto report source; the report runner renders Word and exports the matching PDF through Microsoft Word.

## Client report build

Chart conventions: use solid lines without point markers unless explicitly requested. Put `Notas` above `Fuentes` in captions. End each y-axis on an explicit labeled break with no upper expansion, but hide all protruding tick marks. Both x- and y-axis lines are solid charcoal (`#2b3033`, `linewidth = 0.35`). Zero is a thinner solid grey reference (`#687179`, `linewidth = 0.25`); when it coincides with the bottom boundary, the axis remains a single visible line. Use palette colors at full opacity. Keep these settings inline in the ggplot scales/layers and use the shared theme for axis styling. Automatic report scales include their upper endpoint explicitly because recalculated pretty breaks can otherwise omit it.

Prefer a single horizontal legend row whenever labels fit at the normal font size; retain wrapping for dense legends or narrow panels. On linear y-axes, round negative lower limits outward to a complete major-tick interval (including at least one full interval below zero when values are negative), rather than ending the axis just below zero at the data minimum.

Restore the project packages with `renv::restore()`. R and the Quarto CLI must be installed. The template-faithful report runner also requires Microsoft Word on Windows to export PDF from the editable Word document.

Edit `config/report.yml` for the client name, publication date, graph history start, recent inflation/FX window, and executive-table years. The analysis cutoff is automatically the last day of the month before publication, shared by report charts and standardized exporters; sources may end earlier. Keep the analyst prose in the editable files under `report/sections/`; `report/venezuela_macro.qmd` controls their order and output formats, while `R/report_build.R` prepares validated data, figures, and tables. The EMA/Coca-Cola macro module includes weekly monetary indicators and a second inflation/FX page; each page has short analyst blocks and consistently sized charts exported by the domain graph scripts. `report/templates/estevez_format.docx` is an unchanged copy of the client's Word template: preserve its original header/footer artwork, icons, positioning, styles, and distinct first-page settings. Pandoc ignores the reference document's body; the memo cover and closing/signature page are outside this macro module. The executive table follows the narrative summary on its own page and is also exported as CSV. It shows the configured closing year and the next two years; model estimates and projections remain pending.

From the repository root, run `Rscript scripts/run_report.R` to audit existing source availability, validate the selected inputs, rebuild the report data layer, regenerate standardized graph assets, and render PDF and Word. Set `REPORT_CONFIG` to another configuration file for another edition. To refresh every domain before rendering, use `Rscript scripts/run_all.R`; individual domain runners remain available for selective refreshes.

The report build writes PDF and Word to `output/reports/report/`, standardized PNG/PDF graphs to `outputs/figures/`, and per-variable date coverage to `output/reports/source_coverage.csv`. It also saves the edition's configuration, harmonized data, and a checksum manifest in `output/reports/`. Required sources and individual report variables must pass validation before rendering. The report consumes those exact graph exports; direct Quarto rendering rejects changed inputs or assets. Grouped headline CPI, SMC FX, and the same IMF fiscal components feed the data layer and charts. The cutoff restricts observation dates, not historical source vintages. Set `BCV_MONETARY_OFFLINE=true` to reprocess validated cached weekly workbooks after a separate source download. Forecasts, recovery scenarios, and SENIAT revenue remain explicitly unfinished analyst blocks.

Run `Rscript tests/report_pipeline.R` for offline checks of report date windows and validation failures.

## Metadata Catalogs

- `registry/venezuela_currency_scales.csv`: records Venezuela's three redenominations and conversion factors to the current bolivar scale.
- `scripts/catalog_imf_wb.R`: refreshes World Bank indicator metadata and IMF dataflow, dimension, and code catalogs into `registry/`.
- `scripts/catalog_opec_momr.R`: extends the MOMR inventory from 2001 onward and attempts official archive-link discovery for new or unresolved reports. Registry candidates remain unverified until download and corpus validation succeed.

## OPEC monthly corpus

Run `Rscript scripts/run_oil.R` from the repository root to refresh the inventory, validate PDF downloads, extract the corpus, publish the Venezuela production and crude-price views, and refresh the separate IMF benchmarks. For an offline rebuild of cached reports, run `scripts/oil_13_process_momr_corpus.R`, then `scripts/oil_01_prod_opec.R` and `scripts/oil_02_price_opec.R`. Run `scripts/oil_90_graphs.R` to export the standardized graphs. These views contain monthly observations only; annual, quarterly and year-to-date columns are not mixed into monthly series.

`registry/opec_momr_reports.csv` holds publication months and source URLs. The catalog includes completed months by default; set `OPEC_END_DATE` explicitly to include a published current-month issue. `OPEC_START_DATE` and `OPEC_END_DATE` restrict downloads only; extraction always validates the full registered corpus through the current month. Archive-page access failures are logged, with URL-convention candidates retained for validated download attempts. New unclassified retrieval or parser failures stop publication; explicitly reviewed `unavailable` reports are retried and remain visible in the audit.

The corpus covers 307 readable reports from January 2001 through August 2026. The August 2026 PDF was supplied manually and validated; February 2013 remains a documented retrieval gap. No claim is made about reports before the registered archive. `data/processed/oil_13_momr_corpus.csv` preserves monthly report vintages, country/crude identities, raw printed labels and rows, table notes and footnote markers, PDF pages, source URLs, PDF hashes, parser versions and missing-value markers. Its key is `(report_month, source_type, series_id, date)`. `data/final/oil_13_momr_latest.csv` selects the newest published cell for each `(source_type, series_id, date)`, including explicitly missing values. It does not backfill or interpolate them.

Country and crude aliases live in `registry/opec_series_aliases.csv`; unknown labels stop publication for review. Countries follow each report's membership rather than today's membership, and DoC country rows remain separate from aggregate totals. Brent harmonizes the source's Brent/Dated Brent/North Sea Dated labels while preserving the printed identity; Venezuelan BCF-17, Tia Juana Light and Merey remain separate series. Values are preserved as printed: footnotes can qualify their meaning (for example, some 2025 Saudi direct-communication cells describe supply to market). Asterisks are recorded as markers, not assumed to mean provisional data; review `source_table_notes` before extending country comparisons. PDF and text caches support resumable updates and invalidate when source files or parser code change. Valid cached PDFs are reused; set `OPEC_REFRESH_PDFS=true` with a bounded download date range to refresh corrected source issues without replacing a valid PDF with a failed response.

`data/final/oil_13_momr_coverage.csv`, `oil_13_momr_gaps.csv` and `oil_13_momr_report_audit.csv` distinguish series coverage, missing months/cells and report retrieval/extraction outcomes. Staged audits and OPEC-total reconciliation are in `data/interim/`. Complete production tables must reconcile within independent rounding tolerance. Latest Venezuela direct production has unreported August–October 2014 cells; Merey has no March 2020 observation in the retrieved monthly tables. Graphs leave these gaps open. Run `Rscript tests/opec_corpus.R` for offline parser regression checks. After building the corpus and views, run `Rscript tests/opec_corpus_outputs.R` for source-value and report-contract checks, and `Rscript tests/opec_corpus_rebuild.R` to verify a byte-identical cached rebuild.

## Notes

The standalone `scripts/oil_14_audit_direct_history.R` compares an external reference workbook (`OPEC_REFERENCE_WORKBOOK`) against all MOMR vintages, scans cached reports for earlier direct-production evidence, and compares 2002–2011 with official JODI monthly data. The audited MOMR monthly direct series begins in January 2012, published in the April 2012 issue. JODI matches 87 of the workbook's 120 observations for 2002–2011; 33 differences in 2007–2009 remain unresolved. Audit comparisons are saved in `data/interim/`; neither the workbook nor the canonical series is modified. JODI's crude definition includes lease condensate, so extending the graph requires source-definition review before combining the series.

- Source-only extracts are the priority in this phase.
- Combined estimates, house adjustments, forecast diagnostics, and scheduled runs are intentionally deferred.

### Estimated daily and weekly inflation

- `scripts/prices_05_daily_cpi_bcv.R` allocates headline monthly CPI changes over actual calendar days: daily factor = (current CPI / previous CPI)^(1 / days in month). The estimated daily CPI treats published monthly indices as month-end anchors and reconciles exactly to them.
- `scripts/prices_06_weekly_inflation_bcv.R` compounds those daily factors over Saturday-Friday weeks. Cross-month weeks use the appropriate factor for each day. Only complete seven-day weeks are published in `data/final/prices_06_weekly_inflation_bcv.csv`; month contributions and boundary coverage are audited in `data/interim/prices_06_weekly_*.csv`.
- Both run through `scripts/run_prices.R`. These are separately labelled estimates and do not replace published monthly inflation. Monetary observations with irregular spacing retain their actual date intervals, distinct from the fixed seven-day series.
- Offline checks: `Rscript --vanilla tests/weekly_inflation.R` verifies monthly reconstruction, leap-year/month lengths, mixed-month compounding, and daily CPI ratios.

## Continue on another device

Current report work is on `codex/macro-report-plan`. The report cutoff remains `2026-09-07`; pushing the code does not refresh the source data. Monthly inflation/FX now uses grouped columns; monetary charts retain weekly observations from 2020 and use the shared estimated daily CPI for real levels.

The repository excludes `data/`, `output/`, `outputs/`, and `tmp/`. Copy those data folders separately if the exact local source vintage is needed, or use the domain runners above to download and rebuild inputs. Install dependencies first. Run `scripts/run_prices.R` before `scripts/monetary_92_aggregates_graphs.R`; the latter also requires the monetary runner outputs. Run `scripts/prices_90_inflation_fx_graphs.R` after both price extraction and FX consolidation. Chart scripts save PNG/PDF under `outputs/figures/`.

Daily CPI and Saturday-Friday weekly inflation are reusable derived estimates; published monthly inflation remains separate. For irregular monetary observation intervals, use daily CPI at the actual endpoints. Offline numerical validation: `Rscript --vanilla tests/weekly_inflation.R`.
