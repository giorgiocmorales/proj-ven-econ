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
- `scripts/run_financial.R`
- `scripts/run_oil.R`
- `scripts/run_external.R`
- `scripts/external_96_bop_goods.R`: creates report-ready annual and quarterly BCV goods exports and imports, with import magnitudes shown as positive values.
- `scripts/run_real.R`
- `scripts/run_fiscal.R`
- `scripts/run_all.R`
- `scripts/run_validate.R`: validates report-critical outputs after extraction and stops on missing files, schema drift, duplicate keys, invalid values, or stale observations.
- `scripts/convert_smc_manual_workbooks.R`: converts the six legacy BCV SMC workbooks with Windows Excel before a full historical SMC build.
- `scripts/prices_02_inflation_bcv.R`: derives monthly, year-over-year, and calendar-year inflation from the BCV CPI index.
- `scripts/report_01_data_layer.R`: assembles validated report variables into a standard long data layer using `registry/report_series.csv`.
- `report/venezuela_macro.qmd`: neutral Quarto scaffold that renders the same report source to PDF or Word.

## Client report build

Restore the project packages with `renv::restore()`. R and the Quarto CLI must be installed; PDF output also requires a working TeX distribution (for example, `quarto install tinytex`).

Edit `config/report.yml` for the client name, publication date, observation cutoff, graph history start, recent inflation/FX window, and executive-table years. Keep the analyst prose in `report/venezuela_macro.qmd`. The EMA/Coca-Cola macro module uses one page per section, one or two short analytical paragraphs, and consistent 6.5-inch charts. `report/templates/ema_reference.docx` is an unchanged copy of the client's Word template: preserve its original header/footer artwork, icons, positioning, styles, and distinct first-page settings. Pandoc ignores the reference document's body; the memo cover and closing/signature page are outside this macro module. The executive table is exported separately so it does not displace the client's narrative summary.

From the repository root, run `Rscript scripts/run_report.R` to validate existing source outputs, rebuild the report data layer, and render PDF and Word. Set `REPORT_CONFIG` to another configuration file for another edition. To refresh every domain before rendering, use `Rscript scripts/run_all.R`; individual domain runners remain available for selective refreshes.

The report build writes PDF and Word to `output/reports/report/`, six main PNG graphs to `output/figures/`, and per-variable date coverage to `output/reports/source_coverage.csv`. It also saves the edition's configuration, harmonized data, and a checksum manifest in `output/reports/`. Required sources and individual report variables must pass validation before rendering. The cutoff restricts observation dates, not historical source vintages. Forecasts, recovery scenarios, and SENIAT revenue remain explicitly unfinished analyst blocks.

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
