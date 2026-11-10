# Venezuela Economy Data Pipeline

R-first hub for Venezuelan economic research and consulting. Sources are cataloged
and preserved, then processed into reusable datasets for models, graphs and client
reports. Start with [AGENTS.md](AGENTS.md) for agent working rules; this README
defines the data lifecycle and maps the implemented workflows.

## Current Scope

- `FX`: BCV and Yadio rates, reviewed Palma history, intervention quotes and FX sales.
- `Prices`: national and Caracas CPI, published-frequency inflation and labelled daily/weekly estimates.
- `Monetary`: BCV monetary base and liquidity source extracts.
- `Financial`: BCV financial institution balance sheets, reserve requirements, and reserve positions.
- `Oil`: OPEC production and price vintages, IMF benchmarks and reviewed Reuters exports.
- `Real activity`: BCV GDP, separately labelled reconstructions, IMF/WB indicators and secondary-source wage history.
- `External`: BCV balance of payments and reserves, IMF/WB proxies and separate UN Comtrade direct/mirror trade profiles.
- `Fiscal`: IMF/WB fiscal indicators and reviewed SENIAT collection sources and consolidations.

## Structure

- `scripts/`: source acquisition, preparation, publication and analysis scripts, logically grouped by domain prefix, plus runners.
- `R/`: shared helpers and validation/build logic.
- `config/`: research profiles and client-edition settings.
- `data/raw/`, `data/interim/`, `data/final/`: the data layers defined below.
- `data/processed/`: retired path; historical extracts are recoverable through the cleanup catalog and migration mapping.
- `registry/`: source, dataset, provenance and consumer inventories.
- `tests/`: validation and regression scripts; some rebuild outputs and are not read-only.
- `report/`: editable client report sources, templates and scoped analytical drafts.
- `output/` and `outputs/`: existing report, chart, model and audit artifacts. Preserve their current consumer paths.

## Data lifecycle

The standard flow is **catalog source -> raw -> interim when needed -> validate
-> final -> models, graphs and reports**.

| Layer | Role | Contents |
| --- | --- | --- |
| `data/raw/` | Original acquisition | Downloaded responses, supplied workbooks, source documents and supporting evidence. Preserve original bytes and versions. |
| `data/interim/` | Preparation and consolidation | Parsed tables, standardized dates/units, merged sources, reconciliations, conflicts, quarantined records and intermediate calculations. This also applies to preparing a single source. |
| `data/final/` | Published data interface | Latest validated, consolidated, reusable datasets consumed by analyses and requested for updates. Retain stable identifiers, units, keys, provenance and quality flags. |

Final data can contain harmonized values or derived estimates; preserve their
original inputs and make transformations and estimate status explicit. Final
does not mean an unchanged download or a claim that every value is official.
Keep reusable coverage in final data; apply client-specific windows, chart
formatting and model specifications downstream. Model results, figures and
rendered reports belong in the existing output locations, with their input versions
identified. Supporting source evidence need not become a final numeric dataset.

The former `data/processed/` contents are archived as historical evidence. Active producers and consumers
use `interim` or `final`; `registry/data_layer_migration.csv` maps the original
paths to their initial, checksum-verified publications. These recorded hashes
identify the migration inputs, not later dataset versions. Historical isolated
workspaces can restore missing current paths through this mapping without altering
old manifests. Raw originals remain preserved independently.

### Source and dataset inventories

A provider, a source artifact and a derived dataset are different entities. Reuse
stable identifiers and link them through existing registry entries and source
manifests. Local folders, supplied files, web pages and APIs all require registration.

| Inventory | Responsibility |
| --- | --- |
| `registry/reproducibility_sources.csv` | Source/acquisition profiles: input selectors, importer chains, staging paths, origin evidence, access, transformations and gaps. Reuse this inventory for new sources; a profile is not proof that replay has been verified. |
| Source-specific registries and raw manifests | Exact URLs or external paths, publication/revision identifiers, acquired files, hashes, retrieval metadata and sheet/cell/page evidence. Examples include OPEC, BCV statements and Palma workbooks. |
| `registry/series_registry.csv` | Dataset inventory: source association, domain, frequency, units, layer paths, readiness and limitations. |
| `registry/data_contracts.csv` and source-specific checks | Executable schema, keys, coverage and value requirements. Every declared final publication requires a contract; producer-specific checks also apply. |
| `registry/reproducibility/` | Generated provenance, input mappings, run evidence and verification results. Preserve historical receipts. |
| `registry/report_availability.csv` | Series to inspect for report availability; inclusion is not report integration. |
| `registry/report_series.csv` | Variables selected for the report data layer. New source families also need an adapter in `scripts/report_01_data_layer.R`. |
| `registry/report_graphs.csv` | Report graph producers, required inputs and expected assets/data; optional availability entries are independent. |
| `registry/pipeline_steps.csv` | Authoritative producer scripts, dependencies, raw input locations, source-profile links, command arguments and declared publications. |
| `registry/data_layer_migration.csv` | Mapping and original checksums for retained legacy extracts; current versions are identified by run receipts. |

Before using a new source, register its identity, provider/supplier, exact URL or
local origin, expected raw destination, intended datasets and remaining questions.
Record unknown origin or rights explicitly. Capture the actual acquisition date,
source vintage and checksum when acquired; never manufacture historical metadata.
For a candidate without an importer, leave the script chain empty and explain its
pending state in `gaps`; do not imply that it can already be executed.

Each dataset needs a defined observation grain and unique key; variable meanings;
units and currency/price base; geography; frequency and date conventions; source
selection and transformation rules; coverage/missingness expectations; and its
producer, dependencies and final path. Keep implementation details in producer
comments/docstrings, tabular metadata in registries and exact evidence in manifests.
Use existing status/notes fields to distinguish received, parsed, validated and
report-integrated work without treating these descriptions as a new enforced enum.
Processing readiness and source quality are separate: validation can pass while
the evidence remains secondary, estimated or subject to unresolved limitations.

Consolidation must retain conflicting alternatives and document which is selected.
Identify priorities by variable and source vintage; a supplied workbook can have
different origins by sheet. Distinguish observations, user fills, reconstructions,
estimates and forecasts. Preserve source values/units alongside adjusted values or
in a linked extract. Use `registry/venezuela_currency_scales.csv` for redenominations;
never join incompatible GDP price bases or CPI geographies without an explicit
method. Specify whether aggregation means a sum, period average or end-period level.
Distinguish observation dates, publication dates, acquisition times and report cutoffs.

### Updating and rebuilding data

An update request identifies a **final dataset**. Resolve its producer, sources
and dependencies before choosing a script. The producer may change over time;
the published dataset remains a reusable interface for multiple consumers.

1. Inspect the inventory, acquisition receipts and valid raw files already held.
2. For a processing-only change, rebuild from existing raw without downloading.
   For a source refresh, acquire only missing periods, new releases or revisions
   that the source's update procedure requires checking.
3. Preserve previous raw versions. For a revisable whole-history workbook/API,
   retrieving a complete new snapshot can be necessary; record the reason and
   selected vintage. Validate cached files before reuse and reacquire corrupt or
   incomplete inputs when needed.
4. Prepare the required intermediates in dependency order. Check schema, keys,
   dates, units, coverage, join cardinality and relevant economic identities.
5. Publish final data only after validation, retaining the previous valid version
   and a receipt linking source versions, code, settings and checks. Regenerate
   dependent analyses when their inputs change. Never manually patch final values.

Every source procedure must specify its revision policy, cache validation, refresh
window, offline mode and behavior on missing/failed inputs. No universal expiry
period is assumed. An unchanged parser/model/chart does not justify downloading
retained history. An offline rebuild does not establish that the provider has no
new releases. The source-specific limitations below distinguish retained historical extracts from complete original response archives.

## Execution modes and dependencies

Run commands from the repository root. Restore the project library with
`Rscript scripts/install_dependencies.R`.

```text
# Rebuild a dataset and its dependencies from retained inputs, without network retrieval.
Rscript scripts/run_data.R prices_04_inflation_groups_bcv rebuild
# Download only missing known inputs and rebuild a selected dataset.
Rscript scripts/run_data.R external_13_fx_sales_monthly resume
# Explicitly check source revisions according to its acquisition procedure.
Rscript scripts/run_data.R prices_03_cpi_groups_bcv update
# Rebuild every registered data procedure; rendering and forecast fitting are separate.
Rscript scripts/run_all.R rebuild
# Render the configured report from published datasets.
Rscript scripts/run_report.R
```

| Mode/task | Behavior |
| --- | --- |
| `rebuild` | Uses retained source bytes or explicitly identified legacy source extracts. Missing required cache fails. It never downloads. An ordinary rebuild leaves current raw files unchanged; an explicit failed-run continuation can publish source bytes already acquired in that failed run. |
| `resume` | Reuses valid existing inputs and acquires missing known files/queries. This is the default for domain runners; it does not establish that the provider has no newer releases. |
| `update` | Explicitly refreshes revisable sources. BCV monthly FX reuses validated historical exports with matching date windows unless history refresh is requested; financial workbooks keep immutable historical releases; OPEC discovers releases and retains valid PDFs unless PDF refresh is requested. Single-file revisable workbooks/API snapshots can require complete new responses. |
| Dataset/domain selection | `run_data.R` accepts a registered dataset identity, declared output basename, step ID, domain or `all`. It resolves dependencies once and excludes unrelated branches. Domain runners accept an optional acquisition mode and use the same planner. |
| Validation | `run_validate.R` checks current final-data contracts. An optional ISO date sets its freshness reference. The dataset runner records stale coverage separately from structural failures; reports still enforce their own freshness limits at the edition cutoff. |
| Reporting | `run_report.R` audits availability, validates selected sources, assembles edition data, builds graphs and renders Word/PDF. It does not refresh Reuters or reconstruct GDP. Graph receipts cover declared graph inputs and selected report sources. |
| Historical response replay | `run_reproducibility.R` retains its immutable-baseline/parent-run semantics and separate source-profile operations. These runs do not automatically publish to current data. |

The runner copies required source inputs, retained datasets and code into
`data/pipeline/runs/<run_id>/workspace/`. It validates the declared publications,
then promotes changed data as a recoverable group. Removal of existing observation keys is rejected, so an incomplete response cannot silently truncate history. Failures preserve current data
and retain their workspaces/errors. Each run records configuration, staged code,
input/output checksums, validation results, publication paths, messages/warnings and success/failure. Explicit runner modes govern acquisition; legacy offline flags are temporarily overridden and restored afterward.
Prior published bytes remain in that run's `previous/` directory for recovery.
Retain these ignored receipts and backups when migrating or backing up the project.

To continue after a failed acquisition or processing run, append its run ID:
`Rscript scripts/run_data.R <same_target> <mode> <failed_run_id>`.
The runner reuses that workspace's acquired raw files, checks for changed current
inputs and validates the complete publication again. Use `rebuild` when the failed
run already acquired every required input, or `resume` to acquire missing inputs.
Domain runners likewise accept `<mode> [failed_run_id]`. A new update without a
parent starts from the current published cache rather than the failed workspace.

Targets rebuild their upstream dependencies; they do not automatically rebuild every downstream analysis. Request the derived dataset that an analysis consumes, or use `all`, when its inputs need refreshing. Models and charts consume reusable final datasets. Edition assemblies live in
`output/reports/`, exact plotted tables in `outputs/data/`, and figures in
`outputs/figures/`. Internal producer scripts remain available for debugging and
regression tests; direct invocation bypasses the runner's group publication guard.
Use the dataset runner for normal updates.

Dependency order belongs to `registry/pipeline_steps.csv`, not numeric filenames.
Price inputs precede monetary deflation; fiscal consolidation runs supplements
before consolidation; GDP reconstruction requires all four BCV growth extracts;
Palma history requires canonical FX and historical quotes. OPEC acquisition precedes
corpus processing, with live discovery limited to explicit update mode. The default
Comtrade profile is included in the data inventory; other profiles remain available
through its explicit profile/mode CLI. Cross-country research, exploratory forecasts
and document-only archives have separate entry points.

### Source and coverage limitations

- The BCV index's historical HTML responses were not retained. Cached rebuilds
  validate a preserved extraction explicitly identified as legacy evidence;
  new page acquisitions are stored under stable raw paths.
- The retained Yadio history includes the complete parsed response table from its
  original acquisition, but not its historical HTTP JSON. Rebuilds use that table
  when JSON is unavailable. New normal API acquisitions retain response bytes;
  never present historical parsed extracts as original HTTP responses.
- Some SMC historical workbooks are unavailable locally; the preserved assembled
  extraction retains that history while available current-quarter workbooks are
  reprocessed. Missing historical bytes cannot be recreated by this migration.
- Palma final quotes contain usable records. Parsed cells and quarantined records
  stay in interim, including dates that block carry-forwards. Daily consolidation
  retains its existing canonical-source priority and bounded-carry method.
- The IMF reserves candidate remains `needs_review`; BCV statements remain a
  document archive with numeric extraction pending. They are not executable
  numeric publications in the dataset runner.
- File modification times are separate from verified acquisition times. Financial extracts retain `source_file_modified_at`; unavailable acquisition times remain missing rather than being replaced with the rebuild clock.
- Historical source rights, provenance gaps and explicit user fills remain as
  recorded in source-specific profiles. No migration establishes missing evidence.

## Naming and domain organization

Scripts currently use a flat directory, grouped logically by domain:

| Prefix | Category |
| --- | --- |
| `external` | Exchange rates, FX sales, balance of payments, reserves and trade |
| `oil` | Oil production, prices and exports |
| `prices` | CPI and inflation |
| `real` | GDP, real activity and wages |
| `monetary` | Monetary aggregates and related derived series |
| `financial` | Banking, reserve requirements and financial statements |
| `fiscal` | Public finance and tax collection |
| `cross_country` | Separate comparative research |
| `report` | Client-edition assembly and assets |

### Script and dataset names

- Script pattern: `<domain>_<nn>_<variable>_<source>.R`
- Check existing names before assigning a numeric identifier. Existing legacy names can share numeric prefixes; numbers are not dependency order.
- Variable names should stay short.
- Examples: `prices_01_cpi_bcv.R`, `external_02_fx_ind_bcv.R`, `real_02_gdp_a_s_bcv.R`

Keep published dataset identities stable and descriptive of their contents.
Existing numeric filenames remain compatibility interfaces; consumers should use
the registered dataset path, not infer it from a producer's filename or position.
Document one authoritative producer per output and parameterize differing research
profiles instead of duplicating whole scripts. Physical domain subfolders can be
introduced through a coordinated migration of callers and registries; they are
not required to establish logical domain grouping.

## Repository housekeeping

`registry/repository_cleanup.csv` records archived artifacts, reasons and exact
recovery paths. Recovery bundles live outside Git under
`../proj-ven-econ-review-archive/`; retain them separately when migrating the
project. Each bundle preserves original relative paths and checksum inventories.
Recover an individual file from its cataloged archive location only after checking
its checksum and confirming that the original destination will not be overwritten.

The forecasting review and its supporting material remain in `references/` and
`tmp/forecasting_course_review/`. Historical provenance receipts retain original
paths; use the cleanup catalog to locate intentionally archived files. Historical
references to retired files do not imply that they are current workflow inputs.

## Reproducible source acquisition and preserved baselines

Use R 4.6.1 (the version in `renv.lock`) and restore the project library with
`Rscript scripts/install_dependencies.R`. The reproducibility runner checks direct
dependencies against the lock, including equivalent R version spellings such as
`0.6-5` and `0.6.5`; it never rewrites the lock. The immutable baseline is preserved independently of ordinary
current outputs, which legacy runners can rebuild.

The separate entry point for historical isolated source builds and provenance operations is
`Rscript scripts/run_reproducibility.R`. It is distinct from ordinary domain and
report runners. All arguments are explicit; `-` means no parent/ingestion. The
commands below illustrate separate operations, not a sequence to run in full:

```text
# Create a baseline only for initial setup when no baseline exists.
Rscript scripts/run_reproducibility.R baseline
# Audit or verify the retained baseline and run evidence.
Rscript scripts/run_reproducibility.R audit
Rscript scripts/run_reproducibility.R verify
Rscript scripts/run_reproducibility.R objects
Rscript scripts/run_reproducibility.R policies
# Example live update and replay; choose unused run IDs and inspect the source policy.
Rscript scripts/run_reproducibility.R update yadio_fx yadio-v1 - -
Rscript scripts/run_reproducibility.R replay yadio_fx yadio-replay yadio-v1 -
Rscript scripts/run_reproducibility.R rebuild reuters_exports reuters-v1 - -
# Example reviewed manual ingestion and isolated rebuild.
Rscript scripts/run_reproducibility.R ingest interventions_workbook "path/to/reviewed-bundle" "User supplied; original author unknown" "2026-10-reviewed"
Rscript scripts/run_reproducibility.R rebuild interventions_workbook intervention-v2 intervention-v1 "path/to/ingestion-manifest.csv"
Rscript tests/reproducibility.R
```

`baseline` is a one-time operation and rejects an existing baseline. Reuse the
existing inventory; routine updates must not reset it. It stores
SHA-256-addressed copies of existing data, source documents, code and configuration
under `data/reproducibility/objects/`; its tracked inventory is
`registry/reproducibility/baseline_files.csv`. Later audits compare against that
inventory and never accept changed bytes into it. Source evidence includes current
R scripts, scratch R code, `.Rhistory`, raw ledgers, registries, local-reference
checks, and reachable historical Git source blobs. Lexical URL matches are evidence
candidates, not validated retrieval URLs. `source_records.csv` separately preserves
exact URL/version/checksum/access cells from existing CSV registries and raw ledgers.
Original JSON sidecars remain hash-addressed alongside their raw inputs. Missing
historical download timestamps and licenses are recorded as unknown.

`registry/reproducibility_sources.csv` defines each bespoke source's importer chain,
input selectors, staging destination, environment overrides, access limitations,
license review status and gaps. `input_provenance.csv` maps every raw/manual file
to those profiles and flags unclassified files. `code_inventory.csv` records parser
hashes and literal dependencies; the R scripts remain the transformation specification.
`dependencies.csv`, `external_references.csv`, `git_source_evidence.csv` and
`baseline_comparison.csv` provide inspectable checks. Entries sharing an input
(for example, live and cached OPEC workflows) deliberately share its provenance.

Every `update`, `rebuild` or `replay` creates a new, independently copied workspace
under `data/reproducibility/runs/<run_id>/workspace/`. Existing run IDs are rejected.
An optional successful parent supplies its accumulated data; otherwise the original
baseline supplies the inputs. `update` uses the current importer code and retains
the original baseline plus each revised source vintage; `replay` restores the named
parent's exact code and captured responses. No run automatically promotes results
back into the research baseline. Revisions may change a run's selected/latest view;
older objects and manifests remain available. Updates retain all observations that
the registered source itself supplies; they cannot recover observations never published.
Completed workspace copies may be archived during housekeeping. Parent and replay
restoration uses retained `files.csv` manifests and immutable objects, so it does
not require the parent's old workspace directory.

Run receipts include inputs, outputs and SHA-256 changes, the complete script sequence,
R/package session information, lock hash, configured/inherited source environment,
standard output/error logs and a success/failure result. Successful shared-transport
updates retain original HTTP response bytes, exact requested/final URLs, status,
retrieval time, ETag and Last-Modified when supplied. IMF request URLs are built by
the installed `rsdmx` provider, and World Bank JSON is archived before transformation.
API failures or empty results cannot replace existing extracts. Failed runs remain
inspectable and cannot be parents. Retrieval metadata and generation timestamps can
differ between builds; compare observation values/keys separately from those fields.

Manual ingestion validates readable CSV/JSON/workbook/PDF formats and stages bytes
without changing the supplied file. Images and other evidence are preserved as
bytes; the registered importer validates the reviewed transcription, hashes, units,
keys and cell mappings. Supply a complete reviewed bundle with its updated snapshot
and source manifest where the importer requires them. Ingestion never invents an
upstream URL or automatically transcribes images. A changed workbook with stale
review metadata must fail validation. For a single-file profile, supply exactly one
file; directory profiles retain relative paths. Add reviewed news/transcription
observations while retaining earlier report vintages and explicit user-fill flags.

Some existing downloaders use direct provider clients and have not been connected
to the shared HTTP replay transport. Their update results are still isolated and
snapshotted, but the catalog marks replay unsupported. Cached OPEC and cross-country
rebuilds use their existing offline caches. BCV statement numeric extraction and
press-review evidence have no verified standalone numeric importer. Energy Institute,
`pib_hist.xlsx` and interest-rate workbooks have lossless cell importers; their economic
schemas still require review. Exact upstream origins,
licenses, unavailable OPEC reports, unverified news reproductions, and workbook
provenance remain explicit gaps. This is not a verified fresh-download replication
of every historical input. Forecast fitting is outside this cleanup.

The object store and run workspaces are ignored by Git because `data/` is ignored.
Back up/distribute the object store together with its manifests when access terms
allow; a checkout containing manifests alone cannot recreate unavailable/manual
bytes. The dataset runner publishes current research data through validated staging; use
this historical runner when baseline/parent semantics or exact response replay are required.

Official IMF and World Bank terms reviewed on 2026-10-23 are recorded in
`registry/reproducibility_licenses.csv`; `policies` retains current official HTML
responses and checksums. These policies do not establish a blanket license for
third-party indicators or supplied workbooks. IMF automated bulk downloading is
disabled in this runner because permission is not recorded; existing annual
archives remain preserved and authorized exports can be staged manually.
`source_status.csv` distinguishes supported adapters from actually verified runs;
`checks.csv` and `runs.csv` retain test and build outcomes, including failed attempts.
`verify` also checks every published archive object; `objects` reruns only that
checksum check and writes `objects.csv`. Failed checks raise explicit errors.

## UN Comtrade merchandise trade

Run `Rscript scripts/external_14_trade_comtrade.R config/comtrade.yml resume`
from the repository root. The initial profile requests Venezuela's annual merchandise
exports and imports for 2000-2024, separately as direct world totals and mirror sums
of available reporting partners. It uses the public preview API without credentials.
Each request covers one period, product and flow; reaching the row cap stops the run
instead of accepting a potentially incomplete response. `update` refreshes all queries;
`resume` reuses cached responses and downloads missing queries; `rebuild` uses cached
responses only. Use `update` to retrieve revisions, including newly available data for
queries that previously returned no records. The default profile is registered in
`run_external.R` and the dataset runner; their acquisition mode controls network
use. Other profiles use the explicit profile/mode command above.
Provider quota errors also stop the run; wait for the stated reset before resuming.
Extend `end_period` to include later years without downloading completed queries again.

Copy `config/comtrade.yml` to choose another country, period range, product list, or
monthly frequency. Give separate research profiles distinct `dataset_id` values to
keep their output extracts separate. Products are requested individually and never
summed across HS levels. `HS` uses each reporter's original HS edition; detailed
product codes are not automatically concorded across revisions. Set `access` to
`authenticated` to use a subscription key from `COMTRADE_PRIMARY` in the process
environment or an ignored project `.Renviron`; credentials are sent only in a
redacted request header. The current implementation supports ordinary data queries,
not premium bulk files. Existing project dependencies suffice.

The initial outputs are `data/final/external_14_trade_comtrade_A.csv` and
`data/final/external_14_trade_comtrade_A_observations.csv`. Values are current
USD, not thousands of USD. Mirror exports sum partners' imports from Venezuela;
mirror imports sum partners' exports to Venezuela. Overlapping reporter groups and self-trade
are excluded and retained with exclusion flags in the observations. Each mirror sum
is labelled `available_reporters_only`, with its reporter count; absent responses
remain missing rather than zero. Mirror exports usually reflect import/CIF valuation
and mirror imports usually reflect export/FOB valuation. Direct and mirror values
are never added together, substituted automatically, or reconciled with BCV goods
trade. Source classification, estimation flags, quantities and valuations remain in
the observations where supplied; quantities are not summed across merchandise totals.
Partner coverage can change over time and does not establish completeness of trade.
Code 490 (Other Asia, nes) is retained as a separate customs area, following the
[UN's explanation](https://uncomtrade.org/docs/taiwan-province-of-china-trade-data/),
despite the API reference's group flag.

Original response bytes, SHA-256 hashes, request URLs, safe HTTP metadata, retrieval
times, profile snapshots and successful output vintages are preserved under
`data/raw/comtrade/`. A failed request or validation stops the run before current
extracts are replaced. Back up this ignored data directory along with the outputs;
cached rebuilding uses its request index. Run `Rscript tests/comtrade.R` for offline
validation. See the [UN Comtrade API documentation](https://uncomtrade.org/docs/un-comtrade-api/)
and [mirror-query guidance](https://uncomtrade.org/docs/how-to-retrieve-data-for-world-exports-to-a-country-in-un-comtrade/).

## Domain runners

- `scripts/run_prices.R`
- `scripts/run_monetary.R`
- `scripts/monetary_92_aggregates_graphs.R`: creates standalone weekly monetary-base/liquidity charts from 2020, retaining original observation dates and annual x-axis labels. Real levels use estimated daily headline CPI on each observation date, allocating published monthly inflation by constant daily compounding; the reference is the latest common observation's month. Nominal growth compares against 364 days earlier, using a preceding source observation within seven days only when the exact date is absent. Uses grouped headline CPI when available, otherwise the original headline extract, and the configured report cutoff. Saves exact data in `outputs/data/monetary_92_aggregates.csv`, weekly coverage in `outputs/data/monetary_92_weekly_coverage.csv`, and PNG/PDF in `outputs/figures/`.
- `scripts/run_financial.R`
- `scripts/financial_06_download_statements_bcv.R`: discovers every page of BCV's own financial-statement archive and downloads all linked PDFs separately from banking-system series. The document registry is `registry/bcv_financial_statements.csv`; listing snapshots, content-addressed PDF vintages, current download results, and append-only run receipts live under `data/raw/bcv_financial_statements/`. Valid cached files are reused; set `BCV_STATEMENTS_REFRESH_PDFS=true` to retrieve source revisions while retaining prior PDF hashes. Printed titles and label/filename date discrepancies are preserved for the later R reader; no balance-sheet values are extracted at this stage. Run `Rscript tests/bcv_statements.R` for offline discovery and PDF-validation checks.
- `scripts/run_oil.R`
- `scripts/run_external.R`
- `scripts/external_95_fx_combine.R`: includes intervention USD equivalents as `rate_int` (published bolivar denomination) and `rate_int_adj` (current bolivars per USD) in `data/final/external_fx.csv`. Conversion divides the original Bs/EUR intervention quote by the USD-per-EUR cross-rate derived from BCV SMC ask quotes on the same value date. Missing reference dates and conflicting intervention dates remain missing in the daily series. `data/interim/external_95_fx_intervention_conversion.csv` retains original quotes, conversion inputs, and status for every intervention observation.
- `scripts/external_03_fx_int_bcv.R` and `scripts/external_10_fx_banks_bcv.R`: archive monthly official BCV exports and extract intervention rates (Bs/EUR) and bank buy/sell FX quotes (Bs/USD). The files served as XLS contain HTML tables. The intervention export has a ten-row cap, so its complete HTML listing supplies omitted observations recorded in `export_omissions.csv`. Hash-checked historical months are reused; the latest month and listing are refreshed on every run. Use `BCV_FX_REFRESH_HISTORY=true` with explicit update mode to recheck all months. Download receipts remain under each source's raw directory. Quotes retain their published bolivar denomination; conflicting quotes for the same date and source identifier are retained with `source_key_conflict=true`. These datasets are separate from the consolidated headline USD series.
- `scripts/external_99_fx_report_graphs.R`: after FX consolidation, exports standalone official/unofficial daily rates and the percentage gap with a trailing 30-calendar-day average, using the approved report theme. Reads the recent-history window and cutoff from `config/report.yml` (or `REPORT_CONFIG`). Saves PNG/PDF charts under `outputs/figures/` and plotted values/coverage under `outputs/data/external_99_fx_graph*.csv`. The average requires 30 finite daily values and uses the consolidated pipeline's carried-forward rates.
- `scripts/external_96_bop_goods.R`: creates report-ready annual and quarterly BCV goods exports and imports, with import magnitudes shown as positive values.
- `scripts/run_real.R`
- `scripts/real_91_gdp_quarterly_graphs.R`: exports standardized BCV quarterly year-over-year GDP bars and a combined view of reconstructed, unadjusted quarter-on-quarter and rolling-four-quarter growth. Requires published quarterly, annual and reconstructed GDP inputs. Use `Rscript scripts/run_data.R real_10_gdp_quarterly_bcv rebuild` to prepare the reconstruction and its four growth inputs. Standalone annual charts use `scripts/real_90_gdp_growth_graphs.R` separately. Exact plotted observations are retained in `outputs/data/real_91_gdp_*graph.csv`, with PNG/PDF in `outputs/figures/`; internal quarterly gaps stop generation. Unadjusted quarter-on-quarter changes retain seasonality.
- `scripts/run_fiscal.R`
- `scripts/fiscal_05_components_imf.R`: downloads IMF WEO general-government revenue, expenditure, and net lending/borrowing as percentages of GDP together; failures preserve the existing extract.
- `scripts/fiscal_90_balance_graph.R`: validates the three IMF fiscal components and exports annual revenue as positive navy bars, expenditure as negative red bars, and balance as a yellow line. Uses the configured history start and completed calendar years before the report cutoff year; preserves the IMF values and vintage metadata in `data/final/fiscal_05_components_imf.csv` and the selected observations in `outputs/data/fiscal_90_balance_graph.csv`. PNG/PDF assets are saved in `outputs/figures/`.
- `scripts/run_all.R`
- `scripts/run_validate.R`: validates report-critical outputs after extraction and stops on missing files, schema drift, duplicate keys, invalid values, or stale observations.
- `scripts/convert_smc_manual_workbooks.R`: converts the six legacy BCV SMC workbooks with Windows Excel before a full historical SMC build.
- `scripts/prices_02_inflation_bcv.R`: derives monthly, year-over-year, and calendar-year inflation from the BCV CPI index.
- `scripts/prices_03_cpi_groups_bcv.R` and `scripts/prices_04_inflation_groups_bcv.R`: download the national CPI group table and derive headline/food monthly and year-over-year rates. Food includes non-alcoholic beverages. Both run in `run_prices.R`; set `BCV_CPI_GROUPS_OFFLINE=true` to reprocess the cached workbook. The separate headline extract is preserved and its overlap is audited in `data/interim/prices_03_headline_overlap_check.csv`.
- `scripts/prices_90_inflation_fx_graphs.R`: after price extraction and FX consolidation, exports standalone headline/food year-over-year inflation and a grouped-column monthly inflation/FX comparison. FX changes use complete calendar-month averages of consolidated daily rates; the comparison ends at the latest complete month common to all four series. Uses the report configuration, saves exact plotted data in `outputs/data/prices_90_*`, and exports PNG/PDF to `outputs/figures/`.
- `scripts/report_00_availability.R`: audits monthly, weekly, daily, quarterly, and fiscal input availability by series using `registry/report_availability.csv`; saves latest observations and eligible month-end coverage to `output/reports/data_availability.csv`.
- `scripts/report_02_graphs.R`: runs the standardized domain graph scripts listed in `registry/report_graphs.csv` and writes a checksum receipt for their inputs and exported assets.
- `scripts/report_01_data_layer.R`: assembles validated report variables into a standard long data layer using `registry/report_series.csv`.
- `report/venezuela_macro.qmd`: editable Quarto report source; the report runner renders Word and exports the matching PDF through Microsoft Word.

## Client report build

Chart conventions: use solid lines without point markers unless explicitly requested. Put `Notas` above `Fuentes` in captions. End each y-axis on an explicit labeled break with no upper expansion, but hide all protruding tick marks. Both x- and y-axis lines are solid charcoal (`#2b3033`, `linewidth = 0.35`). Zero is a thinner solid grey reference (`#687179`, `linewidth = 0.25`); when it coincides with the bottom boundary, the axis remains a single visible line. Use palette colors at full opacity. Keep these settings inline in the ggplot scales/layers and use the shared theme for axis styling. Automatic report scales include their upper endpoint explicitly because recalculated pretty breaks can otherwise omit it.

Prefer a single horizontal legend row whenever labels fit at the normal font size; retain wrapping for dense legends or narrow panels. On linear y-axes, round negative lower limits outward to a complete major-tick interval (including at least one full interval below zero when values are negative), rather than ending the axis just below zero at the data minimum.

Restore packages through the setup entry point described above. R and the Quarto CLI must be installed. The template-faithful report runner also requires Microsoft Word on Windows to export PDF from the editable Word document.

Edit `config/report.yml` for the client name, publication date, graph history start, recent inflation/FX window, and executive-table years. The analysis cutoff is automatically the last day of the month before publication, shared by report charts and standardized exporters; sources may end earlier. Keep the analyst prose in the editable files under `report/sections/`; `report/venezuela_macro.qmd` controls their order and output formats, while `R/report_build.R` prepares validated data, figures, and tables. The EMA/Coca-Cola macro module includes weekly monetary indicators and a second inflation/FX page; each page has short analyst blocks and consistently sized charts exported by the domain graph scripts. `report/templates/estevez_format.docx` is an unchanged copy of the client's Word template: preserve its original header/footer artwork, icons, positioning, styles, and distinct first-page settings. Pandoc ignores the reference document's body; the memo cover and closing/signature page are outside this macro module. The executive table follows the narrative summary on its own page and is also exported as CSV. It shows the configured closing year and the next two years; model estimates and projections remain pending.

Run `Rscript scripts/run_report.R` to build the configured client edition from
published final inputs. Use `REPORT_CONFIG` for another configuration. Source
refreshes and GDP reconstruction are handled by the dataset runner before reporting;
`run_all.R` builds data only. A missing required final input must be rebuilt through
its producer rather than silently generated by a graph script.

The report build writes PDF and Word to `output/reports/report/`, standardized PNG/PDF graphs to `outputs/figures/`, and per-variable date coverage to `output/reports/source_coverage.csv`. It also saves the edition's configuration, harmonized data, and a checksum manifest in `output/reports/`. Required sources and individual report variables must pass validation before rendering. The report consumes those exact graph exports; direct Quarto rendering rejects changed inputs or assets. Grouped headline CPI, SMC FX, and the same IMF fiscal components feed the data layer and charts. The cutoff restricts observation dates, not historical source vintages. Set `BCV_MONETARY_OFFLINE=true` to reprocess validated cached weekly workbooks after a separate source download. Forecasts, recovery scenarios, and SENIAT revenue remain explicitly unfinished analyst blocks.

Run `Rscript tests/report_pipeline.R` for offline checks of report date windows and validation failures.

## Metadata Catalogs

- `registry/venezuela_currency_scales.csv`: records Venezuela's three redenominations and conversion factors to the current bolivar scale.
- `scripts/catalog_imf_wb.R`: refreshes World Bank indicator metadata and IMF dataflow, dimension, and code catalogs into `registry/`.
- `scripts/catalog_opec_momr.R`: extends the MOMR inventory from 2001 onward and attempts official archive-link discovery for new or unresolved reports. Registry candidates remain unverified until download and corpus validation succeed.

## OPEC monthly corpus

Run `Rscript scripts/run_oil.R update` to refresh discovery and necessary sources through the dataset runner, validate the corpus and publish the oil datasets. Use `Rscript scripts/run_oil.R rebuild` for an offline cached rebuild. Run `scripts/oil_90_graphs.R` to export the standardized graphs. These views contain monthly observations only; annual, quarterly and year-to-date columns are not mixed into monthly series.

`registry/opec_momr_reports.csv` holds publication months and source URLs. The catalog checks the official latest-report page alongside the archive and includes a published current-month issue when discovery succeeds. Explicit `OPEC_END_DATE` bounds take precedence. The downloader tries dated archive links and the matching latest-report delivery link in a fixed order, and rejects PDFs whose cover month differs from the requested issue; discovery failures remain visible in the audit. Cached validated PDFs are retained when public endpoints are blocked. `OPEC_START_DATE` and `OPEC_END_DATE` restrict downloads only; extraction always validates the full registered corpus through the current month. Archive-page access failures are logged, with URL-convention candidates retained for validated download attempts. New unclassified retrieval or parser failures stop publication; explicitly reviewed `unavailable` reports are retried and remain visible in the audit.

The registered corpus starts in January 2001. The August and October 2026 PDFs were supplied manually and validated; February 2013 remains a documented retrieval gap. The report audit records the current archive coverage. No claim is made about reports before the registered archive. `data/final/oil_13_momr_corpus.csv` preserves monthly report vintages, country/crude identities, raw printed labels and rows, table notes and footnote markers, PDF pages, source URLs, PDF hashes, parser versions and missing-value markers. Its key is `(report_month, source_type, series_id, date)`. `data/final/oil_13_momr_latest.csv` selects the newest published cell for each `(source_type, series_id, date)`, including explicitly missing values. It does not backfill or interpolate them.

Country and crude aliases live in `registry/opec_series_aliases.csv`; unknown labels stop publication for review. Countries follow each report's membership rather than today's membership, and DoC country rows remain separate from aggregate totals. Brent harmonizes the source's Brent/Dated Brent/North Sea Dated labels while preserving the printed identity; Venezuelan BCF-17, Tia Juana Light and Merey remain separate series. Values are preserved as printed: footnotes can qualify their meaning (for example, some 2025 Saudi direct-communication cells describe supply to market). Asterisks are recorded as markers, not assumed to mean provisional data; review `source_table_notes` before extending country comparisons. PDF and text caches support resumable updates and invalidate when source files or parser code change. Valid cached PDFs are reused; set `OPEC_REFRESH_PDFS=true` with a bounded download date range to refresh corrected source issues without replacing a valid PDF with a failed response.

`data/interim/oil_13_momr_coverage.csv`, `oil_13_momr_gaps.csv` and `oil_13_momr_report_audit.csv` distinguish series coverage, missing months/cells and report retrieval/extraction outcomes. Staged audits and OPEC-total reconciliation are in `data/interim/`. Complete production tables must reconcile within independent rounding tolerance. Latest Venezuela direct production has unreported August–October 2014 cells; Merey has no March 2020 observation in the retrieved monthly tables. Graphs leave these gaps open. Run `Rscript tests/opec_corpus.R` for offline parser regression checks. After building the corpus and views, run `Rscript tests/opec_corpus_outputs.R` for source-value and report-contract checks, and `Rscript tests/opec_corpus_rebuild.R` to verify a byte-identical cached rebuild.

## Separate research and methodological scope

The standalone `scripts/oil_14_audit_direct_history.R` compares an external reference workbook (`OPEC_REFERENCE_WORKBOOK`) against all MOMR vintages, scans cached reports for earlier direct-production evidence, and compares 2002–2011 with official JODI monthly data. The audited MOMR monthly direct series begins in January 2012, published in the April 2012 issue. JODI matches 87 of the workbook's 120 observations for 2002–2011; 33 differences in 2007–2009 remain unresolved. Audit comparisons are saved in `data/interim/`; neither the workbook nor the canonical series is modified. JODI's crude definition includes lease condensate, so extending the graph requires source-definition review before combining the series.

Source acquisition, consolidated datasets and labelled derived estimates are
implemented in different domains. Their presence does not establish that every
source is fully consolidated or integrated into the client report. Exploratory FX
forecasts exist in `scripts/external_100_fx_arima.R`; they are separate from source
data and report projections, use their own analysis date, and do not select a
production forecasting method. The methodological review in
`references/forecasting_course_review.md` remains preliminary. New house adjustments,
forecast specifications and scheduled runs require their own defined scope.

`report/monetary_multiplier_base_report.md` is a scoped monetary-essay draft with
fixed historical assumptions and data vintages. Its figures and statements about
the latest available CPI are not current repository-wide policy. Verify inputs
before reusing them. Retired session prompts are indexed in the cleanup catalog.

Reviewed Palma history is published separately by
`scripts/external_15_fx_palma.R` and `scripts/external_16_fx_daily_history.R`;
its workbook and column inventories retain attribution and selection evidence.
Wikipedia wage history is extracted by `scripts/real_13_minimum_wage_wikipedia.R`
and consolidated by `scripts/real_14_minimum_wage_monthly.R`. These sources retain
quality flags and do not automatically replace canonical report inputs.

### Estimated daily and weekly inflation

- `scripts/prices_05_daily_cpi_bcv.R` allocates headline monthly CPI changes over actual calendar days: daily factor = (current CPI / previous CPI)^(1 / days in month). The estimated daily CPI treats published monthly indices as month-end anchors and reconciles exactly to them.
- `scripts/prices_06_weekly_inflation_bcv.R` compounds those daily factors over Saturday-Friday weeks. Cross-month weeks use the appropriate factor for each day. Only complete seven-day weeks are published in `data/final/prices_06_weekly_inflation_bcv.csv`; month contributions and boundary coverage are audited in `data/interim/prices_06_weekly_*.csv`.
- Both run through `scripts/run_prices.R`. These are separately labelled estimates and do not replace published monthly inflation. Monetary observations with irregular spacing retain their actual date intervals, distinct from the fixed seven-day series.
- Offline checks: `Rscript --vanilla tests/weekly_inflation.R` verifies monthly reconstruction, leap-year/month lengths, mixed-month compounding, and daily CPI ratios.

## Continue on another device

For the macro report outline and remaining editorial work, see
[Venezuela macro report: structure and next steps](report/REPORT_STATUS.md).

Inspect the current checkout and `config/report.yml` (or `REPORT_CONFIG`) instead
of relying on a fixed handoff branch or cutoff. `R/report_inputs.R` derives the
cutoff as the last day of the month before the configured publication date.
Source coverage may end earlier; this observation cutoff does not reconstruct
historical publication vintages. Pushing code does not transfer or refresh data.

The repository excludes `data/`, `output/`, `outputs/`, and `tmp/`. Transfer retained
raw evidence, immutable objects, manifests and required manual bundles separately
when the exact local vintage is needed. Existing local data should be reused;
domain downloaders cannot necessarily recover unavailable or supplied inputs.
Install dependencies before rebuilding. Monetary aggregate charts require monetary
outputs and the daily CPI derived from price inputs; inflation/FX charts also
require consolidated FX. Chart scripts save PNG/PDF under `outputs/figures/`.

Daily CPI and Saturday-Friday weekly inflation are reusable derived estimates; published monthly inflation remains separate. For irregular monetary observation intervals, use daily CPI at the actual endpoints. Offline numerical validation: `Rscript --vanilla tests/weekly_inflation.R`.

## Verification

`Rscript tests/pipeline.R` checks zero-download cache reuse, explicit acquisition,
dependency selection, validation failure safety, retained history and grouped
publication using isolated fixtures. Existing domain regression scripts verify
source fidelity, source precedence, monetary conversions, CPI estimates, GDP
reconstruction, Comtrade and report preparation.

`Rscript scripts/verify_data_migration.R <snapshot_directory>` compares current
publication keys and numeric values with the pre-migration snapshot. It writes
`output/validation/data_migration_comparison.csv`; metadata paths, acquisition
labels and explicitly retained exclusions are handled separately. Local preserved
versions and retired artifacts are indexed in the cleanup catalog. Verification
of cached builds does not assert that live provider endpoints are available.
