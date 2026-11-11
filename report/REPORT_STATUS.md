# Venezuela macro report: structure and next steps

Review snapshot: **2026-11-10**. This is an internal handoff. The checked-in
[edition config](../config/report.yml) still sets publication to **2026-10-07**,
which gives an observation cutoff of **2026-09-30**. Set the intended edition
before building a new report.

The [Quarto draft](venezuela_macro.qmd) already has the seven sections below.
The existing charts are wired into the draft, but most narrative blocks are
still prompts for the analyst. The last column is the work still to do.

| Section | In the draft | To finish |
| --- | --- | --- |
| [1. Resumen ejecutivo](sections/executive_summary.qmd) | Two narrative slots, client implications, and an indicator table on its own page. | Write the summary last: what changed and what it means for demand, costs, pricing, and access to FX. Keep estimate and forecast cells blank until results are available. |
| [2. Crecimiento y PIB](sections/growth.qmd) | Aggregate GDP: unadjusted quarter-on-quarter growth and rolling four-quarter growth. | Add a compact BCV view by economic activity to show which sectors grew or contracted. Keep the GDP-per-capita recovery scenario for an annex once the benchmark and assumptions are set. The draft still contains its original scenario placeholder. |
| [3. Inflación y tipo de cambio](sections/inflation_exchange_rate.qmd) | Two pages: headline and food inflation with official and parallel FX trends, then monthly changes and the FX gap. | Write the interpretation using the latest complete month common to the monthly comparisons. |
| [4. Indicadores monetarios](sections/monetary.qmd) | Weekly monetary base and liquidity: real balances and nominal year-over-year growth. | Explain how real balances and nominal growth moved together or diverged. |
| [5. Petróleo](sections/oil.qmd) | Production, benchmark prices, and monthly crude and fuel exports. | Explain the implications for FX generation and note where the series end in different months. |
| [6. Sector externo y disponibilidad de divisas](sections/external.qmd) | Goods exports and imports; placeholder for monthly FX amounts. | Add monthly FX sales, keeping BCV observations, allocations based on Ecoanalítica monthly shares, and the workbook extension distinguishable. Use the source's sales definition and flag changes in scope. |
| [7. Balance fiscal y recaudación](sections/fiscal.qmd) | Annual IMF revenue, expenditure, and balance; placeholder for SENIAT. | Add a separate monthly panel for nominal SENIAT collections. If showing USD equivalents at the official and parallel rates, label each conversion and preserve the source and estimate flags. |

## Where to resume

1. Review the three panel additions in sections 2, 6, and 7. Their reusable
   dataset IDs are `real_10_gdp_quarterly_bcv`, `external_13_fx_sales_monthly`,
   and `fiscal_08_seniat_consolidated_monthly`. Check the
   [dataset inventory](../registry/series_registry.csv) and the published files
   for their coverage in the intended edition. The three panels are not yet
   wired into the [report chart registry](../registry/report_graphs.csv).
2. Integrate the panels from validated final inputs, finish the narrative, and
   write the executive summary after reviewing the charts. Recovery scenarios
   and forecasts can follow separately from the baseline.
3. Follow the [report build instructions](../README.md#client-report-build)
   when ready to render. GitHub has the source and this handoff; exact input
   vintages and generated Word/PDF files need a separate transfer. See
   [Continue on another device](../README.md#continue-on-another-device).
