# Project Handoff

## Completed in this session

- Repaired all four BCV GDP importers for the current `Var_punt%` worksheet.
- Reworked the BCV SMC importer to cache raw files, refresh only the latest
  quarter after the first build, use bounded retries, and checkpoint a full
  history build in `data/processed/external_01_fx_smc_bcv.building.csv`.
- Converted the six legacy SMC workbooks in `data/manual_fix/` to readable
  `.xlsx` files locally. These files are ignored by Git and must be recreated
  or supplied on another computer before a full SMC build can complete.
- Repaired the April 2026 OPEC registry entry to use the official PDF.
- Added `scripts/catalog_opec_momr.R`, which creates OPEC MOMR archive
  candidates from 2001 onward without activating unvalidated reports.
- Extended the OPEC production locator to accept the historical `OPEC crude
  oil production` heading and corrected production annual-period labels to be
  derived from the report date.
- Made OPEC appendix downloads optional for the PDF-based production and price
  extractors.

## Current local data state

Local data directories are intentionally ignored by Git. The latest verified
outputs produced in this session include:

- BCV annual GDP: 2008-2025.
- BCV quarterly GDP: 2008 Q1-2026 Q1.
- OPEC monthly Venezuelan production: February 2024-March 2026.
- OPEC monthly Merey price: March 2024-March 2026.
- The SMC full-build checkpoint completed 2020 Q1-2021 Q2 before the local
  run was stopped. The checkpoint will not transfer with this commit.

## Next work, in priority order

1. Run or resume the SMC historical build until it creates
   `data/processed/external_01_fx_smc_bcv.csv`. On a fresh checkout, first
   recreate the six converted `data/manual_fix/*.xlsx` workbooks from their
   corresponding `.xls` originals, or update the importer with a portable
   conversion approach.
2. Complete the OPEC historical layout audit with one or two reports per year.
   The 2020 and 2023 production tables were confirmed to use the legacy
   `OPEC crude oil production` heading. Activate only reports that pass both
   country-level production and Merey price extraction checks.
3. Replace the Venezuela-only OPEC row parser with a table parser that emits
   monthly production for every country and both `secondary_sources` and
   `direct_communication` series.
4. Map the exact BCV balance-of-payments series identifiers for exports,
   imports, and the current account.
5. Standardize monthly, year-over-year, and calendar-year inflation from the
   BCV CPI level.
6. Add source contracts: required columns, key uniqueness, units, plausible
   ranges, and freshness thresholds. Report-critical failures must stop a run.
7. Add the report data layer, `targets`, Quarto template, tests, and `renv`
   lockfile described in `REPORT_PIPELINE.md`.

## Validation before use

- OPEC processed outputs retain overlapping report vintages by design. Use the
  `data/final/*_latest.csv` outputs for one observation per series/date.
- Do not activate all OPEC archive candidates merely because the PDF URL
  exists. Historical table structures vary and must be validated per layout.
