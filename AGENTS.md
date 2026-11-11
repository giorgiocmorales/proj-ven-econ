# Agent instructions

## Purpose and reading order

This repository is the shared Venezuelan economic data and analysis hub for the
user's consulting work. Build reusable datasets that can serve multiple models,
graphs and client reports.

1. Read [README.md](README.md), especially the data lifecycle, inventories,
   execution modes and implementation gaps.
2. Inspect the relevant source and dataset registry entries, producer scripts,
   helpers, domain runner and consumers before changing a workflow.
3. Check the working-tree diff and preserve existing user changes. Keep changes
   narrowly scoped and uncommitted unless the user explicitly requests a commit.

The README owns project structure and operating procedures. These instructions
own agent behavior. Code and registries establish implemented behavior; reconcile
discrepancies explicitly rather than presenting a proposed procedure as implemented.
Historical handoff prompts, analytical drafts and course reviews apply only to
their stated task and vintage. They do not override repository instructions or
establish current data coverage, model selection or report cutoffs.

## Working with sources and datasets

- Follow the README's source-registration and `raw -> interim -> final` procedure
  for web sources, local folders, supplied files and reviewed transcriptions.
  Reuse existing inventories and stable identifiers before creating new ones.
- Treat an update request as a request for a final dataset and its dependency
  chain. Identify the producing procedure and all required source versions first.
- Reuse valid local raw inputs by default. Retrieve only missing material or
  revisions required by the source-specific refresh procedure. A parser change,
  new graph or model rerun does not require another download.
- Inspect actual downloader behavior before execution: a domain runner or an
  `update` command is not automatically incremental or cache-only. Use supported
  cached modes when appropriate; never silently substitute stale data for a
  requested refresh or claim an offline rebuild when network access occurs.
- Preserve original files and earlier vintages. Read external folders without
  modifying them; process the archived copy. Never invent source attribution,
  acquisition dates, units, licenses or missing observations.
- Keep source selection, monetary conversions, frequency changes, estimates and
  conflict resolutions explicit. Retain original values and evidence. Never
  silently splice incompatible price bases, geographies, monetary regimes or
  economic definitions; never treat missing observations as zero.
- New analysis consumers should read validated final datasets. Follow the
  documented compatibility mapping when replaying historical `data/processed/`
  snapshots. Active producers and consumers use final/interim paths.
- Never hand-edit final observations. Correct the underlying evidence or
  transformation, preserve its provenance, and regenerate affected outputs.
- Keep client windows, chart formatting and model choices downstream of reusable
  data. A new research dataset must not become a prerequisite for an unrelated
  dataset or report merely because it appears in an inventory.

## Pipeline changes and validation

- Group scripts by the existing domain prefixes. Preserve established paths and
  dataset identifiers unless a coordinated migration is part of the request.
  Script numbers identify files; runner calls and input dependencies define order.
- Declare inputs, outputs, refresh behavior and relevant parameters in producer
  code comments/docstrings and existing registries. Keep one authoritative
  producer for each published dataset; separate source acquisition, preparation,
  publication and downstream analysis where the existing workflow permits it.
- Use `scripts/run_data.R <dataset_id|domain|all> <rebuild|resume|update> [failed_run_id]` for
  ordinary data publication. Its dependency inventory is `registry/pipeline_steps.csv`;
  link source profiles and dataset producers there and in the dataset inventory.
  Use the smallest relevant target. Domain runners and `run_all.R` use the same
  staged publication path; report rendering is a separate command.
- Internal producer scripts can write immediately when invoked directly. Use
  them in isolated workspaces or inspected regression tests; ordinary updates
  must pass through the dataset runner's validation and recoverable publication.
- Validate schema, keys, dates, units, finite values, coverage and applicable
  accounting identities before publishing final data. Check joins for duplication
  and loss. Retain explicit missingness, conflicts and source-quality flags.
- Preserve the previous valid final version until its replacement passes checks.
  Retain input versions, configuration, code identity and validation evidence
  sufficient to explain or reproduce a published version.
- Distinguish a received source, a parsed source, validated reusable data and
  report integration. Record readiness and remaining gaps in existing registry
  status/notes fields; do not declare every imported source ready for analysis.
- Source provenance and processing validation do not establish economic
  comparability. Observations, user-supplied fills, reconstructions, estimates,
  forecasts and scenario assumptions must remain distinguishable.
- Run checks appropriate to the affected workflow. Inspect test side effects
  before running rebuild suites. Report what was verified, any changed output
  versions and unresolved limitations. Do not claim a fresh-source replication
  from a cached rebuild or infer real-time vintages from observation-date filters.
- Update applicable registry entries and the README when interfaces or operating
  procedures change. Do not create duplicate inventories or session changelogs.

## Code and dependency conventions

- Check for existing logic and package functionality before implementing code.
  Follow DRY, KISS and YAGNI; match repository style and keep the smallest useful
  diff. Avoid unrelated refactoring of legacy code.
- Write code comments in English. Clearly comment each code section/chunk for
  audit; keep transformation details in the code that performs them.
- Use tidyverse conventions and `%>%` as the primary R pipe. Prefer package
  functions and pipelines over unnecessary custom functions.
- Do not introduce default parameter values in function definitions. Make all
  parameters explicit and pass them explicitly at call sites.
- Raise explicit, specific errors. Do not suppress failures or use catch-all
  handlers that hide their cause. Failed retrievals or validation must not
  replace valid datasets with empty or partial results.
- Manage dependencies in the project environment and configuration, including
  `DESCRIPTION` and `renv.lock` where applicable. Never install project
  dependencies globally or rely on an unrecorded one-off installation. Read
  installed package source when needed rather than guessing its behavior.
- Prefer code docstrings/comments for implementation documentation. Keep each
  broader concept in one authoritative place and link to it from other documents.
  Describe current state, scope and limitations rather than change history.
- Never create a Git commit without an explicit user request. When integration
  is requested, prefer merge over squash unless the user requests squash.
