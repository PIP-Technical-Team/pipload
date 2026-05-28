---
date: 2026-05-28
title: "Rewrite pip_inv_enrich with direct qs2 bulk loading and wide-column expansion"
status: completed
completed-date: 2026-05-28
completed-phases: [1, 2]
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-05-28-pip-inv-enrich-fast-bulk-loading.md"
language: R
estimated-effort: medium
phases: 2
tags: [performance, metadata, inventory, pip_inv_enrich, data.table]
---

# Plan: Rewrite pip_inv_enrich with direct qs2 bulk loading (v3 — final)

## Objective

Replace the slow per-row `pip_read()` loop in `pip_inv_enrich()` with direct
`qs2::qs_read()` using versioned file paths already present in the inventory.
Add correct wide-column expansion for named-vector metadata fields (cpi, ppp,
pop, gdp, pce) with smart prefix handling, year-aware stripping, and strict
whitelist validation.

## Context

- Current implementation: `pip_read()` per row → stamp version resolution
  overhead → ~90ms/file × 4146 = ~6 minutes.
- Direct `qs2::qs_read()` = ~5ms/file → ~20s total (18x improvement).
- The inventory already has `path_metadata` and `version_id_metadata` columns.
- Metadata is an R list with 26 elements. Only 14 fields are valid for
  enrichment (see R13).
- `qs2` and `joyn` are already in `Imports`.

### Changes from v2

- **Added R13**: Whitelist validation — only 14 specific fields are allowed.
- **Removed**: Unnamed vector handling (class/names can never be requested).
- **Removed**: General named vector fallback (all named vectors are in the
  special set: cpi, ppp, pop, gdp, pce).
- **Simplified** `expand_meta_field()` to only handle scalars + the 5 known
  vector fields.

## Requirements

| ID  | Requirement                                                          | Source    |
|-----|----------------------------------------------------------------------|-----------|
| R1  | Use direct `qs2::qs_read()` with versioned paths from inventory      | brainstorm |
| R2  | NA version_id_metadata → NA for all fields (no fallback to latest)   | brainstorm |
| R3  | Document that NA version_id_metadata should be investigated          | brainstorm |
| R4  | Scalar fields → single column per field                              | brainstorm |
| R5  | `cpi` → wide columns `cpi_YYYY_area` (prefix with `cpi_`)           | brainstorm |
| R6  | `ppp` → wide columns using element names as-is (already prefixed)    | brainstorm |
| R7  | `pop`/`gdp`/`pce` → strip `YYYY_` only when year matches `surveyid_year`, keep area: `pop_area` | review |
| R7b | If pop/gdp/pce year differs from surveyid_year, keep full name and add `<field>_year` column | review |
| R8  | Avoid prefix doubling (e.g. no `ppp_ppp_...`)                        | brainstorm |
| R9  | Skip fields already present as columns in `inv` (no overwrite)       | brainstorm |
| R10 | Warn listing pip_ids where all requested fields are NA               | existing  |
| R11 | Sequential `lapply` (no parallel deps)                               | brainstorm |
| R12 | Use `joyn::left_join` for the final merge                            | review |
| R13 | Validate `fields` against whitelist; abort with informative error listing valid options if invalid field requested | review |

### Valid fields whitelist (R13)

Scalars: `gd_type`, `reporting_level`, `ppp_data_level`, `cpi_data_level`,
`gdp_data_level`, `pce_data_level`, `pop_data_level`, `aux_data_levels`,
`distribution_type`

Vectors: `cpi`, `ppp`, `pop`, `gdp`, `pce`

## Implementation Steps

## Phase 1: Core implementation

### 1. Rewrite `pip_inv_enrich()` core logic

- **Requirements**: R1, R2, R9, R10, R11, R12, R13
- **Files**: `R/pip_inv_enrich.R`
- **Details**:
  1. Define the whitelist as a package-internal constant (in `R/pip_inv_enrich.R`
     or `R/aaa.R`).
  2. Validate `fields` against whitelist upfront; abort via `cli::cli_abort()`
     listing valid options if any invalid field is passed.
  3. Remove the `extract_one()` inner function and `pip_read()` loop.
  4. Require `path_metadata` column in `inv`; abort if missing.
  5. Construct versioned file paths: `path_metadata/versions/version_id_metadata/artifact`.
  6. Mark rows with `is.na(version_id_metadata)` as unfetchable upfront.
  7. Bulk-read all fetchable rows via `lapply(paths, \(p) tryCatch(qs2::qs_read(p), error = \(e) NULL))`.
  8. Before expanding, check which `fields` already exist in `inv` names —
     skip those with `cli::cli_inform()`.
  9. Keep `joyn::left_join` for the final merge.
- **Test Scenarios**:
  - ✅ Happy path: scalar field extraction from 3+ rows
  - 🛑 Edge case: `path_metadata` column missing → informative abort
  - 🛑 Edge case: all version_id_metadata are NA → all fields NA + warning
  - ❌ Error path: invalid field name → abort with valid options listed
  - ❌ Error path: file corrupted → row gets NA, warning emitted
- **Acceptance criteria**: Enrichment of 50 rows completes in < 3s; invalid
  field request aborts immediately with helpful message.

### 2. Implement field expansion helper

- **Requirements**: R4, R5, R6, R7, R7b, R8
- **Files**: `R/pip_inv_enrich.R` (internal helper `expand_meta_field()`)
- **Details**:
  Create an internal (non-exported) helper `expand_meta_field(field_name, value, surveyid_year)`:
  - **Scalar** (the 9 scalar fields): `list(<field_name> = value)`
  - **`cpi`**: prefix each element name with `cpi_` → `cpi_2005_rural`
  - **`ppp`**: use element names as-is (already start with `ppp_`);
    dedup check: if name starts with `ppp_`, don't prepend again
  - **`pop`/`gdp`/`pce`**:
    - Parse year from each element name (leading `^\d{4}_`)
    - If year matches `surveyid_year`: strip year, prefix with field →
      `pop_rural`, `gdp_national`
    - If year differs: keep full element name prefixed with field →
      `pop_2019_national`; also include `<field>_year = <mismatched_year>`
  - No other cases exist (whitelist ensures this).
- **Test Scenarios**:
  - ✅ `cpi` + names `c("2005_rural", "2011_urban")` → `cpi_2005_rural`, `cpi_2011_urban`
  - ✅ `ppp` + names `c("ppp_2011_02_02_national")` → `ppp_2011_02_02_national` (no doubling)
  - ✅ `pop` + names `c("2022_rural", "2022_national")`, surveyid_year=2022 → `pop_rural`, `pop_national`
  - ✅ `pop` + names `c("2019_national")`, surveyid_year=2022 → `pop_2019_national` + `pop_year = 2019`
  - ✅ scalar `reporting_level = "2"` → `reporting_level`
- **Acceptance criteria**: Unit tests pass for all five expansion categories.

### 3. Assemble expanded columns into inventory

- **Requirements**: R1, R2, R4–R9, R12
- **Files**: `R/pip_inv_enrich.R`
- **Details**:
  1. For each row, call `expand_meta_field()` on each requested field from the
     loaded metadata list (passing `surveyid_year` from the metadata itself).
  2. Collect all row-expansions into a list of named lists (one list per row).
  3. Use `data.table::rbindlist(..., fill = TRUE)` to handle varying column
     sets (surveys with different area breakdowns get NA for missing areas).
  4. Bind `pip_id` as key column.
  5. Merge onto `inv` via `joyn::left_join(inv, field_dt, by = "pip_id",
     relationship = "many-to-one", reportvar = FALSE, verbose = FALSE)`.
- **Test Scenarios**:
  - ✅ Two surveys with different `pop` areas → union of columns, NAs filled
  - ✅ Mixed scalar + vector fields in one call
  - 🛑 Survey with NULL for a requested field → NA columns
- **Acceptance criteria**: Full integration test with mixed fields returns
  correct wide data.table with no duplicate rows.

## Phase 2: Tests, docs, and polish

### 4. Update roxygen2 documentation

- **Requirements**: R2, R3, R9, R13
- **Files**: `R/pip_inv_enrich.R`
- **Details**:
  - Update `@param inv` to note that `path_metadata` column is required.
  - Update `@param fields` to list all 14 valid fields explicitly.
  - Update `@return` to describe wide-column output and NA semantics.
  - Add `@note` about NA `version_id_metadata` meaning missing metadata that
    should be investigated.
  - Update `@details` to explain: direct qs2 fast path, vector expansion
    rules (cpi/ppp prefix, pop/gdp/pce year-stripping), and skip-existing
    behavior.
- **Acceptance criteria**: `devtools::document()` runs clean; `?pip_inv_enrich`
  shows updated help with the full whitelist.

### 5. Update and expand tests

- **Requirements**: All
- **Files**: `tests/testthat/test-pip_inv_enrich.R`
- **Details**:
  - Use temp files written with `qs2::qs_save()` and construct `inv` with
    `path_metadata` pointing to the temp directory structure.
  - Add tests for:
    - Whitelist validation (invalid field → abort with message)
    - Vector field expansion (cpi, ppp, pop/gdp/pce)
    - Prefix dedup (ppp names not doubled)
    - Year stripping for pop/gdp/pce (matching year)
    - Year mismatch → full name + `<field>_year` column
    - Skip-existing-columns behavior
    - NA version_id_metadata → NA fields
    - `path_metadata` column missing → abort
    - Mixed scalar + vector fields
  - Keep existing tests for basic scalar extraction (update mocking strategy).
- **Acceptance criteria**: `devtools::test()` passes; coverage on
  `pip_inv_enrich.R` ≥ 90%.

## Testing Strategy

- **Unit tests**: Use `withr::local_tempdir()` + `qs2::qs_save()` to write
  known metadata lists to temp files; construct `inv` with `path_metadata`
  pointing to the temp directory structure (mimicking stamp layout:
  `dir/versions/<vid>/artifact`).
- **Integration test**: One test that reads from the real network path (marked
  `skip_on_ci()`) to confirm end-to-end performance.
- **Edge-case coverage**: NA versions, corrupted files, missing fields,
  prefix collisions, year mismatches.

## Documentation Checklist

- [ ] Function roxygen2 documentation updated (whitelist listed, all params)
- [ ] `@examples` updated to show vector field usage
- [ ] Inline comments for prefix dedup and year-stripping logic
- [ ] NEWS.md entry for performance improvement

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Stamp file layout changes (`versions/<id>/artifact`) | Function breaks silently | Defensive path-exists check before reading; test against known layout |
| `path_metadata` column missing in older inventories | Abort on legacy data | Clear error message pointing to `setup_working_release()` |
| Very wide result when requesting all vector fields | Memory/display issues | Document behavior; user chooses which fields to request |
| Year-mismatch in pop/gdp/pce (future pipeline change) | Extra columns appear | Handled by design: full name kept + year column added |

## Out of Scope

- Parallelism (roadmapped as separate idea)
- Pre-computed metadata summary artifact (pipeline-side change)
- Caching/memoization of loaded metadata
- Changes to `pip_read()` itself
- Supporting fields not in the whitelist
- Skip-existing detection for expanded column name patterns (deferred)
