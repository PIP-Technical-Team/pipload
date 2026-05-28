---
date: 2026-05-28
title: "Rewrite pip_inv_enrich with direct qs2 bulk loading and wide-column expansion"
status: active
scope: "Standard"
brainstorm: ".cg-docs/brainstorms/2026-05-28-pip-inv-enrich-fast-bulk-loading.md"
language: R
estimated-effort: medium
phases: 2
tags: [performance, metadata, inventory, pip_inv_enrich, data.table]
---

# Plan: Rewrite pip_inv_enrich with direct qs2 bulk loading

## Objective

Replace the slow per-row `pip_read()` loop in `pip_inv_enrich()` with direct
`qs2::qs_read()` using versioned file paths already present in the inventory.
Add correct wide-column expansion for named-vector metadata fields (cpi, ppp,
pop, gdp, pce) with smart prefix handling.

## Context

- Current implementation: `pip_read()` per row → stamp version resolution
  overhead → ~90ms/file × 4146 = ~6 minutes.
- Direct `qs2::qs_read()` = ~5ms/file → ~20s total (18x improvement).
- The inventory already has `path_metadata` and `version_id_metadata` columns.
- Metadata is an R list with 26 elements: 20 scalar fields + 1 character
  vector (`names`, `class`) + 5 named numeric vectors (`cpi`, `ppp`, `pop`,
  `gdp`, `pce`).
- `qs2` is already in `Imports`.

## Requirements

| ID  | Requirement                                                          | Source    |
|-----|----------------------------------------------------------------------|-----------|
| R1  | Use direct `qs2::qs_read()` with versioned paths from inventory      | brainstorm |
| R2  | NA version_id_metadata → NA for all fields (no fallback to latest)   | brainstorm |
| R3  | Document that NA version_id_metadata should be investigated          | brainstorm |
| R4  | Scalar fields → single column per field                              | brainstorm |
| R5  | `cpi` → wide columns `cpi_YYYY_area` (prefix with `cpi_`)           | brainstorm |
| R6  | `ppp` → wide columns using element names as-is (already prefixed)    | brainstorm |
| R7  | `pop`/`gdp`/`pce` → strip `YYYY_` prefix, keep area: `pop_area`     | brainstorm |
| R8  | Avoid prefix doubling (e.g. no `ppp_ppp_...`)                        | brainstorm |
| R9  | Skip fields already present as columns in `inv` (no overwrite)       | brainstorm |
| R10 | Warn listing pip_ids where all requested fields are NA               | existing  |
| R11 | Sequential `lapply` (no parallel deps); future parallelism is roadmap idea | brainstorm |

## Implementation Steps

## Phase 1: Core implementation

### 1. Rewrite `pip_inv_enrich()` core logic

- **Requirements**: R1, R2, R9, R10, R11
- **Files**: `R/pip_inv_enrich.R`
- **Details**:
  1. Remove the `extract_one()` inner function and `pip_read()` loop.
  2. Construct versioned file paths: `path_metadata/versions/version_id_metadata/artifact`.
  3. Require `path_metadata` column in `inv`; abort if missing.
  4. Mark rows with `is.na(version_id_metadata)` as unfetchable upfront.
  5. Bulk-read all fetchable rows via `lapply(paths, \(p) tryCatch(qs2::qs_read(p), error = \(e) NULL))`.
  6. Before expanding, check which `fields` already exist in `inv` names — skip those with an informational message.
  7. Remove the `joyn::left_join` — use data.table merge or direct `:=` assignment instead (removes joyn dependency for this function).
- **Test Scenarios**:
  - ✅ Happy path: scalar field extraction from 3+ rows
  - 🛑 Edge case: `path_metadata` column missing → informative abort
  - 🛑 Edge case: all version_id_metadata are NA → all fields NA + warning
  - ❌ Error path: file exists but is corrupted → row gets NA, warning emitted
- **Acceptance criteria**: Enrichment of 50 rows completes in < 1s (vs ~4.5s before).

### 2. Implement field expansion logic (scalar vs vector)

- **Requirements**: R4, R5, R6, R7, R8
- **Files**: `R/pip_inv_enrich.R` (internal helper)
- **Details**:
  Create an internal helper `expand_meta_field(field_name, value)` that returns
  a named list suitable for data.table column assignment:
  - **Scalar** (length 1, no names): `list(<field_name> = value)`
  - **`cpi`**: prefix each element name with `cpi_` → `cpi_2005_rural`, `cpi_2011_urban`
  - **`ppp`**: use element names as-is (they already start with `ppp_`)
  - **`pop`/`gdp`/`pce`**: strip leading `YYYY_` regex pattern (`^\\d{4}_`),
    prefix with field name → `pop_rural`, `gdp_national`, `pce_urban`
  - **General named vector** (not in special set): prefix with `field_` + element name
  - **Unnamed vector** (length > 1, like `class`, `names`): store as list-column
    with a warning that it cannot be expanded to wide format
  - **Prefix dedup**: before prefixing, check if element name already starts
    with `tolower(field_name)` followed by `_` — if so, use as-is.
- **Test Scenarios**:
  - ✅ `cpi` with names `c("2005_rural", "2011_urban")` → cols `cpi_2005_rural`, `cpi_2011_urban`
  - ✅ `ppp` with names `c("ppp_2011_02_02_national")` → col `ppp_2011_02_02_national` (no doubling)
  - ✅ `pop` with names `c("2022_rural", "2022_national")` → cols `pop_rural`, `pop_national`
  - ✅ scalar `reporting_level = "2"` → col `reporting_level`
  - 🛑 `class` (unnamed length-3 vector) → list-column + warning
- **Acceptance criteria**: Unit tests pass for all five field type categories.

### 3. Assemble expanded columns into inventory

- **Requirements**: R1, R2, R4–R9
- **Files**: `R/pip_inv_enrich.R`
- **Details**:
  1. For each row, call `expand_meta_field()` on each requested field from the
     loaded metadata list.
  2. Collect all row-expansions into a list of named lists.
  3. Use `data.table::rbindlist(..., fill = TRUE)` to handle varying column
     sets (surveys with different area breakdowns get NA for missing areas).
  4. Bind `pip_id` as key column.
  5. Merge onto `inv` via `inv[field_dt, on = "pip_id"]` or `:=` with match.
- **Test Scenarios**:
  - ✅ Two surveys with different `pop` areas → union of columns, NAs filled
  - ✅ Mixed scalar + vector fields in one call
  - 🛑 Survey with NULL for a requested field → NA columns
- **Acceptance criteria**: Full integration test with mixed fields returns
  correct wide data.table.

## Phase 2: Tests, docs, and polish

### 4. Update roxygen2 documentation

- **Requirements**: R2, R3, R9
- **Files**: `R/pip_inv_enrich.R`
- **Details**:
  - Update `@param inv` to note that `path_metadata` column is required.
  - Update `@param fields` to document the three categories (scalar, prefixed
    vector, year-stripped vector).
  - Update `@return` to describe wide-column output and NA semantics.
  - Add `@note` about NA `version_id_metadata` meaning missing metadata that
    should be investigated.
  - Update `@details` to explain the direct qs2 fast path (no stamp overhead).
  - Remove references to `joyn::left_join`.
- **Acceptance criteria**: `devtools::document()` runs clean; `?pip_inv_enrich`
  shows updated help.

### 5. Update and expand tests

- **Requirements**: All
- **Files**: `tests/testthat/test-pip_inv_enrich.R`
- **Details**:
  - Replace `pip_read` mocks with `qs2::qs_read` mocks (or use temp files
    written with `qs2::qs_save()`).
  - Add tests for:
    - Vector field expansion (cpi, ppp, pop/gdp/pce)
    - Prefix dedup (ppp names not doubled)
    - Year stripping for pop/gdp/pce
    - Skip-existing-columns behavior
    - NA version_id_metadata → NA fields
    - `path_metadata` column missing → abort
    - Mixed scalar + vector fields
  - Keep existing tests for basic scalar extraction (update mocking strategy).
- **Acceptance criteria**: `devtools::test()` passes; coverage on
  `pip_inv_enrich.R` ≥ 90%.

## Testing Strategy

- **Unit tests**: Mock file reads using `withr::local_tempdir()` + `qs2::qs_save()`
  to write known metadata lists, then test `pip_inv_enrich()` against them.
- **Integration test**: One test that reads from the real network path (marked
  `skip_on_ci()`) to confirm end-to-end performance.
- **Edge-case coverage**: NA versions, corrupted files, missing fields, unnamed
  vectors, prefix collisions.

## Documentation Checklist

- [ ] Function roxygen2 documentation updated
- [ ] `@examples` updated to show vector field usage
- [ ] Inline comments for prefix dedup logic
- [ ] NEWS.md entry for performance improvement

## Risks & Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Stamp file layout changes (`versions/<id>/artifact`) | Function breaks silently | Pin test against known layout; add defensive check that path exists before reading |
| `path_metadata` column missing in older inventories | Abort on legacy data | Provide clear error message pointing to `setup_working_release()` |
| Very wide result when requesting all vector fields | Memory/display issues | Document behavior; user chooses which fields to request |

## Out of Scope

- Parallelism (roadmapped as separate idea)
- Pre-computed metadata summary artifact (pipeline-side change)
- Caching/memoization of loaded metadata
- Changes to `pip_read()` itself
- Supporting derived/computed fields not in the metadata list
