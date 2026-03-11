---
date: 2026-03-04
title: "Fix version filtering: remove module from grouping"
status: active
brainstorm: "docs/brainstorms/2026-03-04-fix-version-filtering-module-grouping.md"
language: "R"
estimated-effort: "small"
tags: [bug-fix, pip_find_data, version-filtering]
---

# Plan: Fix version filtering — remove module from grouping

## Objective

Remove `module` from the `by` clause in the version-filtering steps of
`pip_find_data()` so that `vermast`/`veralt` maximums are computed globally
per country-year-survey, preventing duplicate rows when different modules
exist under different versions (e.g., ZAF 2008).

## Context

The brainstorm (`docs/brainstorms/2026-03-04-fix-version-filtering-module-grouping.md`)
decided on Approach 1: simply remove `module` from the `by` grouping. The
`module` and `source` columns remain on every row as regular columns. The
downstream `pip_keep_pc_source()` logic continues to handle multi-source
deduplication when the max version has more than one source.

Existing tests in `tests/testthat/test-pip_find_data.R` cover basic
argument validation but not the version-filtering logic itself.

## Implementation Steps

### 1. Fix PC version filtering (lines 261–262, 268–269)

- **File**: `R/pip_find_data.R`
- **Details**: Change both `by` clauses in the `filter_to_pc` block from
  `by = .(country_code, surveyid_year, survey_acronym, module)` to
  `by = .(country_code, surveyid_year, survey_acronym)`
- **Acceptance criteria**: ZAF 2008 with `filter_to_pc = TRUE` returns
  only the V02/GPWG row, not both V01/HIST and V02/GPWG.

### 2. Fix TB version filtering (lines 357–358, 363–364)

- **File**: `R/pip_find_data.R`
- **Details**: Same change in the `filter_to_tb` block — remove `module`
  from both `by` clauses.
- **Acceptance criteria**: TB filtering uses global version comparison,
  same as PC.

### 3. Add regression test

- **File**: `tests/testthat/test-pip_find_data.R`
- **Details**: Add a test that constructs a minimal `data.table` mimicking
  the ZAF 2008 scenario (two rows, same country-year-survey, different
  vermast, different module) and verifies only the highest-version row
  survives after filtering. This test should not depend on the PIP network
  drive.
- **Tests**:
  - One source per version, different modules → only max version row kept
  - Same version, two sources → `pip_keep_pc_source()` picks highest priority
  - All columns (`module`, `source`, `filename`, etc.) preserved in output
- **Acceptance criteria**: Tests pass with `devtools::test()`.

### 4. Manual verification

- **Details**: Run `pip_find_data(country = "ZAF", year = 2008, filter_to_pc = TRUE)`
  interactively and confirm a single row is returned.
- **Acceptance criteria**: One row, `vermast == "V02"`, `source == "GPWG"`.

## Testing Strategy

- **Unit tests**: Mock inventory data as a `data.table` to test the filtering
  logic without network access. Cover:
  1. Single version, single source (no-op case)
  2. Multiple versions across modules (the bug case)
  3. Same version, multiple sources (source-dedup case)
  4. All output columns preserved
- **Integration**: Manual check with real ZAF 2008 data on the PIP drive.

## Documentation Checklist

- [ ] Inline comments updated in the `by` clause to explain why `module` is excluded
- [ ] No roxygen2 changes needed (function signature unchanged)
- [ ] No README changes needed

## Risks & Mitigations

| Risk | Mitigation |
|---|---|
| Edge case where a country-year-survey intentionally needs per-module versioning | Not a real scenario per user confirmation; `pip_keep_pc_source()` handles multi-source at same version |
| Breaking downstream consumers that expect duplicate rows | No known consumers depend on duplicates; duplicates cause crashes |

## Out of Scope

- Refactoring into a reusable `filter_max_version()` helper (Approach 3)
- Fixing the deprecated `pip_find_dlw()` replacement (separate concern)
- Changes to `pip_keep_pc_source()` logic or source priority order
