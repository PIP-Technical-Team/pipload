---
date: 2026-03-04
title: "Fix version filtering: remove module from grouping"
status: decided
chosen-approach: "Remove module from by clause"
tags: [bug-fix, pip_find_data, version-filtering, data-dedup]
---

# Fix version filtering: remove module from grouping

## Context

`pip_find_data()` with `filter_to_pc = TRUE` (and `filter_to_tb = TRUE`) groups
by `.(country_code, surveyid_year, survey_acronym, module)` when computing the
max `vermast` and `veralt`. This means versions are compared *within* each
module separately, which can produce duplicate country-year-survey rows when
different modules exist under different versions.

**Example**: ZAF 2008 has:
- `ZAF_2008_LCS_v01_M_v01_A_PIP_PC-HIST.dta` (vermast v01, source HIST)
- `ZAF_2008_LCS_V02_M_V01_A_PIP_PC-GPWG.dta` (vermast V02, source GPWG)

Both rows survive because each is the max version *within its own module*. The
correct result is only the V02/GPWG row.

## Requirements

1. Version filtering (max `vermast`, then max `veralt`) must compare across all
   modules for a given country-year-survey.
2. The `module` and `source` columns must be preserved in the output.
3. The downstream source-deduplication logic (`pip_keep_pc_source()`) must
   continue to work for cases where the max version has multiple sources.
4. Apply the same fix to both PC and TB filtering blocks.

## Approaches Considered

### Approach 1: Remove module from by clause

Simply change `by = .(country_code, surveyid_year, survey_acronym, module)` to
`by = .(country_code, surveyid_year, survey_acronym)` in both the `vermast` and
`veralt` max-filtering steps. The `module` and `source` columns remain on each
row; only the grouping changes.

**Pros**: Minimal change, directly fixes root cause, no new dependencies.
**Cons**: None identified.
**Effort**: Small.

### Approach 2: Keep module in by, add second deduplication pass

Keep existing logic, add a post-filter step that deduplicates across modules by
picking the globally highest version.

**Pros**: Doesn't change existing logic.
**Cons**: More code, treats symptom not cause, harder to reason about.
**Effort**: Medium.

### Approach 3: Refactor into reusable filter_max_version() helper

Extract version-filtering into a standalone function with configurable `by_cols`.

**Pros**: DRY, testable.
**Cons**: Larger refactor on a deprecated function, more regression risk.
**Effort**: Medium.

## Decision

**Approach 1** — remove `module` from the `by` clause in both PC and TB blocks.
Simplest fix, directly addresses the root cause. The `module` column is not
dropped; it remains on each row as a regular column.

## Next Steps

1. Edit `pip_find_data.R`: remove `module` from `by` in both PC and TB blocks.
2. Verify with ZAF 2008 test case.
3. Run existing tests.
