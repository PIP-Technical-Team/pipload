---
date: 2026-03-04
title: "Version filtering grouped by module produces duplicate rows"
category: "data-quality"
language: "R"
tags: [data.table, grouping, by-clause, version-filtering, inventory, pip_find_data]
root-cause: "module included in the data.table by= clause caused max(version) to be computed per-module rather than globally, so rows from different modules with different versions all survived the filter"
severity: "P1"
---

# Version Filtering Grouped by Module Produces Duplicate Rows

## Problem

`pip_find_data(filter_to_pc = TRUE)` (and `filter_to_tb = TRUE`) was returning
multiple rows for the same country-year-survey when different modules existed
under different master versions.

**Symptom (ZAF 2008 example):**

| country | year | vermast | module  | source |
|---------|------|---------|---------|--------|
| ZAF     | 2008 | V01     | PC-HIST | HIST   |
| ZAF     | 2008 | V02     | PC-GPWG | GPWG   |

Both rows survived — only the V02/GPWG row should.

## Root Cause

The version-filtering step computed `max(vermast)` with `module` in the `by=`
clause:

```r
# BUGGY — module in by= means max is computed within each module group
maxmast := vermast == max(vermast),
by = .(country_code, surveyid_year, survey_acronym, module)
```

Because each module only sees its own rows, `V01` is the max for the HIST
group and `V02` is the max for the GPWG group. Both pass `maxmast == 1`.

The intent was to keep only the *globally* highest version across all modules
for a given country-year-survey.

## Solution

Remove `module` from the `by=` clause so the max is computed across all rows
for the same country-year-survey, regardless of module:

```r
# FIXED — module intentionally excluded from by=
# versions compared globally so only one version (and its module) survives
maxmast := toupper(vermast) == max(toupper(vermast)),
by = .(country_code, surveyid_year, survey_acronym)
```

Apply the same fix to `veralt` and to the TB block (4 `by=` clauses total).

The `module` and `source` columns on each row are **not** modified — only the
grouping for `max()` changes. Any rows tied at the max version are handled
downstream by `pip_keep_pc_source()` (priority: GPWG > HIST > BIN > GROUP > synth).

## Prevention

When writing `by=` clauses for "keep max per group" patterns, ask:

> Should the max be computed *within* each subgroup independently, or *across*
> all subgroups sharing the same key?

If the latter, do **not** include the differentiating column in `by=`.

Anti-pattern to avoid:

```r
# WRONG if you want global max per country-year-survey
dt[, maxver := ver == max(ver), by = .(country, year, survey, module)]

# RIGHT
dt[, maxver := ver == max(ver), by = .(country, year, survey)]
```

## Related

- `docs/solutions/testing-patterns/2026-03-04-lc-collate-string-comparison.md`
- `docs/brainstorms/2026-03-04-fix-version-filtering-module-grouping.md`
- `docs/plans/2026-03-04-fix-version-filtering-module-grouping.md`
