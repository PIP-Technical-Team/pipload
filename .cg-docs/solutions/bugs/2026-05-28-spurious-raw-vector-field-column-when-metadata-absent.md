---
date: 2026-05-28
title: "Spurious raw vector-field column when survey metadata lacks that field"
category: "bugs"
type: "bug"
language: "R"
tags: [pip_inv_enrich, data.table, rbindlist, vector-fields, cpi, metadata]
root-cause: "has-meta branch emitted list(cpi = NA) for absent vector fields, creating a raw 'cpi' column that coexisted with expanded 'cpi_YYYY_area' columns after rbindlist(fill=TRUE)"
severity: "P1"
test-written: "yes"
fix-confirmed: "yes"
---

# Spurious raw vector-field column when survey metadata lacks that field

## Symptom

Calling `load_pip_master_inventory(fields = c("reporting_level", "cpi"))` (or
`pip_inv_enrich(inv, fields = "cpi")` directly) produced an extra all-NA
logical column named `cpi` alongside the correct wide columns
`cpi_2005_national`, `cpi_2011_national`, etc.

## Root Cause

Inside `pip_inv_enrich()`, the has-metadata expansion loop handled a missing
field with:

```r
val <- meta[[field]]
if (is.null(val)) {
  setNames(list(NA), field)   # emits list(cpi = NA)
}
```

When a survey's metadata exists but does not contain a `cpi` entry,
`meta[["cpi"]]` is `NULL` and `setNames(list(NA), "cpi")` was returned.
Rows that *do* have `cpi` returned `list(cpi_2011_national = 0.266, ...)` from
`expand_meta_field()`. After `data.table::rbindlist(fill = TRUE)`, both column
sets were merged, producing a spurious `cpi` (logical) column alongside the
legitimate `cpi_*` expansion columns.

This is the has-metadata sibling of P1.1 (which fixed the same pattern in the
null-metadata branch).

## Reproduction Test

Added to `tests/testthat/test-pip_inv_enrich.R`:

```r
test_that("no spurious raw 'cpi' column when some rows lack cpi in metadata", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  meta1 <- list(
    surveyid_year = 2022,
    reporting_level = "national",
    cpi = c(`2011_national` = 0.266)
  )
  meta2 <- list(
    surveyid_year = 1980,
    reporting_level = "national"
    # no cpi field at all
  )
  inv <- data.table::rbindlist(list(
    make_inv_with_meta("CHN_2022_X_INC_ALL", meta1, dir1),
    make_inv_with_meta("ARG_1980_X_INC_ALL", meta2, dir2)
  ))

  result <- pip_inv_enrich(inv, fields = c("reporting_level", "cpi"))

  expect_contains(names(result), "cpi_2011_national")
  expect_false("cpi" %in% names(result))
  expect_true(is.na(result[pip_id == "ARG_1980_X_INC_ALL", cpi_2011_national]))
})
```

## Fix

In `R/pip_inv_enrich.R`, the null-value branch inside the has-metadata
expansion loop was changed to skip vector fields entirely (returning an empty
list) instead of emitting a raw field-name NA:

```r
if (is.null(val)) {
  if (field %in% .PIP_META_VECTOR_FIELDS) {
    return(list())   # omit: rbindlist(fill=TRUE) fills expanded cols with NA
  }
  setNames(list(NA_character_), field)
}
```

`rbindlist(fill = TRUE)` already handles the missing expanded columns by
filling them with `NA`. The only change needed is to not emit the raw
field-name key for vector fields.

Scalar fields still emit `NA_character_` (typed NA) so their column appears in
every row.

## Lessons Learned

`rbindlist(fill = TRUE)` merges column sets from all rows — if **any** row
emits a key name that conflicts with an expansion pattern used by other rows,
both columns coexist in the result. The rule is:

> For vector fields that expand into multiple column names, **never emit the
> raw field name as a key** in any row's output list — not in the null-meta
> branch, not in the absent-field branch, not anywhere. Only
> `expand_meta_field()` may emit keys for vector fields.

The null-meta branch (P1.1) was fixed first; this bug was its exact counterpart
in the has-meta branch. Both branches must apply the same guard:
skip raw-key emission for `.PIP_META_VECTOR_FIELDS`.

## Related

- P1.1 in `.cg-docs/reviews/2026-05-28-pip-inv-enrich-fast-bulk-v3-review.md`
  — same pattern in the null-metadata branch (already fixed).
