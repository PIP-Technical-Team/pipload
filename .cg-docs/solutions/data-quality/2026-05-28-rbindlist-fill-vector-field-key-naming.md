---
date: 2026-05-28
title: "rbindlist(fill=TRUE) vector-field key-naming discipline"
category: "data-quality"
language: "R"
tags: [data.table, rbindlist, vector-fields, metadata, pip_inv_enrich, wide-column-expansion]
root-cause: "Emitting the raw field name as a list key for absent vector fields creates a spurious column alongside the expanded wide columns after rbindlist(fill=TRUE)"
severity: "P1"
---

# rbindlist(fill=TRUE) vector-field key-naming discipline

## Problem

When building a list of rows for `rbindlist(fill=TRUE)`, mixing key names
across rows causes all unique keys to become columns — silently. If row A
emits `list(cpi = NA)` for a missing field while row B emits
`list(cpi_2011_national = 0.266)` from an expansion function, the result
contains **both** a `cpi` column and a `cpi_2011_national` column:

```r
# row A (field absent):
list(cpi = NA)

# row B (field present):
list(cpi_2011_national = 0.266, cpi_2005_rural = 1.1)

data.table::rbindlist(list(row_A, row_B), fill = TRUE)
#    cpi  cpi_2011_national  cpi_2005_rural
#     NA              NA               NA      ← row A
#     NA           0.266             1.1      ← row B (cpi filled NA by fill=TRUE)
```

The `cpi` column is spurious — it contains only logical NAs and is
meaningless, but it pollutes the result silently.

## Root Cause

The code that handles "field absent from this survey's metadata" used the same
`setNames(list(NA), field)` pattern for **both** scalar and vector fields.
For scalar fields, emitting the raw field name is correct — it ensures the
column appears in every row. For vector fields, it is wrong — the expanded
columns (`cpi_YYYY_area`) are the real output, not the raw `cpi` key.

This same bug can appear in **any** branch that produces per-row output lists
for `rbindlist`: the null-metadata branch, the absent-field branch, etc.

## Solution

For vector fields that expand into multiple column names, **never emit the raw
field name as a list key** from any branch. Omitting the key causes
`rbindlist(fill=TRUE)` to fill all expanded columns for that row with `NA`,
which is the correct behavior.

```r
if (is.null(val)) {
  # Vector fields: return empty list — rbindlist(fill=TRUE) fills wide
  # columns (cpi_YYYY_area, etc.) with NA automatically.
  # Scalar fields: emit typed NA so the column appears in every row.
  if (field %in% .PIP_META_VECTOR_FIELDS) {
    return(list())
  }
  setNames(list(NA_character_), field)
}
```

Apply the same guard in **every** branch that can return early for a row:
- null-metadata branch (P1.1 fix)
- absent-field-in-metadata branch (this fix)
- any future early-return path added to the expansion loop

## Prevention

### Rule

> For vector fields that expand into multiple column names via a helper like
> `expand_meta_field()`, **the raw field name must never appear as a key in
> any row's output list**. Only the expansion helper may emit keys for vector
> fields.

### Checklist before adding a new early-return branch in a `rbindlist` loop

- [ ] Does the branch return `list(field = NA)` for any field?
- [ ] Is that field a vector field (i.e., does it expand into multiple columns)?
- [ ] If yes → return `list()` instead, not `list(field = NA)`.

### Testing pattern

Always include a mixed-rows test when implementing wide-column expansion:
two rows where one has the vector field and one does not. Assert
`expect_false(field %in% names(result))`.

```r
test_that("no spurious raw column when some rows lack vector field", {
  # row1 has cpi, row2 does not
  result <- pip_inv_enrich(inv_two_rows, fields = "cpi")
  expect_contains(names(result), "cpi_2011_national")
  expect_false("cpi" %in% names(result))
  expect_true(is.na(result[pip_id == "row2_id", cpi_2011_national]))
})
```

## Related

- [bugs/2026-05-28-spurious-raw-vector-field-column-when-metadata-absent.md](../bugs/2026-05-28-spurious-raw-vector-field-column-when-metadata-absent.md) — the specific instance of this pattern in `pip_inv_enrich` (P1.1b fix)
- P1.1 in `.cg-docs/reviews/2026-05-28-pip-inv-enrich-fast-bulk-v3-review.md` — same pattern in the null-metadata branch (first occurrence)
