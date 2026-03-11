---
date: 2026-03-04
title: "testthat runs with LC_COLLATE=C, breaking string max() on mixed-case versions"
category: "testing-patterns"
language: "R"
tags: [testthat, locale, LC_COLLATE, max, string-comparison, data.table, withr]
root-cause: "testthat runs tests with LC_COLLATE=C where uppercase ASCII < lowercase ASCII, so max(c('v01', 'V02')) returns 'v01' instead of 'V02'"
severity: "P2"
---

# testthat Runs with LC_COLLATE=C — String max() Breaks on Mixed-Case Inputs

## Problem

Tests that verify version-string comparisons passed interactively but failed
when run via `testthat::test_file()` or `devtools::test()`.

**Interactive console** (`LC_COLLATE = English_United States.utf8`):
```r
max(c("v01", "V02"))  # => "V02"  ✓
```

**Inside testthat** (`LC_COLLATE = C`):
```r
max(c("v01", "V02"))  # => "v01"  ✗
```

Under the C locale, uppercase letters (`V`, ASCII 86) sort *before* lowercase
(`v`, ASCII 118), so `"v01" > "V02"`.

## Root Cause

`testthat` resets `LC_COLLATE` to `"C"` for reproducibility. Any code that
calls `max()`, `min()`, `sort()`, or `order()` on character strings with
**mixed case** will produce locale-dependent results.

The production inventory has version strings like `v01`, `V02`, `V01` —
mixed case is real data, not a test artifact.

## Solution

### 1. Make production code locale-independent with `toupper()`

Normalize to uppercase before comparison so the result is the same under any
locale:

```r
# BEFORE (locale-sensitive)
maxmast := vermast == max(vermast),

# AFTER (locale-independent)
maxmast := toupper(vermast) == max(toupper(vermast)),
```

### 2. Pin the hostile locale in tests that exercise this path

Use `withr::local_locale()` to explicitly run the test under `LC_COLLATE = "C"`,
proving the fix holds in the worst case:

```r
test_that("filter keeps max version (locale-independent)", {
  withr::local_locale(c(LC_COLLATE = "C"))  # pin hostile locale

  dt <- data.table::data.table(
    country = "ZAF",
    ver     = c("v01", "V02")   # mixed case — would fail without toupper()
  )
  dt[, maxver := toupper(ver) == max(toupper(ver))]
  expect_equal(dt[maxver == TRUE, ver], "V02")
})
```

### 3. Use all-uppercase version strings in fixtures as a fallback

If you cannot change the production code, use uppercase-only mock data so
tests pass under both locales:

```r
# Safe fixture — no mixed case
vermast = c("V01", "V02")   # max() returns "V02" under any locale
```

## Prevention

- Always wrap `max()` / `min()` / `sort()` on version strings with `toupper()`
  (or `tolower()`) when the strings may contain mixed case.
- In tests that exercise string-ordering logic, add
  `withr::local_locale(c(LC_COLLATE = "C"))` to document and enforce
  locale-independence.
- Avoid relying on interactive `max()` results for mixed-case strings — the
  console locale may differ from CI/testthat.

**Checklist for any `max()`/`min()` on string columns:**

```r
# Ask: could this column contain mixed case in production?
# Yes → normalize first
dt[, flag := toupper(col) == max(toupper(col)), by = group]
```

## Related

- `docs/solutions/data-quality/2026-03-04-version-filtering-module-grouping-bug.md`
- `withr` package: `withr::local_locale()`, `withr::with_locale()`
- R internals: `?Comparison`, `?locales`
