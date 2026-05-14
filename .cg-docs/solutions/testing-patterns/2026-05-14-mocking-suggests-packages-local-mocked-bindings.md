---
date: 2026-05-14
title: "Unit-testing functions that call a Suggests package with local_mocked_bindings()"
category: "testing-patterns"
language: "R"
tags: [testthat, local_mocked_bindings, Suggests, soft-dependency, mocking, skip_if_not_installed, CI]
root-cause: "Combining skip_if_not_installed() with local_mocked_bindings() silently skips core unit tests in CI, producing false-green results"
severity: "P1"
---

# Unit-Testing Functions That Call a Suggests Package

## Problem

A function uses a `Suggests`-only package (e.g. `pipdata`). Tests that mock the
`Suggests` call with `local_mocked_bindings()` also have `skip_if_not_installed()`.
In CI — where the `Suggests` package is typically absent — the tests are silently
skipped, even though the mock removes the real package requirement entirely.
Result: CI is green but the core code path is never tested.

## Root Cause

`skip_if_not_installed("pipdata")` evaluates at test runtime, before any mocking
takes effect. It checks whether `pipdata` is installed in the current R library.
If it is not, it skips — regardless of whether the test body uses mocks that would
never touch the real package.

## Solution

**Rule**: `skip_if_not_installed()` belongs only on tests that call the real package.
Mocked tests do not need it.

```r
# ✅ CORRECT — guard-check test (no mock; tests rlang::check_installed fires)
test_that("aborts when pipdata is not installed", {
  local_mocked_bindings(
    check_installed = function(pkg, ...) rlang::abort(paste0("need ", pkg)),
    .package = "rlang"
  )
  expect_error(my_func(), class = "rlang_error")
})

# ✅ CORRECT — mocked unit test (mock removes real package requirement)
test_that("calls pd_deflation with correct args", {
  # No skip_if_not_installed — the mock replaces the real call
  local_mocked_bindings(
    pd_deflation = function(dt, pip_id = NULL, ...) data.table(),
    .package = "pipdata"
  )
  expect_no_error(my_func(id_name = "BOL_2022_EH_INC_ALL"))
})

# ✅ CORRECT — integration test (calls the real package; skip is appropriate)
test_that("integration: load → deflate round-trip", {
  skip_if_not_installed("pipdata")
  skip_on_ci()
  # ... real call
})
```

```r
# ❌ WRONG — skip negates the mock
test_that("calls pd_deflation with correct args", {
  skip_if_not_installed("pipdata")   # <-- skip fires before mock; CI never runs this
  local_mocked_bindings(
    pd_deflation = function(...) data.table(),
    .package = "pipdata"
  )
  expect_no_error(my_func())
})
```

## Prevention

When writing tests for a function that guards a `Suggests` package with
`rlang::check_installed()`, follow this three-test structure:

| Test type | Has `skip_if_not_installed`? | Has mock? |
|---|---|---|
| Guard test — verifies error fires when pkg absent | No | Yes (`check_installed` mock) |
| Unit test — verifies function behaviour | No | Yes (target function mock) |
| Integration test — exercises real package | **Yes** | No |

**Additional**: if `local_mocked_bindings()` is the correct tool, remember it
requires the target package to be loadable (namespace must exist). For CI where
the package is never installed, mock `rlang::check_installed` in the guard test
and leave the namespace calls mocked too.

## Related

- [environment-issues/2026-05-14-soft-dep-circular-dependency-suggests.md](../environment-issues/2026-05-14-soft-dep-circular-dependency-suggests.md) — Why `Suggests` is used and how to declare it correctly.
