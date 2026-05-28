---
date: 2026-05-28
title: "expect_warning() without expect_error() re-throws the error in R CMD check"
category: "testing-patterns"
language: "R"
tags: [testthat, expect_warning, expect_error, R-CMD-check, lifecycle, deprecated]
root-cause: "expect_warning() only catches and muffles the warning, not the subsequent error; in an environment where the function also errors (e.g. no working release configured), the uncaught error propagates and fails the test"
severity: "P2"
---

# expect_warning() without expect_error() re-throws the error in R CMD check

## Problem

A test case that tested deprecation warnings passed interactively (where
`pipfun::setup_working_release()` had been called) but failed during
`R CMD check` with an unexpected error:

```r
# Before — fails under R CMD check
pip_load_aux(measure = "ppp", file_to_load = "ppp_vintage") |>
  expect_warning()
```

`R CMD check` error:
```
── Failure: Syntax is working ──────────────────────────────────────────────
pip_load_aux(...) did not throw a warning.
Error: Working release has not been set up.
```

## Root Cause

`expect_warning()` captures the first warning then **re-executes the expression**
to completion. In the `R CMD check` environment there is no working release
configured, so `pip_load_aux()` fires `lifecycle::deprecate_warn()` (the
expected warning) and then immediately calls `pipfun::get_wrk_release()` which
`cli::cli_abort()`s with "Working release has not been set up."

`expect_warning()` muffles the deprecation warning but does not absorb the
subsequent error — it re-throws it, failing the test.

The other three cases in the same `test_that()` block already used
`|> expect_error()` after `|> expect_warning()`, which is why they passed.
This one case was missing the error assertion.

## Solution

Always chain `|> expect_error()` after `|> expect_warning()` when the function
under test can also throw an error after the warning:

```r
# After — passes under R CMD check
pip_load_aux(measure = "ppp", file_to_load = "ppp_vintage") |>
  expect_warning() |>
  expect_error()
```

The pipe chains `expect_warning()` → `expect_error()` on the same expression.
testthat captures the warning first, then lets the error propagate to
`expect_error()`.

## Prevention

- When testing a deprecated function that also makes network/env calls, **always
  assert both the warning and the subsequent error** — do not stop at
  `expect_warning()` alone.
- A test that only passes interactively (because the environment is configured)
  is hiding an incomplete assertion. Use `withr::local_envvar()` or
  `testthat::skip_if_not()` to make environment preconditions explicit, or
  assert the full error chain as shown above.
- Pattern to follow for lifecycle deprecation tests in this package:

```r
# Pattern: deprecated param + env not set up
fn(deprecated_arg = "value") |>
  expect_warning() |>   # lifecycle::deprecate_warn()
  expect_error()        # downstream abort (no working release, dir not found, etc.)

# Pattern: deprecated param + happy path (env is set up via withr)
withr::local_envvar(PIP_WORKING_RELEASE = "<release>")
fn(deprecated_arg = "value") |>
  expect_warning()      # only if no error follows
```

## Related

- [build-errors/2026-05-28-rcmd-check-rd-param-mismatches.md](../build-errors/2026-05-28-rcmd-check-rd-param-mismatches.md) — companion R CMD check fixes from the same session
- [testing-patterns/2026-05-14-mocking-suggests-packages-local-mocked-bindings.md](./2026-05-14-mocking-suggests-packages-local-mocked-bindings.md) — related pattern for isolating Suggests packages in tests
