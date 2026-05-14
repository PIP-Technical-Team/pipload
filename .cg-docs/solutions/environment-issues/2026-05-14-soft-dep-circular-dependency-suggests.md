---
date: 2026-05-14
title: "Soft dependency pattern for circular imports in the PIP package ecosystem"
category: "environment-issues"
language: "R"
tags: [circular-dependency, Suggests, rlang, pipdata, pipload, package-development]
root-cause: "pipdata imports pipload; adding pipdata to pipload Imports would create a circular dependency"
severity: "P1"
---

# Soft Dependency Pattern for Circular Imports in the PIP Package Ecosystem

## Problem

`pipload` needs to call `pipdata::pd_deflation()` inside `load_pip_deflated_data()`.
Adding `pipdata` to `Imports` in `pipload`'s `DESCRIPTION` would create a circular
dependency: `pipdata` already imports `pipload`. R's package install system will
refuse to install either package, and `devtools::check()` will fail with a cycle error.

## Root Cause

The PIP package graph has `pipdata → pipload` as a hard dependency (pipdata uses
pipload to load survey data). Any feature in pipload that calls back into pipdata
creates a directed cycle in the dependency graph.

## Solution

Declare `pipdata` in `Suggests` (not `Imports`) and guard the call at runtime with
`rlang::check_installed()`:

**DESCRIPTION:**
```
Suggests:
    pipdata (>= 0.0.0.9017)
```

**R function:**
```r
# NOTE: pipdata is a soft dependency (Suggests). Do not add it to Imports —
# circular dependency: pipdata imports pipload.
load_pip_deflated_data <- function(...) {
  rlang::check_installed(
    "pipdata",
    reason = "to apply deflation via `pd_deflation()`"
  )
  # ... rest of function
}
```

**Key rules:**
- Always pin a minimum version in `Suggests`: `pipdata (>= x.y.z)` — API drift
  goes undetected otherwise.
- Use `rlang::check_installed()` (not `requireNamespace()`) — it produces a
  formatted, user-facing error with installation instructions.
- Call the package with the `::` operator (`pipdata::pd_deflation()`), never
  `library(pipdata)` or `@import pipdata`.
- Leave a `# NOTE:` comment above the function explaining the constraint so no
  future developer accidentally promotes it to `Imports`.

## Prevention

- In any PIP package that needs to call another PIP package: check the existing
  `Imports` chain first. If the target already imports the current package,
  `Suggests` is the only valid option.
- Document the constraint explicitly in code comments — roxygen description is
  **not** the right place (it renders in the user-facing help page).
- `renv::snapshot()` will abort in a dev session where the package-under-development
  is loaded via `devtools::load_all()` and a `Suggests` package depends on it.
  Run `renv::snapshot()` from a clean R session after `R CMD INSTALL` instead.

## Related

- [testing-patterns/2026-05-14-mocking-suggests-packages-local-mocked-bindings.md](../testing-patterns/2026-05-14-mocking-suggests-packages-local-mocked-bindings.md) — How to unit-test functions that call a `Suggests` package without requiring it to be installed.
