---
date: 2026-06-04
title: "Add verbose argument to data loading functions missing it"
status: active
scope: "Lightweight"
brainstorm: null
language: "R"
estimated-effort: "small"
tags: [verbose, UX, consistency]
---

# Plan: Add verbose argument to data loading functions missing it

## Objective

Audit all exported data loading functions in pipload and ensure each has a
`verbose` argument defaulting to `getOption("pipload.verbose")`. Functions
that currently lack it should gain a consistent `verbose` parameter and
appropriate messaging.

## Context

The package convention is `verbose = getOption("pipload.verbose")` with
`cli::cli_alert_info()` messages when `verbose == TRUE`. Most functions
already follow this pattern. A few older or utility functions do not expose
`verbose` — they are either deprecated or were added before the convention
solidified.

### Current state

| Function | File | Has `verbose`? | Notes |
|----------|------|:-:|---|
| `load_pip_data` | load_pip_data.R | ✅ | `getOption("pipload.verbose")` |
| `find_pip_data` | load_pip_data.R | ✅ | |
| `load_pip_deflated_data` | load_pip_deflated_data.R | ✅ | |
| `load_dlw_data` | load_dlw_data.R | ✅ | |
| `find_dlw_data` | load_dlw_data.R | ✅ | |
| `load_aux_data` | load_aux_data.R | ✅ | |
| `pip_load_data` | pip_load_data.R | ✅ | (legacy) |
| `pip_load_dlw` | pip_load_dlw.R | ✅ | (legacy) |
| `pip_load_cache` | pip_load_cache.R | ✅ | |
| `pip_find_data` | pip_find_data.R | ✅ | (legacy) |
| `pip_find_dlw` | pip_find_dlw.R | ✅ | (legacy) |
| `pip_load_results` | pip_load_results.R | ✅ | |
| `pip_load_all_aux` | pip_load_all_aux.R | ✅ | Default `FALSE` (inconsistent) |
| `pip_find_cache` | pip_find_cache.R | ❌ | No verbose at all |
| `pip_load_dlw_inventory` | pip_load_dlw_inventory.R | ❌ | No verbose at all |
| `load_dlw_gmd_inventory` | load_dlw_data.R | ❌ | Zero-arg lambda, no verbose |
| `load_dlw_gmd_log` | load_dlw_data.R | ❌ | Zero-arg lambda, no verbose |
| `load_gmd_valid_inv` | load_dlw_data.R | ❌ | Zero-arg lambda, no verbose |
| `load_gmd_valid_log` | load_dlw_data.R | ❌ | Zero-arg lambda, no verbose |
| `load_gmd_valid_report` | load_dlw_data.R | ❌ | Zero-arg lambda, no verbose |
| `pip_load_inventory` | pip_load_inventory.R | ❌ | Deprecated — skip |

## Requirements

| ID | Requirement | Source |
|----|-------------|--------|
| R1 | `pip_find_cache()` gains `verbose = getOption("pipload.verbose")` and emits an info message when filtering succeeds | user |
| R2 | `pip_load_dlw_inventory()` gains `verbose = getOption("pipload.verbose")` and emits a loading message | user |
| R3 | `pip_load_all_aux()` default for `verbose` is changed from `FALSE` to `getOption("pipload.verbose")` for consistency | user |
| R4 | Five zero-arg DLW loaders (`load_dlw_gmd_inventory`, `load_dlw_gmd_log`, `load_gmd_valid_inv`, `load_gmd_valid_log`, `load_gmd_valid_report`) gain `verbose = getOption("pipload.verbose")` | user |
| R5 | Deprecated functions (`pip_load_inventory`) are left unchanged | user |

## Implementation Steps

### 1. Add `verbose` to `pip_find_cache()`
- **Requirements**: R1
- **Files**: `R/pip_find_cache.R`
- **Details**: Add `verbose = getOption("pipload.verbose")` parameter. Add a `cli::cli_alert_info()` after the filtering step reporting how many rows matched.
- **Tests**: Verify function still returns correct results; test that `verbose = FALSE` suppresses messages.
- **Acceptance criteria**: `pip_find_cache(verbose = TRUE)` emits a cli message; `verbose = FALSE` is silent.

### 2. Add `verbose` to `pip_load_dlw_inventory()`
- **Requirements**: R2
- **Files**: `R/pip_load_dlw_inventory.R`
- **Details**: Add `verbose = getOption("pipload.verbose")` parameter. Emit a `cli::cli_alert_info("Loading DLW inventory from {.path {dlw_inv_file}}")` when loading.
- **Tests**: Verify existing behavior is unchanged; test that message appears/disappears with flag.
- **Acceptance criteria**: Function loads inventory with optional info message.

### 3. Add `verbose` to five zero-arg DLW loaders
- **Requirements**: R4
- **Files**: `R/load_dlw_data.R`
- **Details**: For each of `load_dlw_gmd_inventory`, `load_dlw_gmd_log`, `load_gmd_valid_inv`, `load_gmd_valid_log`, `load_gmd_valid_report`: add `verbose = getOption("pipload.verbose")` parameter and emit `cli::cli_alert_info("Loading {.field <artifact>}")` before the `pip_read()` call when `verbose == TRUE`.
- **Tests**: Verify each function still returns correct results; verbose flag controls messaging.
- **Acceptance criteria**: All five functions accept `verbose` and emit info messages when TRUE.

### 4. Align `pip_load_all_aux()` default
- **Requirements**: R3
- **Files**: `R/pip_load_all_aux.R`
- **Details**: Change `verbose = FALSE` to `verbose = getOption("pipload.verbose")`.
- **Tests**: Ensure no regressions in existing tests.
- **Acceptance criteria**: Default is now option-driven like the rest of the package.

## Testing Strategy

- Existing tests should continue to pass.
- Manual interactive check: set `options(pipload.verbose = TRUE)` and call each function to confirm messages appear.
- No new test files required for a lightweight change; add inline `expect_message`/`expect_no_message` if the test infrastructure supports it.

## Documentation Checklist
- [x] Function documentation (roxygen2 `@param verbose` already inherited in most — add explicit `@param` where added)
- [ ] No README updates needed
- [ ] No inline comments needed

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Changing `pip_load_all_aux` default from `FALSE` to option-driven may surprise callers expecting silence | The package option defaults to `TRUE` interactively, but pipelines should set the option explicitly; this is the documented convention |

## Out of Scope

- Refactoring message content or switching to a logging framework.
- Adding `verbose` to deprecated functions.
- Adding `verbose` to non-data-loading utilities (e.g., `pip_create_globals`).
