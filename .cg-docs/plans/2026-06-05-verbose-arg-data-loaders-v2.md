---
date: 2026-06-05
title: "Add verbose argument to data loading functions missing it (v2)"
status: active
scope: "Lightweight"
brainstorm: null
language: "R"
estimated-effort: "small"
tags: [verbose, UX, consistency]
supersedes: ".cg-docs/plans/2026-06-04-verbose-arg-data-loaders.md"
review-findings: [P2.1-cascade-amplification, P2.2-pip-read-threading, P3.1-inheritParams, P3.2-testing-hedge, P3.3-legacy-guard]
---

# Plan: Add verbose argument to data loading functions missing it (v2)

## Objective

Audit all exported data loading functions in pipload and ensure each has a
`verbose` argument defaulting to `getOption("pipload.verbose")`. Functions
that currently lack it should gain a consistent `verbose` parameter and
appropriate messaging. The `verbose` value must be threaded through the entire
call stack (including `pip_read()`) so that `verbose = FALSE` guarantees
silence.

## Context

The package convention is `verbose = getOption("pipload.verbose")` with
`cli::cli_alert_info()` messages when `verbose == TRUE`. Most functions
already follow this pattern. A few older or utility functions do not expose
`verbose` — they are either deprecated or were added before the convention
solidified.

`pip_read()` already accepts `verbose` and forwards it to `stamp::st_load()`.
Any function calling `pip_read()` must pass its own `verbose` value through to
ensure callers get consistent silence when they request it.

### Revision notes (from plan review)

- **P2.1**: `pip_load_all_aux` cascades `verbose` into `pip_load_aux()` inside
  a loop. Changing the default from `FALSE` to `getOption("pipload.verbose")`
  means N messages fire interactively (one per aux). This is intentional —
  document it and add a loop-level test.
- **P2.2**: The five lambda functions must pass `verbose = verbose` to
  `pip_read()` to ensure end-to-end silence when `verbose = FALSE`.
- **P3.1**: `pip_find_cache` inherits `@param verbose` from `pip_load_cache`
  via `@inheritParams` — no explicit `@param` needed.
- **P3.2**: Testing strategy is definitive — `expect_message()` /
  `expect_no_message()` tests are required, not optional.
- **P3.3**: `pip_load_dlw_inventory` uses legacy globals; note it as
  provisional pending possible deprecation.

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
| `pip_load_dlw_inventory` | pip_load_dlw_inventory.R | ❌ | No verbose; legacy globals pattern (provisional) |
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
| R2 | `pip_load_dlw_inventory()` gains `verbose = getOption("pipload.verbose")` and emits a loading message (provisional — may be deprecated) | user |
| R3 | `pip_load_all_aux()` default for `verbose` changes from `FALSE` to `getOption("pipload.verbose")` | user |
| R4 | Five zero-arg DLW loaders gain `verbose = getOption("pipload.verbose")` and thread it to `pip_read()` | user + P2.2 |
| R5 | All modified functions pass `verbose` through to downstream calls (`pip_read()`, `pip_load_aux()`) — no silent leaks | P2.2 |
| R6 | Deprecated functions (`pip_load_inventory`) are left unchanged | user |

## Implementation Steps

## Phase 1: Core implementation

### 1. Add `verbose` to `pip_find_cache()`
- **Requirements**: R1
- **Files**: `R/pip_find_cache.R`
- **Details**: Add `verbose = getOption("pipload.verbose")` parameter. Add a `cli::cli_alert_info()` after the filtering step reporting how many rows matched.
- **Documentation**: No explicit `@param verbose` needed — `@inheritParams pip_load_cache` already provides it (P3.1).
- **Acceptance criteria**: `pip_find_cache(verbose = TRUE)` emits a cli message; `verbose = FALSE` is silent.

### 2. Add `verbose` to `pip_load_dlw_inventory()` (provisional)
- **Requirements**: R2
- **Files**: `R/pip_load_dlw_inventory.R`
- **Details**: Add `verbose = getOption("pipload.verbose")` parameter. Emit `cli::cli_alert_info("Loading DLW inventory from {.path {dlw_inv_file}}")` when loading. Note: this function uses legacy `pip_create_globals()` globals and may be deprecated when the pipeline migration completes (P3.3).
- **Acceptance criteria**: Function loads inventory with optional info message.

### 3. Add `verbose` to five zero-arg DLW loaders and thread to `pip_read()`
- **Requirements**: R4, R5
- **Files**: `R/load_dlw_data.R`
- **Details**: For each of `load_dlw_gmd_inventory`, `load_dlw_gmd_log`, `load_gmd_valid_inv`, `load_gmd_valid_log`, `load_gmd_valid_report`:
  1. Change `\()` to `\(verbose = getOption("pipload.verbose"))`.
  2. Add `cli::cli_alert_info("Loading {.field <artifact>}")` gated on `if (verbose)`.
  3. Pass `verbose = verbose` to the `pip_read()` call so that `pip_read()`'s own message and `stamp::st_load()` both respect the flag.
- **Documentation**: These share `@rdname load_dlw_data`; add a single `@param verbose logical. If TRUE display loading messages. Default is option "pipload.verbose"` to the shared block.
- **Acceptance criteria**: `load_gmd_valid_inv(verbose = FALSE)` produces zero messages (neither from the function nor from `pip_read()`).

### 4. Align `pip_load_all_aux()` default and document cascade
- **Requirements**: R3, R5
- **Files**: `R/pip_load_all_aux.R`
- **Details**: Change `verbose = FALSE` to `verbose = getOption("pipload.verbose")`. The function already passes `verbose = verbose` to `pip_load_aux()` inside the loop (line ~125). **Cascade note** (P2.1): with the new default, interactive users will see one message per aux file loaded (currently 5 by default). This matches the behaviour of all other loaders and is the intended UX — pipelines should set `options(pipload.verbose = FALSE)` at session start.
- **Acceptance criteria**: Default is option-driven; passing `verbose = FALSE` silences the entire loop.

## Phase 2: Tests

### 5. Add message tests for all modified functions
- **Requirements**: R1–R5
- **Files**: `tests/testthat/test-verbose-arg.R` (new)
- **Details**: For each modified function, add paired tests:
  - `expect_message(fn(verbose = TRUE), ...)` confirming cli output.
  - `expect_no_message(fn(verbose = FALSE))` confirming full silence.
  - For `pip_load_all_aux`: test with a length-2 `aux` vector to confirm multiple messages fire when verbose, and zero when not (P2.1 cascade coverage).
  - Tests will need mocking (`local_mocked_bindings`) to avoid requiring real data connections.
- **Acceptance criteria**: All tests pass under `devtools::test()`.

## Testing Strategy

- `expect_message()` for `verbose = TRUE` paths.
- `expect_no_message()` for `verbose = FALSE` paths (end-to-end silence).
- Mock external dependencies (`pipfun::get_pip_folders`, `stamp::st_alias_list`, `pip_read`) to run without a live pipeline connection.
- Test `pip_load_all_aux` with loop length > 1 to cover the cascade amplification (P2.1).

## Documentation Checklist
- [x] Function documentation: `@param verbose` via `@inheritParams` where applicable; explicit where not inherited
- [ ] No README updates needed
- [ ] No inline comments needed beyond the cascade note in `pip_load_all_aux`

## Risks & Mitigations

| Risk | Mitigation |
|------|-----------|
| Changing `pip_load_all_aux` default from `FALSE` to option-driven fires N messages interactively (P2.1) | This is intended UX — matches all other loaders. Pipelines must set `options(pipload.verbose = FALSE)`. Document in `?pip_load_all_aux`. |
| `pip_load_dlw_inventory` may be deprecated soon (P3.3) | Mark as provisional in the plan. The verbose addition is trivial and doesn't block migration. |
| Shared `@rdname load_dlw_data` block — adding `@param verbose` to all five lambdas at once | Add a single `@param verbose` in the shared documentation block (not per-function) to avoid duplication. |

## Out of Scope

- Refactoring message content or switching to a logging framework.
- Adding `verbose` to deprecated functions.
- Adding `verbose` to non-data-loading utilities (e.g., `pip_create_globals`).
- Migrating `pip_load_dlw_inventory` to new pipeline structure.
