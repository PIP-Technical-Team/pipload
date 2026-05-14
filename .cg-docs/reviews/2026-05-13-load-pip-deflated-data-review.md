---
plan: .cg-docs/plans/2026-05-13-load-pip-deflated-data.md
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P1.5: fixed
  P1.6: fixed
  P2.1: fixed
  P2.2: fixed
  P2.3: fixed
  P2.4: fixed
  P2.5: fixed
  P2.6: fixed
  P2.7: skipped
  P2.8: fixed
  P2.9: fixed
  P2.10: fixed
  P3.1: fixed
  P3.2: fixed
  P3.3: fixed
  P3.4: fixed
  P3.5: fixed
  P3.6: fixed
---

## Review Report

**Review depth**: Standard + @cg-data-quality (auto-escalation: `load*.R` trigger)
**Files reviewed**: 3 (`R/load_pip_deflated_data.R`, `tests/testthat/test-load_pip_deflated_data.R`, `DESCRIPTION`)
**Findings**: 22 (P1: 6, P2: 10, P3: 6)

---

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-code-quality / cg-version-control / cg-architecture] `NAMESPACE` — `load_pip_deflated_data` has `@export` but NAMESPACE has not been regenerated.
  **Why**: Function is unreachable from outside the package on install; `devtools::check()` will warn.
  **Fix**: Run `devtools::document()` and commit the regenerated `NAMESPACE` and `man/load_pip_deflated_data.Rd`.

- **[P1.2]** [cg-data-quality] `R/load_pip_deflated_data.R:76` — `find_pip_data()` returning 0 rows sets `pip_id <- character(0)`. Since `character(0)` is not `NULL`, `load_pip_data()` takes the `id_name` branch, forwarding the empty vector to `check_pip_id_name()`, which throws an opaque `"argument is of length zero"` base-R error.
  **Why**: Silent data integrity failure with no user-readable context.
  **Fix**: After `pip_id <- inv[, pip_id]`, add `if (length(pip_id) == 0L) cli::cli_abort(...)`.

- **[P1.3]** [cg-data-quality / cg-architecture / cg-code-quality] `R/load_pip_deflated_data.R:76` — `find_pip_data()` returning >1 rows passes a length-N vector to `load_pip_data()`, which aborts with "Wrong number of data to load" — attributing the fault to the wrong caller.
  **Why**: Users called `load_pip_deflated_data`, but the error points inside `load_pip_data`.
  **Fix**: After `pip_id <- inv[, pip_id]`, add `if (length(pip_id) > 1L) cli::cli_abort(...)` with `pip_id` enumeration.

- **[P1.4]** [cg-code-quality / cg-testing / cg-reproducibility] `tests/testthat/test-load_pip_deflated_data.R:21` — Tests 2, 3, and 4 use `local_mocked_bindings` to stub `pipdata::pd_deflation` but also have `skip_if_not_installed("pipdata")`. The skip silently skips these core unit tests whenever `pipdata` is absent — including in most CI environments — even though the mocks remove the real package requirement.
  **Why**: CI produces a green signal while the core code path is untested.
  **Fix**: Remove `skip_if_not_installed("pipdata")` from the three mocked tests (2, 3, 4). Keep it only in the integration test (test 5).

- **[P1.5]** [cg-architecture] `R/load_pip_deflated_data.R:96` — `as_pipid()` case missing in the no-`module`-column branch. `assign_pipclass()` dispatches `as_pipid()` when `"sim" %in% names(df)` (imputed-data surveys). The else branch only checks GROUP vs default — imputed surveys without a `module` column silently receive `pipmd` class.
  **Why**: Silent misclassification; direct divergence from `assign_pipclass()` logic.
  **Fix**: Add `if ("sim" %in% names(survey)) as_pipid(survey)` as first arm of the no-module else branch (before the GROUP check).

- **[P1.6]** [cg-data-quality] `R/load_pip_deflated_data.R:37` — When `id_name` is supplied alongside filter args (`country_code`, etc.), the filter args are silently discarded. A user calling with both `id_name = "BOL_..."` and `country_code = "PRY"` gets Bolivia data without any warning.
  **Why**: Silent data integrity issue — wrong country, no signal.
  **Fix**: Emit `cli::cli_warn()` before the `if (!is.null(id_name))` block when any filter arg is also non-NULL.

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-code-quality / cg-architecture] `R/load_pip_deflated_data.R:98` — `utils::tail(strsplit(...))` for extracting the module suffix. Not idiomatic; `utils` not in Imports; fragile if `survey_acronym` ever contains underscores.
  **Why**: `sub(".*_", "", pip_id)` is more robust and has no package dependency.
  **Fix**: Replace with `pip_module <- sub(".*_", "", pip_id)`.

- **[P2.2]** [cg-code-quality] `R/load_pip_deflated_data.R:103` — Final `pipdata::pd_deflation(...)` call not wrapped in `return()`.
  **Why**: `cg-skill-r-shared` requires explicit `return()` at the end of non-trivial functions.
  **Fix**: `return(pipdata::pd_deflation(...))`.

- **[P2.3]** [cg-code-quality / cg-documentation] `R/load_pip_deflated_data.R:25` — `@family load_pip_data` creates a single-member family. `load_pip_data()` has no `@family` tag.
  **Why**: pkgdown renders a See-Also "Family" section listing only this function — useless.
  **Fix**: Remove `@family` and rely on `@seealso [load_pip_data()]`, OR add matching `@family PIP data loading` to `load_pip_data()` too.

- **[P2.4]** [cg-architecture / cg-code-quality] `R/load_pip_deflated_data.R:96` — Module-detection logic (GROUP/default) duplicated from `pd_deflation()` internals. P1.5 already demonstrates the copy diverged.
  **Why**: Maintenance liability; any future module type in `pd_deflation()` must be mirrored here manually.
  **Fix**: Extract a `@keywords internal` helper `assign_pipclass_from_id(survey, pip_id)` in `R/pip_class.R` centralising the full dispatch (sim → `as_pipid`, GROUP → `as_pipgd`, else → `as_pipmd`).

- **[P2.5]** [cg-documentation] `R/load_pip_deflated_data.R:8` — Developer constraint (*"Do not add `pipdata` to `Imports` — circular dependency…"*) is rendered verbatim in the user-facing help page.
  **Why**: Clutters user documentation with implementation details.
  **Fix**: Move to a `# NOTE:` comment above the function or to `@section Developer notes:`.

- **[P2.6]** [cg-reproducibility] `DESCRIPTION:38` — `pipdata` in Suggests has no minimum version constraint.
  **Why**: `pd_deflation()` is called with a specific interface; API drift goes undetected at install time.
  **Fix**: Add `pipdata (>= x.y.z)` with the version that introduced `pd_deflation(dt, pip_id, cpi, ppp, pop)`.

- **[P2.7]** [cg-reproducibility] `renv.lock` — `pipdata` absent from lockfile.
  **Why**: Contributors running `renv::restore()` get no `pipdata`; integration tests silently skip; false-green CI.
  **Fix**: Install `pipdata`, run `renv::snapshot()`, commit updated lockfile.

- **[P2.8]** [cg-data-quality] `R/load_pip_deflated_data.R:51` — No type validation on `cpi`, `ppp`, `pop`. Passing a `data.frame` or list produces an error inside `pipdata::pd_deflation()` with no attribution to the wrapper.
  **Why**: Wrapper should own its boundary validation, especially since `pipdata` is a soft dep.
  **Fix**: Add `data.table::is.data.table()` guards for each non-NULL aux arg after `rlang::check_installed()`.

- **[P2.9]** [cg-testing] `tests/testthat/test-load_pip_deflated_data.R` — `find_pip_data()` code path (country-code branch, no `id_name`) completely untested.
  **Why**: Separate code path with its own 0-row and >1-row failure modes.
  **Fix**: Add unit test mocking `find_pip_data()` returning a 1-row inventory data.table.

- **[P2.10]** [cg-testing] `tests/testthat/test-load_pip_deflated_data.R` — `as_pipgd()` (GROUP survey) branch never exercised.
  **Why**: Every test uses suffix `ALL` → `as_pipmd()`.
  **Fix**: Add test with `id_name = "BOL_2022_EH_INC_GROUP"` asserting `expect_s3_class(dt, "pipgd")` inside the pd_deflation mock.

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-testing] — `module` column branch (`assign_pipclass()` legacy-pipeline path) not tested.
  **Fix**: Add test where `load_pip_data` mock returns table with `module = "PC"` column.

- **[P3.2]** [cg-testing] — Extension stripping + `toupper()` not tested.
  **Fix**: Add test passing `id_name = "BOL_2022_EH_INC_ALL.qs2"` asserting `pip_id == "BOL_2022_EH_INC_ALL"` inside mock.

- **[P3.3]** [cg-code-quality] `tests/testthat/test-load_pip_deflated_data.R:13` — `class = "error"` in `expect_error()` is vacuous.
  **Fix**: Replace with `class = "rlang_error"` or remove the `class` argument.

- **[P3.4]** [cg-performance] `R/pip_class.R` — `data.table::copy()` inside `as_pipmd()` / `as_pipgd()` / `as_pipid()` duplicates the large survey table unnecessarily. `setattr()` already mutates by reference.
  **Fix**: Remove `data.table::copy()` from all three functions; return `invisible(x)` instead.

- **[P3.5]** [cg-documentation] `R/load_pip_deflated_data.R:22` — `@return` does not describe what deflation adds to the output.
  **Fix**: `@return A data.table with the same structure as [load_pip_data()] output, with welfare variables deflated to real values. The S3 class (\`pipmd\` or \`pipgd\`) is preserved.`

- **[P3.6]** [cg-data-quality] `R/load_pip_deflated_data.R:100` — Malformed `pip_id` with <5 tokens silently assigns `pipmd` class after extracting the wrong module segment.
  **Fix**: Emit `cli::cli_warn()` when parsed module token is not in a known set (`ALL`, `GPWG`, `HIST`, `BIN`, `GROUP`, `SYNTH`).

---

### ✅ Passed
- **cg-version-control**: No secrets, credentials, or hardcoded paths. `compound-gpid.local.md` correctly gitignored. `.cg-docs/` plan tracked correctly.
- **cg-performance**: No significant performance issues. Scalar string ops are negligible; `find_pip_data()` not double-called.
- **cg-architecture**: Placement in `pipload` with `pipdata` in Suggests is the correct call given the circular-dependency constraint.
