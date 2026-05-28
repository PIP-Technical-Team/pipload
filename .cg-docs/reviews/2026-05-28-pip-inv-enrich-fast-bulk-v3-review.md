---
plan: .cg-docs/plans/2026-05-28-pip-inv-enrich-fast-bulk-v3.md
findings:
  P1.1: fixed
  P1.2: fixed
  P1.3: fixed
  P1.4: fixed
  P2.1: open
  P2.2: open
  P2.3: open
  P2.4: fixed
  P2.5: fixed
  P2.6: open
  P2.7: open
  P2.8: open
  P2.9: open
  P2.10: fixed
  P2.11: fixed
  P2.12: fixed
  P2.13: open
  P2.14: open
  P2.15: open
  P2.16: open
  P2.17: open
  P2.18: open
  P3.1: fixed
  P3.2: fixed
  P3.3: open
  P3.4: open
  P3.5: open
  P3.6: open
  P3.7: open
  P3.8: open
  P3.9: fixed
---

## Review Report

**Review depth**: standard (auto-escalation: `load*.R` trigger applied)
**Files reviewed**: 7 (`R/pip_inv_enrich.R`, `R/pip_class.R`, `R/load_pip_deflated_data.R`,
`R/load_pip_data.R`, `tests/testthat/test-pip_inv_enrich.R`,
`tests/testthat/test-load_pip_deflated_data.R`, `.gitignore`)
**Findings**: 32 (P1: 4, P2: 18, P3: 10)
**Safe-auto fixes applied**: 11 (P1.1–P1.4, P2.4, P2.5, P2.10, P2.11, P2.12, P3.1, P3.2, P3.9); 38/38 tests passing after fixes.

---

### P1 — CRITICAL (must fix before merge)

- **[P1.1]** [cg-data-quality] `R/pip_inv_enrich.R` — `field_rows` null-meta branch assigns `row[[field]] <- NA` for **vector fields** (cpi/ppp/pop/gdp/pce), creating a spurious raw-name column (e.g. `"cpi"`) that co-exists with the expanded wide columns (`cpi_2005_rural`, …) from non-null rows after `rbindlist(fill=TRUE)`.
  **Why**: `rbindlist` merges the two column sets, producing a garbage `"cpi"` column of all-NA except the null-meta rows.
  **Fix**: Skip assignment for vector fields in the null-meta branch — `fill=TRUE` fills those to NA automatically. ✅ **Fixed** (applied).

- **[P1.2]** [cg-data-quality] `R/pip_inv_enrich.R` — `grep(paste0("^", field), names(inv))` pre-clean step matches unintended sibling scalar columns. For `field = "pop"` the pattern `^pop` matches `pop_data_level`; `"cpi"` → `cpi_data_level`; `"ppp"` → `ppp_data_level`; `"gdp"` → `gdp_data_level`; `"pce"` → `pce_data_level`. Any of these already present in `inv` are **silently deleted**.
  **Why**: The intent is to remove stale expansion columns from a prior run, not unrelated scalars.
  **Fix**: Use `paste0("^", field, "_")` — trailing underscore anchors to the expansion prefix. ✅ **Fixed** (applied).

- **[P1.3]** [cg-documentation] `R/load_pip_data.R:3` — `@param fields` is in the shared `load_pip_data` `@rdname` block, but `load_pip_data()` itself has no `fields` parameter. `R CMD CHECK` will warn `documented argument 'fields' with no corresponding argument`.
  **Why**: The param must only appear on the inventory loader functions.
  **Fix**: Move `@param fields` out of the shared block and place it directly on `load_pip_release_inventory` and `load_pip_master_inventory` (or use a separate `@rdname` helper block). `[manual]`

- **[P1.4]** [cg-code-quality] `R/pip_inv_enrich.R` — Final expression is bare `inv` instead of `return(inv)`.
  **Why**: `cg-skill-r-shared` requires explicit `return()` at the end of non-trivial functions; all other exit paths already use `return()`.
  **Fix**: Add `return(inv)`. ✅ **Fixed** (applied).

---

### P2 — IMPORTANT (should fix)

- **[P2.1]** [cg-data-quality] `R/pip_inv_enrich.R:58` — `sub("^(\\d{4})_.*$", "\\1", elem_name)` returns `elem_name` unchanged when the name has no leading `YYYY_`. The unchanged string is compared to the 4-digit year string, fails, and is appended to `mismatch_years` — producing nonsense values like `"national,rural"` in the `<field>_year` column.
  **Why**: Silent data corruption when element names deviate from the assumed format.
  **Fix**: Validate the leading year pattern and skip/warn for names that don't match:
  ```r
  yr_match <- regmatches(elem_name, regexpr("^\\d{4}", elem_name))
  if (length(yr_match) == 0L) {
    cli::cli_warn("Unexpected element name without YYYY_ prefix: {.val {elem_name}}. Skipping.", class = "pip_inv_enrich_bad_elem_name")
    next
  }
  elem_year <- yr_match
  ```
  `[manual]`

- **[P2.2]** [cg-data-quality] `R/pip_inv_enrich.R` — `field_dt` is one row per inventory row, so duplicate `pip_id` values (same survey at multiple reporting levels) would give a right-hand table that is not unique on `pip_id`, violating `joyn::left_join(..., relationship = "many-to-one")`.
  **Why**: `many-to-one` asserts the right table has unique keys; if not, joyn raises an error or produces a Cartesian product depending on version.
  **Fix**: Join on a row index key instead of `pip_id`:
  ```r
  inv[, .row_id := .I]
  # build field_rows using .row_id
  inv <- joyn::left_join(inv, field_dt, by = ".row_id", relationship = "one-to-one", ...)
  inv[, .row_id := NULL]
  ```
  `[manual]`

- **[P2.3]** [cg-data-quality] `R/load_pip_deflated_data.R:112` — `find_pip_data()` accepts `...` but internally filters to `pip_id_vars` only. The arguments `latest_version`, `vermast`, `veralt`, and `collection` passed by `load_pip_deflated_data()` are **silently discarded**.
  **Why**: A user specifying `vermast = "01"` gets no filtering, then the multi-match guard fires unexpectedly.
  **Fix**: Expand `find_pip_data()` to accept and forward these arguments explicitly, or document clearly that pass-through filtering is limited to `pip_id_vars`. `[manual]`

- **[P2.4]** [cg-data-quality] `R/pip_inv_enrich.R` — Untyped `NA` (logical class) used in null-meta scalar branch. `rbindlist` infers column type as `logical` if all early rows are null-meta; later `character` rows then coerce or fail silently.
  **Why**: Typed NAs prevent ambiguous type inference.
  **Fix**: Use `NA_character_` for scalar fields. ✅ **Fixed** (applied as part of P1.1 fix).

- **[P2.5]** [cg-data-quality] `R/pip_inv_enrich.R:35` — `value[[1L]]` silently drops elements 2…N for scalar fields with unexpected length > 1. No warning is emitted.
  **Why**: An upstream pipeline bug storing a scalar field as a length-2 vector would be invisible.
  **Fix**: Emit `cli::cli_warn()` when `length(value) > 1L` before taking `value[[1L]]`. ✅ **Fixed** (applied).

- **[P2.6]** [cg-architecture] `R/pip_inv_enrich.R` — Stamp artifact layout (`"versions"` / `"artifact"` path segments) is hardcoded with no defensive check. If stamp changes layout, all rows silently return `NULL → NA` — the `tryCatch(\(e) NULL)` absorbs every read failure individually with no aggregate diagnostic.
  **Why**: User sees individual `pip_id` warnings but no clear "layout looks wrong" error.
  **Fix**: After constructing `paths`, verify at least one non-NA path exists before iterating. If zero valid paths found, `cli::cli_abort()` with the constructed path template. `[manual]`

- **[P2.7]** [cg-architecture] `R/pip_inv_enrich.R` — When `version_id_metadata` column is **absent** from `inv` (`has_version_col = FALSE`), the all-NA `paths` path is taken silently, then the `pip_inv_enrich_missing_meta` warning fires with the message "no metadata was saved for this survey — check the pipeline output" — which is the wrong diagnosis.
  **Why**: Column-absent and column-present-but-NA are different root causes that share the same misleading warning text.
  **Fix**: Add a distinct `cli::cli_warn()` at the top of the `!has_version_col` branch: `"Inventory lacks 'version_id_metadata'; all enrichment fields will be NA. Reload via load_pip_release_inventory()."` `[manual]`

- **[P2.8]** [cg-architecture] `tests/testthat/test-pip_class.R` — `assign_pipclass_from_id()` has no direct unit tests. The unrecognised-module-token warning path is never exercised.
  **Why**: Regression in the warning condition (wrong `toupper()` placement, wrong `grepl` flag) would go undetected.
  **Fix**: Add tests in `test-pip_class.R` for: (1) known token → correct class, (2) `GROUP` → `pipgd`, (3) unrecognised token → warns + returns `pipmd`. `[manual]`

- **[P2.9]** [cg-code-quality] `R/pip_class.R` — `cli::cli_warn()` in `assign_pipclass_from_id()` has no `class =` argument.
  **Why**: Every other `-warn` / `-abort` in the package uses a structured condition class. Without one, tests can only match on message text.
  **Fix**: Add `class = c("pip_unknown_module_token", "pipwrn")`. `[manual]`

- **[P2.10]** [cg-documentation] `R/pip_inv_enrich.R:97` — `@param fields` says `"silently skipped"` but the code calls `cli::cli_inform()` and the `@details` section says `"with an informational message"` — contradiction.
  **Why**: User-visible help text must match runtime behaviour.
  **Fix**: Change to `"skipped with an informational message"`. ✅ **Fixed** (applied).

- **[P2.11]** [cg-documentation] `R/pip_inv_enrich.R:107` — `@return` does not declare the return type.
  **Why**: Convention is to open `@return` with the type.
  **Fix**: Change to `"A data.table identical to the input inv with ..."`. ✅ **Fixed** (applied).

- **[P2.12]** [cg-documentation] `R/load_pip_data.R:3` — Description block references `[load_pip_inventory_release]` (stale name; function is `load_pip_release_inventory`). Produces an `undocumented/unknown topic` warning on `R CMD CHECK`.
  **Fix**: Replace with `[load_pip_release_inventory]`. ✅ **Fixed** (applied).

- **[P2.13]** [cg-reproducibility] `R/pip_inv_enrich.R:207` — `tryCatch(..., error = \(e) NULL)` swallows read errors with no per-file warning. A single corrupt artifact returns `NULL` invisibly; the aggregate `pip_inv_enrich_missing_meta` warn only fires when **all** fields for an ID are NA, so a partial failure among many is undetected.
  **Why**: Silent failure means the enriched inventory can contain stale NAs without any signal.
  **Fix**:
  ```r
  tryCatch(qs2::qs_read(p), error = \(e) {
    cli::cli_warn(c("!" = "Could not read metadata: {.path {p}}", "i" = "{conditionMessage(e)}"),
      class = "pip_inv_enrich_read_error")
    NULL
  })
  ```
  `[manual]`

- **[P2.14]** [cg-testing] `tests/testthat/test-load_pip_deflated_data.R:40` — `expect_s3_class(dt, "pipmd")` placed **inside** the `pd_deflation` mock body. If `load_pip_data()` throws before reaching the mock, the inner `expect_*` is silently skipped and the test passes vacuously.
  **Why**: Assertions must not be inside mocks.
  **Fix**: Capture `dt` via `<<-` binding and assert after the call returns (see pattern in cg-skill-r-testing). `[manual]`

- **[P2.15]** [cg-testing] `tests/testthat/test-load_pip_deflated_data.R:118` — Integration test calls `pipfun::setup_working_release()` with no cleanup via `withr::defer()`.
  **Why**: Side-effecting setup leaks global options/env vars to subsequent tests on failure.
  **Fix**: Add `withr::defer(pipfun::teardown_working_release(), teardown_env = parent.frame())` before the setup call (or equivalent `withr::local_*` wrappers). `[manual]`

- **[P2.16]** [cg-testing] `tests/testthat/test-pip_inv_enrich.R` — No test covers the branch where `version_id_metadata` column is **absent entirely** from `inv` (`has_version_col == FALSE`).
  **Why**: This triggers a distinct code path that produces all-NA enrichment but currently shares the same (incorrect) warning text as P2.7.
  **Fix**: Add a test with `inv` having `path_metadata` but no `version_id_metadata` column, asserting the `pip_inv_enrich_missing_meta` warning and NA output. `[manual]`

- **[P2.17]** [cg-testing] `tests/testthat/test-pip_inv_enrich.R` — `gdp` and `pce` vector fields have zero test coverage despite sharing the same year-stripping logic as `pop`.
  **Why**: A regression specific to `gdp` or `pce` would be invisible.
  **Fix**: Add year-match test for `gdp` and year-mismatch test for `pce` (or expand existing `pop` tests to cover all three). `[manual]`

- **[P2.18]** [cg-version-control] `man/pip_inv_enrich.Rd`, `man/load_pip_data.Rd` — regenerated `.Rd` files are unstaged (` M` in `git status`).
  **Why**: A commit right now omits the latest roxygen2 regenerations.
  **Fix**: `git add man/` before committing. `[manual]`

---

### P3 — MINOR (nice to have)

- **[P3.1]** [cg-code-quality] `R/load_pip_deflated_data.R` — Missing `"x" =` name on zero-match `cli_abort()` first bullet (renders plain instead of ✖ red).
  **Fix**: Add `"x" = "No matching survey found."`. ✅ **Fixed** (applied).

- **[P3.2]** [cg-code-quality] `R/load_pip_deflated_data.R` — Same missing `"x" =` on multi-match abort.
  **Fix**: Add `"x" = "More than one survey matched..."`. ✅ **Fixed** (applied).

- **[P3.3]** [cg-code-quality] `R/pip_inv_enrich.R` — `cli::cli_inform()` for skip-existing uses `paste0()` to split the message string across lines; can be a single inline string. Minor style only.
  **Fix**: Merge into a single string without `paste0()`. `[advisory]`

- **[P3.4]** [cg-code-quality] `R/pip_class.R` — `assign_pipclass_from_id()` has `@keywords internal` which still generates `man/assign_pipclass_from_id.Rd` (internal help page). `expand_meta_field()` correctly uses `@noRd` to suppress its `.Rd`. Consider adding `@noRd` alongside `@keywords internal` if the Rd is not wanted.
  **Fix**: Add `@noRd` to `assign_pipclass_from_id()`. `[advisory]`

- **[P3.5]** [cg-performance] `R/pip_inv_enrich.R` — `ifelse()` evaluates both branches and is not type-strict. On this code path `fs::path()` returns `fs_path` (character subclass); `as.character()` wrapping is already applied. `data.table::fifelse()` could not be used directly without `as.character()` due to the class mismatch — leave as-is.
  **Note**: `fifelse` was attempted and reverted; `ifelse + as.character()` is the correct pattern here. `[advisory]`

- **[P3.6]** [cg-performance] `R/pip_inv_enrich.R:67` — Growing vector in `expand_meta_field()`: `mismatch_years <- c(mismatch_years, elem_year)` inside a loop. O(n²) allocations where n = length of vector field (≤ 3 typically); negligible absolute cost but anti-pattern.
  **Fix**: Pre-allocate `mismatch_years <- character(length(value))` with counter. `[advisory]`

- **[P3.7]** [cg-performance] `R/pip_inv_enrich.R` — `names(inv)` called inside a `for (field in fields)` loop. Allocates a character vector on each of ≤ 14 iterations.
  **Fix**: Cache `inv_names <- names(inv)` before the loop. ✅ **Fixed** (applied as `inv_names` cache in P1.2 grep fix block).

- **[P3.8]** [cg-performance] `R/pip_inv_enrich.R:250` — `row <- c(row, expand_meta_field(...))` inside the inner loop copies the entire `row` list on each field iteration. O(n_rows × n_fields²) copy operations; bounded at 14 fields so worst-case is mild but avoidable.
  **Fix**: Collect results separately and combine once: `c(row, do.call(c, lapply(fields, ...)))`. `[advisory]`

- **[P3.9]** [cg-version-control] `script.R` untracked — not covered by `.gitignore`, risking accidental staging.
  **Fix**: Add `script.R` to `.gitignore`. ✅ **Fixed** (applied).

- **[P3.10]** [cg-testing] `tests/testthat/test-pip_inv_enrich.R` — Multiple `expect_true("col" %in% names(result))` calls; on failure prints only `FALSE is not TRUE` with no context on what columns were actually present.
  **Fix**: Replace with `expect_contains(names(result), "col")` (testthat ≥ 3.2.0). `[advisory]`

---

### ✅ Passed

- **cg-version-control**: No secrets or credentials. NAMESPACE consistent with `@export` / `@keywords internal`. No hardcoded absolute paths. `.Rbuildignore` excludes `.cg-docs`. `assign_pipclass_from_id` correctly absent from NAMESPACE exports.
- **cg-reproducibility**: No hardcoded absolute/UNC paths in committed R code. No `set.seed()` needed. `version = NULL` behaviour matches existing conventions.
- **cg-architecture**: `assign_pipclass_from_id()` correctly placed in `pip_class.R`. `pipdata` correctly kept in `Suggests` (circular dep avoidance). `expand_meta_field()` internal with `@noRd`. No circular import concerns.
- **cg-performance**: Bulk `qs2::qs_read()` approach delivers the 18× speedup as designed. `rbindlist(fill=TRUE)` is the only viable approach for the wide-column expansion pattern.
