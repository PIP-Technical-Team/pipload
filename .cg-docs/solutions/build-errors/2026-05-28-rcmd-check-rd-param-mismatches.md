---
date: 2026-05-28
title: "R CMD check Rd warnings from mismatched @param names and malformed roxygen2"
category: "build-errors"
language: "R"
tags: [roxygen2, R-CMD-check, Rd, documentation, @param, @inheritParams, DESCRIPTION]
root-cause: "Five classes of roxygen2 authoring errors produced undocumented-argument or bad-param-name warnings: malformed @param tag, @param names that diverged from the function signature, params in shared @rdname blocks belonging to only some functions, missing @param for params that are not covered by @inheritParams, and a missing DESCRIPTION trailing newline"
severity: "P2"
---

# R CMD check Rd warnings from mismatched @param names and malformed roxygen2

## Problem

`devtools::check()` (or `R CMD check`) printed multiple warnings:

```
Warning: incomplete final line found on 'DESCRIPTION'
Warning: documented argument 'year' with no corresponding argument
Warning: documented argument 'survey' with no corresponding argument
Warning: documented argument 'filename' with no corresponding argument
Warning: documented argument 'fields' with no corresponding argument in 'load_pip_data'
Warning: undocumented arguments in 'pip_load_aux': 'maindir'
Warning: undocumented arguments in 'pip_load_all_aux': 'branch'
Warning: undocumented arguments in 'pip_merge_aux': 'branch'
Warning: undocumented arguments in 'pip_read': 'format'
```

## Root Cause

Five distinct authoring mistakes:

1. **`DESCRIPTION` missing trailing newline** — `readLines()` warns on files
   without a final `\n`. Minor but noisy.

2. **Malformed `@param` tag** (`OLD_pip_load_aux.R`):
   ```r
   #' @param character: main directory.   # ← "character" is not a param name
   ```
   The parameter name was accidentally omitted; the type annotation was
   parsed as the name.

3. **`@param` names that diverged from the function signature**
   (`load_dlw_data.R`): docs said `year`, `survey`, `filename` but the
   function uses `surveyid_year`, `survey_acronym`, `id_name`. The params
   were renamed in code but not in the roxygen block.

4. **`@param fields` in a shared `@rdname` block where the host function
   lacks that parameter** (`load_pip_data.R`): `fields` was documented in
   the block for `load_pip_data()`, which does not have a `fields` argument.
   It belongs only on `load_pip_release_inventory` and
   `load_pip_master_inventory`.

5. **Params not reachable via `@inheritParams` but present in signature**:
   - `branch` in `pip_load_all_aux()` and `pip_merge_aux()` — not in the
     `pip_load_aux()` they `@inheritParams` from.
   - `format` in `pip_read()` — not documented and not inherited.

## Solution

### 1. DESCRIPTION trailing newline
Ensure the file ends with `\n`. In most editors: add a blank line at the end.

### 2. Malformed `@param`
```r
# Before
#' @param character: main directory.

# After
#' @param maindir character: main directory.
```

### 3. @param names diverged from signature
Update the roxygen block to match the current parameter names exactly:
```r
# Before
#' @param year numeric: four digit year
#' @param survey character: survey acronym
#' @param filename character: File name

# After
#' @param surveyid_year numeric: four digit year
#' @param survey_acronym character: survey acronym
#' @param id_name character: full survey id name
```

### 4. @param in shared @rdname block for a param only some functions have
Move the `@param` tag out of the shared block and place it directly above each
function that actually has the parameter:
```r
# load_pip_release_inventory
#' @param fields Character vector of metadata field names ...
#' @return data.table with PIP inventory for the current release
#' @rdname load_pip_data
load_pip_release_inventory <- ...

# load_pip_master_inventory
#' @param fields Character vector of metadata field names ...
#' @return data.table with PIP master inventory
#' @rdname load_pip_data
load_pip_master_inventory <- ...
```

### 5. Params not covered by @inheritParams
Add explicit `@param` for each parameter that `@inheritParams` does not reach:
```r
#' @param branch character: data branch. One of "DEV", "PROD", "main".
#' @param format Character. File format, e.g. "qs2".
```

## Prevention

- **After renaming any function parameter**, search its roxygen block for the
  old name and update it. `devtools::check()` will catch it, but it's faster
  to catch it at rename time.
- **Shared `@rdname` blocks**: only put `@param` entries in the shared block
  if *every* function in that `@rdname` group has that parameter.
- **`@inheritParams`**: it only pulls params that exist in the referenced
  function. Any param added to the inheriting function but not in the source
  function requires an explicit `@param` tag.
- **Run `devtools::document()` after every roxygen edit** and check the output
  for `Writing` lines — if a file that should not have changed is rewritten,
  inspect why.
- **DESCRIPTION**: always end the file with a newline.

## Related

- [testing-patterns/2026-05-28-expect-warning-without-expect-error-swallows-error.md](../testing-patterns/2026-05-28-expect-warning-without-expect-error-swallows-error.md) — companion fix from the same `R CMD check` run
