# Project Context

Additional context for Copilot and the Compound GPID plugin. Edit freely —
this file is committed to git and shared with the team.

## Data Sources
<!-- Where does data come from? File paths, databases, APIs, vintage conventions -->

## Domain Rules
<!-- Project-specific rules that Copilot should always follow -->

### pip_inv_enrich — rbindlist vector-field key-naming rule

When building per-row output lists for `rbindlist(fill=TRUE)` in
`pip_inv_enrich()`, **never emit the raw field name as a list key for vector
fields** (`cpi`, `ppp`, `pop`, `gdp`, `pce`). These fields expand into wide
columns (e.g. `cpi_2011_national`) via `expand_meta_field()`. Emitting
`list(cpi = NA)` for an absent or null-metadata row creates a spurious `cpi`
logical column alongside the expanded wide columns. Return `list()` instead —
`rbindlist(fill=TRUE)` fills the expanded columns with `NA` automatically.
This rule applies to **every** branch in the expansion loop (null-metadata,
absent-field, and any future early-return). See
`.cg-docs/solutions/data-quality/2026-05-28-rbindlist-fill-vector-field-key-naming.md`.

## Work in Progress
<!-- Modules, features, or migrations currently underway -->

## Workspace Notes
<!-- Related folders, dependencies on other projects in the VS Code workspace -->

## R CMD check Conventions

### roxygen2 authoring rules (to avoid Rd warnings)

- `@param` names must match the function signature exactly — update the docs
  when renaming a parameter.
- Never put `@param X` in a shared `@rdname` block if only a subset of
  functions in that group have parameter `X`; place it on each function
  individually.
- `@inheritParams source_fn` only pulls params defined in `source_fn`. Params
  unique to the inheriting function need an explicit `@param`.
- `DESCRIPTION` must end with a trailing newline.
- See `.cg-docs/solutions/build-errors/2026-05-28-rcmd-check-rd-param-mismatches.md`.

### testthat: always chain expect_error() after expect_warning() when the function can also error

`expect_warning()` muffles the warning but re-throws any subsequent error.
In `R CMD check` (no working release configured), most `pip_*` functions abort
after the deprecation warning fires. Pattern:

```r
fn(deprecated_arg = "value") |> expect_warning() |> expect_error()
```

See `.cg-docs/solutions/testing-patterns/2026-05-28-expect-warning-without-expect-error-swallows-error.md`.
