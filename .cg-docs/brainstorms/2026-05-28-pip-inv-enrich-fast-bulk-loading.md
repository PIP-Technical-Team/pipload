---
date: 2026-05-28
title: "Fast bulk metadata enrichment for pip_inv_enrich"
status: decided
scope: "Standard"
chosen-approach: "Direct qs2 bulk loading with wide-column expansion"
tags: [performance, metadata, inventory, pip_inv_enrich]
---

# Fast bulk metadata enrichment for pip_inv_enrich

## Context

`pip_inv_enrich()` is called by `load_pip_release_inventory()` and
`load_pip_master_inventory()` when `fields` is non-empty. The current
implementation loops through all ~4146 rows one by one, calling `pip_read()`
(which goes through stamp's version resolution) at ~90ms per artifact.
Total: ~6 minutes for a full inventory enrichment.

Direct `qs2::qs_read()` bypasses stamp and costs ~5ms per file, giving an 18x
speedup.

## Requirements

1. **Fast path**: Read metadata files directly via `qs2::qs_read()` using
   versioned paths constructed from `inv$path_metadata` +
   `inv$version_id_metadata` (layout: `path/versions/vid/artifact`).
2. **NA for missing metadata**: If `version_id_metadata` is `NA`, the survey
   has no metadata — return `NA` for all requested fields. Document that users
   should investigate why.
3. **Wide-column expansion for vector fields**:
   - `cpi`, `ppp`: prefix + element name → columns like `cpi_2005_rural`,
     `ppp_2011_02_02_national`. Detect if element names already contain the
     field prefix (e.g. `ppp_2005_...`) and avoid doubling it.
   - `pop`, `gdp`, `pce`: strip the `YYYY_` year prefix from element names
     (it equals `surveyid_year`) and use `field_area` → `pop_rural`,
     `gdp_national`, `pce_urban`.
   - Scalar fields (`reporting_level`, `welfare_type`, etc.): single column.
4. **No overwrite**: Skip fields that already exist as columns in `inv`.
5. **Sequential** for now (no parallelism dependency).
6. **Warning on row expansion**: Not applicable (wide columns, no extra rows).

## Approaches Considered

### Approach 1: Direct qs2 bulk loading (chosen)

Rewrite `pip_inv_enrich` to construct file paths from inventory columns and
use `qs2::qs_read()` in a simple `lapply`. Field expansion logic handles
scalar vs named-vector fields with smart prefix deduplication.

- **Pros**: 18x faster, self-contained (one file), no new dependencies
- **Cons**: Bypasses stamp version resolution (acceptable — inventory pins
  versions); relies on stamp internal file layout
- **Effort**: Small-medium

### Approach 2: pip_read_bulk wrapper around stamp

Create batch wrapper that still delegates to stamp but suppresses overhead.

- **Pros**: Keeps stamp as single source of truth
- **Cons**: Still ~50ms/file, doesn't fix core bottleneck
- **Effort**: Medium

### Approach 3: Pre-compute enrichment at pipeline write-time

Embed metadata fields directly into the inventory artifact during pipeline.

- **Pros**: Zero runtime cost
- **Cons**: Requires cross-repo pipeline changes, not available now
- **Effort**: Large

## Decision

Approach 1 — direct `qs2::qs_read()` bulk loading with wide-column expansion.
Pragmatic fix that exploits version IDs already present in the inventory.

## Next Steps

1. Rewrite `pip_inv_enrich()` in `R/pip_inv_enrich.R`
2. Update roxygen documentation to reflect new behavior (wide columns,
   NA semantics, no-overwrite rule)
3. Update/add tests for vector field expansion and prefix dedup
4. Add "parallel enrichment" idea to roadmap for future iteration
