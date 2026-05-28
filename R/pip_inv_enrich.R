# Valid metadata fields that can be requested for inventory enrichment.
# Scalars: single-value character fields.
# Vectors: named numeric vectors expanded into wide columns.
.PIP_META_VALID_FIELDS <- c(
  "gd_type", "reporting_level", "ppp_data_level", "cpi_data_level",
  "gdp_data_level", "pce_data_level", "pop_data_level", "aux_data_levels",
  "distribution_type",
  "cpi", "ppp", "pop", "gdp", "pce"
)

# Subset of valid fields that are named numeric vectors requiring expansion.
.PIP_META_VECTOR_FIELDS <- c("cpi", "ppp", "pop", "gdp", "pce")


#' Expand one metadata field value into a named list of inventory columns
#'
#' Internal helper. Converts a single field value from a metadata list into
#' a named list suitable for row-binding into the enrichment data.table.
#'
#' @param field_name Character scalar. One of `.PIP_META_VECTOR_FIELDS` or a
#'   scalar field name.
#' @param value The raw value from the metadata list.
#' @param surveyid_year Numeric scalar. Used for year-stripping of
#'   `pop`/`gdp`/`pce` element names.
#'
#' @return A named list: one element per output column.
#' @noRd
expand_meta_field <- function(field_name, value, surveyid_year) {
  # Scalar fields: single column.
  # Warn if the field unexpectedly holds more than one element — only the
  # first is used; the rest are silently dropped by the pipeline writer.
  if (!field_name %in% .PIP_META_VECTOR_FIELDS) {
    if (length(value) > 1L) {
      cli::cli_warn(
        c(
          "Field {.field {field_name}} has {length(value)} element{?s}; using first only.",
          "i" = "This may indicate an upstream pipeline issue."
        ),
        class = "pip_inv_enrich_scalar_truncated"
      )
    }
    return(setNames(list(value[[1L]]), field_name))
  }

  nm <- names(value)

  # ppp: element names already carry the ppp_ prefix (e.g. ppp_2011_02_02_national)
  if (field_name == "ppp") {
    return(setNames(as.list(value), nm))
  }

  # cpi: prefix element names with cpi_ (e.g. 2005_rural → cpi_2005_rural)
  if (field_name == "cpi") {
    return(setNames(as.list(value), paste0("cpi_", nm)))
  }

  # pop / gdp / pce: year-aware stripping.
  # Element names are like "2022_rural" or "2019_national".
  # Strip the leading YYYY_ only when it matches surveyid_year; otherwise keep
  # the full element name and add a <field>_year column with the mismatched year.
  yr_str <- as.character(surveyid_year)
  out <- list()
  mismatch_years <- character(length(value))
  mismatch_count <- 0L

  for (i in seq_along(value)) {
    elem_name <- nm[[i]]
    elem_year <- sub("^(\\d{4})_.*$", "\\1", elem_name)
    # If sub returned the string unchanged, the YYYY_ prefix is absent — skip.
    if (elem_year == elem_name) {
      cli::cli_warn(
        "Unexpected element name without YYYY_ prefix: {.val {elem_name}}. Skipping.",
        class = "pip_inv_enrich_bad_elem_name"
      )
      next
    }
    area_part <- sub("^\\d{4}_", "", elem_name)

    if (elem_year == yr_str) {
      # Year matches: strip year, prefix with field name
      col_nm <- paste0(field_name, "_", area_part)
    } else {
      # Year differs: keep full element name, prefix with field name
      col_nm <- paste0(field_name, "_", elem_name)
      mismatch_count <- mismatch_count + 1L
      mismatch_years[[mismatch_count]] <- elem_year
    }
    out[[col_nm]] <- value[[i]]
  }

  # Add <field>_year column when at least one element had a mismatched year
  if (mismatch_count > 0L) {
    out[[paste0(field_name, "_year")]] <- paste(
      unique(mismatch_years[seq_len(mismatch_count)]),
      collapse = ","
    )
  }

  return(out)
}


#' Enrich a PIP inventory with fields from per-survey metadata
#'
#' For each `pip_id` in `inv`, loads the corresponding metadata artifact
#' directly from its versioned path on disk (bypassing stamp version
#' resolution) and expands the requested fields as new columns. Only fields
#' in the whitelist are accepted; see `fields` below.
#'
#' This function is called automatically by [load_pip_release_inventory()] and
#' [load_pip_master_inventory()] when their `fields` argument is non-empty.
#' By default no enrichment is performed.
#'
#' @param inv A `data.table` containing the PIP inventory. Must have `pip_id`
#'   and `path_metadata` columns. The `path_metadata` column is present in
#'   inventories loaded via [load_pip_release_inventory()] or
#'   [load_pip_master_inventory()]. If it is absent, the function aborts with
#'   an informative error.
#'
#'   If a `version_id_metadata` column is present, the exact versioned artifact
#'   is read (`path_metadata/versions/<version_id>/artifact`). Rows where
#'   `version_id_metadata` is `NA` have no saved metadata and receive `NA` for
#'   all requested fields.
#'
#' @param fields Character vector of metadata field names to add as columns.
#'   Fields already present as columns in `inv` are skipped with an
#'   informational message (no overwrite). Must be a subset of the following
#'   valid fields:
#'
#'   **Scalar fields** (add one column each):
#'   `"gd_type"`, `"reporting_level"`, `"ppp_data_level"`, `"cpi_data_level"`,
#'   `"gdp_data_level"`, `"pce_data_level"`, `"pop_data_level"`,
#'   `"aux_data_levels"`, `"distribution_type"`
#'
#'   **Vector fields** (expand into multiple wide columns):
#'   - `"cpi"` — columns named `cpi_<YYYY>_<area>` (e.g. `cpi_2005_rural`)
#'   - `"ppp"` — columns use existing names as-is (e.g. `ppp_2011_02_02_national`)
#'   - `"pop"`, `"gdp"`, `"pce"` — columns named `<field>_<area>` when the
#'     embedded year matches `surveyid_year` (e.g. `pop_rural`); when the year
#'     differs, the full element name is kept (`pop_2019_national`) and a
#'     `<field>_year` column is added.
#'
#' @return A `data.table` identical to the input `inv` with the requested
#'   columns appended. The number of rows is unchanged. Vector fields may add many columns (up to one per
#'   unique area × year combination across all surveys); surveys lacking a
#'   particular element receive `NA`.
#'
#' @note
#' When `version_id_metadata` is `NA` for a survey, it means no metadata
#' artifact was saved for it during the pipeline run. This is unexpected and
#' should be investigated — check the pipeline output for that `pip_id` to
#' understand why it was not produced.
#'
#' @details
#' **Fast path**: metadata is read directly via `qs2::qs_read()` using the
#' versioned file path from the inventory, avoiding stamp's per-artifact
#' version resolution overhead (~5ms/file vs ~90ms/file).
#'
#' **No overwrite**: fields found as existing columns in `inv` are skipped
#' with an informational message. To force refresh, remove the columns first.
#'
#' @seealso [load_pip_release_inventory()], [load_pip_master_inventory()]
#' @family inventory
#' @export
#'
#' @examples
#' \dontrun{
#' inv <- load_pip_master_inventory(fields = "reporting_level")
#'
#' # Enrich after loading with a vector field:
#' raw_inv <- load_pip_master_inventory()
#' enriched <- pip_inv_enrich(raw_inv, fields = c("reporting_level", "cpi"))
#' # Results in columns: reporting_level, cpi_2005_rural, cpi_2011_urban, ...
#' }
pip_inv_enrich <- function(inv, fields = character(0)) {
  if (length(fields) == 0L) {
    return(inv)
  }

  # Validate fields against whitelist
  invalid <- setdiff(fields, .PIP_META_VALID_FIELDS)
  if (length(invalid) > 0L) {
    valid_fields <- .PIP_META_VALID_FIELDS
    cli::cli_abort(
      c(
        "Invalid {cli::qty(length(invalid))} field{?s} in {.arg fields}: {.val {invalid}}.",
        "i" = "Valid fields are: {.val {valid_fields}}"
      ),
      class = c("pip_inv_enrich_invalid_field", "piperr")
    )
  }

  # Require path_metadata column
  if (!"path_metadata" %in% names(inv)) {
    cli::cli_abort(
      c(
        "{.arg inv} must have a {.col path_metadata} column.",
        "i" = "Run {.code pipfun::setup_working_release()} and reload the inventory."
      ),
      class = c("pip_inv_enrich_no_path_metadata", "piperr")
    )
  }

  # Skip fields already present as columns in inv
  already_present <- intersect(fields, names(inv))
  if (length(already_present) > 0L) {
    cli::cli_inform(
      c(
        "i" = "Skipping {length(already_present)} field{?s} already present in {.arg inv}: {.val {already_present}}."
      ),
      class = "pip_inv_enrich_skip_existing"
    )
    fields <- setdiff(fields, already_present)
    if (length(fields) == 0L) {
      return(inv)
    }
  }

  has_version_col <- "version_id_metadata" %in% names(inv)

  # Build versioned file paths: path_metadata/versions/version_id/artifact.
  # Rows with NA version_id_metadata have no metadata → path stays NA.
  if (has_version_col) {
    paths <- ifelse(
      is.na(inv$version_id_metadata),
      NA_character_,
      as.character(fs::path(
        inv$path_metadata,
        "versions",
        inv$version_id_metadata,
        "artifact"
      ))
    )
  } else {
    cli::cli_warn(
      c(
        "Inventory lacks {.col version_id_metadata}; all enrichment fields will be NA.",
        "i" = "Reload via {.code load_pip_release_inventory()} or {.code load_pip_master_inventory()}."
      ),
      class = c("pip_inv_enrich_no_version_col", "pipwrn")
    )
    paths <- rep(NA_character_, nrow(inv))
  }

  # Bulk-read metadata files directly (fast path: no stamp version resolution)
  metas <- lapply(paths, \(p) {
    if (is.na(p)) {
      return(NULL)
    }
    tryCatch(qs2::qs_read(p), error = \(e) {
      cli::cli_warn(
        c(
          "!" = "Could not read metadata: {.path {p}}",
          "i" = "{conditionMessage(e)}"
        ),
        class = c("pip_inv_enrich_read_error", "pipwrn")
      )
      NULL
    })
  })

  # Defensive check: if non-NA paths exist but all reads failed, the
  # P2.6: Detect layout issues.  Only abort when a path EXISTS on disk
  # but could not be read — that indicates a stamp layout change.  Individual
  # missing artifacts (file simply not there) are normal; those are reported
  # by the per-file pip_inv_enrich_read_error warning and produce NA rows.
  non_na_paths <- sum(!is.na(paths))
  if (non_na_paths > 0L) {
    extant_idx <- which(!is.na(paths) & file.exists(paths))
    if (
      length(extant_idx) > 0L &&
        !any(vapply(metas[extant_idx], Negate(is.null), logical(1L)))
    ) {
      example_path <- paths[[extant_idx[[1L]]]]
      cli::cli_abort(
        c(
          "x" = "{length(extant_idx)} metadata file{?s} found on disk but none could be read.",
          "i" = "Example path: {.path {example_path}}",
          "i" = "Check that the stamp artifact layout has not changed."
        ),
        class = c("pip_inv_enrich_layout_error", "piperr")
      )
    }
  }

  # Add a row index so the join key is always unique (pip_id may repeat
  # across reporting levels).
  inv[, .row_id := .I]

  # For each row, expand requested fields into named list of columns
  field_rows <- lapply(seq_len(nrow(inv)), \(i) {
    meta <- metas[[i]]
    row <- list(.row_id = i, pip_id = inv$pip_id[[i]])

    if (is.null(meta)) {
      # No metadata available: NA for scalar fields only.
      # Vector fields are intentionally omitted so rbindlist(fill=TRUE) fills
      # those rows with NA rather than creating a spurious raw-field-name column
      # that conflicts with the expanded wide columns (e.g. "cpi" vs "cpi_2005_rural").
      for (field in fields) {
        if (!field %in% .PIP_META_VECTOR_FIELDS) {
          row[[field]] <- NA_character_
        }
      }
      return(row)
    }

    yr <- meta$surveyid_year

    # Collect all field expansions then combine once to avoid O(n²) list copies.
    extra <- lapply(fields, \(field) {
      val <- meta[[field]]
      if (is.null(val)) {
        # Field absent from this survey's metadata.
        # For scalar fields: emit a named NA so the column appears in every row.
        # For vector fields: omit entirely — rbindlist(fill=TRUE) will fill the
        # expanded wide columns (cpi_YYYY_area etc.) with NA automatically.
        # Emitting list(cpi = NA) here would create a spurious raw 'cpi' column
        # that conflicts with the expanded columns from other rows.
        if (field %in% .PIP_META_VECTOR_FIELDS) {
          return(list())
        }
        setNames(list(NA_character_), field)
      } else {
        expand_meta_field(field, val, yr)
      }
    })
    return(c(row, do.call(c, extra)))
  })

  field_dt <- data.table::rbindlist(field_rows, fill = TRUE)

  # Warn about pip_ids where no metadata could be loaded (all new cols are NA)
  enriched_cols <- setdiff(names(field_dt), c("pip_id", ".row_id"))
  if (length(enriched_cols) > 0L) {
    failed <- field_dt[
      rowSums(!is.na(field_dt[, enriched_cols, with = FALSE])) == 0L,
      pip_id
    ]
    if (length(failed) > 0L) {
      cli::cli_warn(
        c(
          "{length(failed)} pip_id{?s} returned NA for all enrichment fields.",
          "i" = "IDs: {.val {utils::head(failed, 5L)}}",
          "i" = paste0(
            "A missing {.col version_id_metadata} means no metadata was saved ",
            "for this survey. Check the pipeline output to investigate."
          )
        ),
        class = c("pip_inv_enrich_missing_meta", "pipwrn")
      )
    }
  }

  # Remove any pre-existing expanded columns for requested fields from prior
  # enrichment runs.  Use "^<field>_" (trailing underscore) to avoid accidentally
  # deleting sibling scalar columns that share the field name as a prefix
  # (e.g. "^pop_" leaves "pop_data_level" untouched).
  inv_names <- names(inv)
  for (field in fields) {
    existing <- grep(paste0("^", field, "_"), inv_names, value = TRUE)
    if (length(existing) > 0L) {
      inv[, (existing) := NULL]
    }
  }

  # Left join enrichment columns back onto inventory using the unique row index
  # (pip_id alone is not unique when the same survey appears at multiple
  # reporting levels).  Drop pip_id from the right-hand table before joining.
  field_dt_join <- field_dt[, !c("pip_id"), with = FALSE]
  inv <- joyn::left_join(
    inv,
    field_dt_join,
    by = ".row_id",
    relationship = "one-to-one",
    reportvar = FALSE,
    verbose = FALSE
  )
  inv[, .row_id := NULL]

  return(inv)
}
