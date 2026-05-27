#' Enrich a PIP inventory with fields from per-survey metadata
#'
#' For each `pip_id` in `inv`, loads the corresponding metadata artifact from
#' stamp (the `"pip_meta"` alias) and extracts the requested fields as new
#' columns in the inventory. The exact metadata version is resolved from
#' `inv$version_id_metadata` when present; otherwise the latest version is
#' used.
#'
#' This function is called automatically by [load_pip_master_inventory()] when
#' its `fields` argument is non-empty. By default no enrichment is performed.
#' Usage example:
#' ```r
#' inv <- load_pip_master_inventory(fields = "reporting_level")
#' # Or enrich after loading:
#' raw_inv <- load_pip_master_inventory()
#' enriched <- pip_inv_enrich(raw_inv, fields = "reporting_level")
#' ```
#'
#' @param inv A `data.table` containing the PIP master inventory. Must have a
#'   `pip_id` column. If a `version_id_metadata` column is present, it is used
#'   to load the exact metadata version for each survey.
#' @param fields Character vector naming list elements to extract from each
#'   survey's metadata object (e.g. `"reporting_level"`). Each named element
#'   is added as a column in the returned inventory. Surveys whose metadata
#'   cannot be loaded receive `NA` for all requested fields.
#'
#' @return The input `inv` with the requested columns added or replaced.
#'   Pre-existing columns matching any requested field name (including joyn
#'   suffix variants like `field.x` / `field.y`) are removed before the new
#'   values are joined in.
#'
#' @details
#' Metadata is loaded via [pip_read()] with `alias = "pip_meta"`. Loading
#' errors (e.g. stale version, missing artifact) are caught silently — the
#' affected rows receive `NA` for all requested fields and a warning is emitted
#' listing the affected `pip_id`s.
#'
#' @seealso [load_pip_master_inventory()]
#' @family inventory
#' @export
#'
#' @examples
#' \dontrun{
#' inv <- load_pip_master_inventory(fields = "reporting_level")
#' # Or enrich multiple fields:
#' inv2 <- pip_inv_enrich(inv, fields = c("reporting_level", "welfare_type"))
#' }
pip_inv_enrich <- function(inv, fields = character(0)) {
  if (length(fields) == 0L) {
    return(inv)
  }

  has_version_col <- "version_id_metadata" %in% names(inv)

  # For each pip_id, load its metadata and extract the requested fields.
  # Returns a one-row data.table with pip_id + one column per field.
  extract_one <- function(i) {
    pip_id <- inv$pip_id[[i]]
    version <- if (has_version_col && !is.na(inv$version_id_metadata[[i]])) {
      inv$version_id_metadata[[i]]
    } else {
      NULL
    }

    meta <- tryCatch(
      pip_read(id = pip_id, alias = "pip_meta", version = version),
      error = function(e) NULL
    )

    row <- data.table::data.table(pip_id = pip_id)
    for (field in fields) {
      row[[field]] <- if (!is.null(meta) && !is.null(meta[[field]])) {
        meta[[field]][[1L]] # scalar: take first element in case it is a vector
      } else {
        NA
      }
    }
    row
  }

  field_rows <- lapply(seq_len(nrow(inv)), extract_one)
  field_dt <- data.table::rbindlist(field_rows, fill = TRUE)

  # Warn about pip_ids whose metadata could not provide all requested fields
  failed <- field_dt[
    rowSums(is.na(field_dt[, fields, with = FALSE])) == length(fields),
    pip_id
  ]
  if (length(failed) > 0L) {
    cli::cli_warn(
      c(
        "{length(failed)} pip_id(s) returned NA for all enrichment fields.",
        "i" = "IDs: {.val {utils::head(failed, 5L)}}",
        "i" = "Metadata may be missing or the version may be stale."
      ),
      class = c("pip_inv_enrich_missing_meta", "pipwrn")
    )
  }

  # Remove any pre-existing columns for the requested fields (incl .x/.y artifacts)
  for (field in fields) {
    existing <- grep(paste0("^", field), names(inv), value = TRUE)
    if (length(existing) > 0L) {
      inv[, (existing) := NULL]
    }
  }

  # Left join enrichment columns back onto inventory
  inv <- joyn::left_join(
    inv,
    field_dt,
    by = "pip_id",
    relationship = "many-to-one",
    reportvar = FALSE,
    verbose = FALSE
  )

  inv
}
