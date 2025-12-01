#' Read a versioned artifact saved with {stamp}
#'
#' @description
#' `pip_read()` loads an artifact written by `pip_write()`.
#' It is a thin wrapper around `stamp::st_load()` and preserves
#' all version semantics (negative versions, "select", etc.)
#'
#' @param id Character. Artifact name (file name stem).
#' @param dir Directory where artifact is stored.
#' @param version Version selector:
#'   * NULL or 0 → latest version
#'   * negative integer → N versions back
#'   * "select" / "pick" / "choose" → interactive menu
#'   * "available" → list available versions (extension provided below)
#'   * version-id character → load specific version
#' @param verbose Whether to print status messages.
#'
#' @return The loaded R object.
#'
#' @export
pip_read <- function(
    id,
    dir = ".",
    version = NULL,
    verbose = TRUE
) {
  file <- fs::path(dir,
                   id)

  if (identical(version, "available")) {
    vr <- stamp::st_versions(file)
    vr[, vintage := (.I - 1) * -1]
    return(vr[])
  }

  if (verbose)
    cli::cli_alert_info("Loading {.file {file}} (version = {.val {version}})")

  # Delegate everything else to stamp
  stamp::st_load(file,
                 version = version)
}


#' Save an R object to a versioned artifact using {stamp}
#'
#' @description
#' `pip_write()` saves an R object to disk using {stamp} versioned artifacts.
#' It is a thin wrapper around [stamp::st_save()] that provides:
#' * a simplified interface for storing objects in a directory
#' * optional "force write even if identical" behavior
#' * automatic creation of the directory if it doesn't exist
#'
#' @param x The R object to save.
#' @param id Character. The artifact identifier (used as the file name stem).
#' @param dir Character. Directory where the artifact will be saved. Defaults to `"."`.
#' @param format Character, optional. Format for serialization (`"qs2"`, `"rds"`, `"csv"`, `"fst"`, `"json"`).
#'   If `NULL`, the format is inferred from the file extension or `stamp` defaults.
#' @param metadata Named list of additional metadata to store with the artifact.
#' @param code Optional function, expression, or character. Its hash is stored with the artifact.
#' @param force_identical_write Logical. If `TRUE`, always write a new version even if content is unchanged.
#'   If `FALSE` (default), a new version is written only when the content or code has changed.
#' @param ... Additional arguments forwarded to [stamp::st_save()].
#'
#' @returns Invisibly, a list returned by [stamp::st_save()] containing:
#'   - `path`: full path to the artifact
#'   - `metadata`: merged metadata including content hashes, file size, etc.
#'   - `version_id`: internal version identifier created by {stamp}
#'
#' @details
#'
#' Versioning policy is controlled via `force_identical_write`:
#' - `FALSE` → uses `st_opts("versioning") = "content"` (default stamp behavior)
#'   A new version is only written if the content or code has changed.
#' - `TRUE` → temporarily sets `st_opts("versioning") = "timestamp"`
#'   Always creates a new version even if the object is unchanged.
#'
#' @examples
#' # Save a simple vector
#' pip_write(1:5, id = "example_vector", dir = tempdir())
#'
#' # Force a new version even if identical
#' pip_write(1:5, id = "example_vector", dir = tempdir(), force_identical_write = TRUE)
#'
#' @export
pip_write <- function(
    x,
    id,
    dir = ".",
    format = NULL,
    metadata = list(),
    code = NULL,
    ...
) {
  # ensure directory exists
  if (!fs::dir_exists(dir)) fs::dir_create(dir)

  # determine file path
  file <- fs::path(dir, id)

  # declare st_path
  sp <- stamp::st_path(file, format = format)

  # save with stamp
  out <- stamp::st_save(
    x        = x,
    file     = sp,
    metadata = metadata,
    code     = code,
    ...
  )

  invisible(out)
}


#' Filter version according to user selection
#'
#' @param vr data,table from [get_pin_versions]
#' @param version provided by the user
#'
#' @returns character vector of length 1 with pins version
#' @keywords internal
filter_version <- function(vr, version) {
  if (is.character(version)) {
    VER <- vr[ver == version]

  } else if (is.numeric(version) && version < 0) {
    VER <- vr[vintage == version]

  } else {
    cli::cli_abort("{.arg version} does not meet the correct specifications")
  }

  if (nrow(VER) == 0) {
    cli::cli_abort("{.arg version} selected ({.field version}) is not available in {name}")
  }

  if (nrow(VER) > 1) {
    cli::cli_abort("{.arg version} selected ({.field {version}}) returns more than one version.
                     inspect with `pins::pin_versions()`")
  }

  VER[, ver]
}
