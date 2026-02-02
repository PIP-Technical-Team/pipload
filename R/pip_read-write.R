#' Read a versioned artifact saved with {stamp}
#'
#' @description
#' `pip_read()` loads an artifact written by `pip_write()`.
#' It is a thin wrapper around `stamp::st_load()` and preserves
#' all version semantics (negative versions, "select", etc.)
#'
#' @param id Character. Artifact name or path (e.g., "myfile" or "data/myfile.qs2").
#'   Can include directory structure; format extension is optional.
#' @param version An integer or a quoted directive. Retrieve a specific version
#' of an artifact. See details in `pip_read`.
#' @param alias Optional character. Passed to `stamp` functions (`st_versions`,
#'   `st_load`) to specify the stamp root; forwarded as-is. `NULL` (default) means no alias.
#' @param verbose Whether to print status messages.
#'
#' @return The loaded R object.
#'
#' @details
#' The `version` argument allows you to load specific versions:
#'   * `NULL` (default): loads the most recent version available.
#'   * Negative integer (e.g., `-1`) or zero (`0`): loads that number of versions
#'     before the most recent version. So, if `0`, it loads the current
#'     version, which is equivalent to `NULL`. If `-1`, it will load the version
#'     right before the current one, `-2` loads two versions before, and so on.
#'   * Positive numbers: Error.
#'   * Character: treated as a specific version ID (e.g., "20250801T162739Z-d86e8").
#'   * `"select"`, `"pick"`, or `"choose"`: displays an interactive menu to select from
#'     available versions (only in interactive R sessions).
#'   * `"available"`: loads a list of the versions availables product from `stamp::st_versions`
#'     but with an added variable called `vintage` that indicates the order of the versions.
#'
#' @export
pip_read <- function(
  id,
  format = "qs2",
  version = NULL,
  alias = NULL,
  verbose = TRUE
) {
  # NOTE: do not call stamp::st_init() here; caller should initialize stamp if needed via alias

  # Use id as file path (can include directory structure)
  file <- id

  # Handle format parameter
  file_ext <- fs::path_ext(file)
  has_ext <- !is.na(file_ext) && !identical(file_ext, "")

  if (!is.null(format)) {
    if (has_ext) {
      # If file has extension and format is specified, they must match
      if (!identical(tolower(file_ext), tolower(format))) {
        cli::cli_abort(c(
          x = "Mismatch between file extension {.val {file_ext}} and requested format {.val {format}}.",
          i = "Either remove the extension from {.arg id} or set {.code format = NULL}."
        ))
      }
      # Extension already present and matches, use as-is
    } else {
      # No extension, add the format
      file <- fs::path_ext_set(path = file, ext = format)
    }
  }

  # List available versions
  if (identical(version, "available")) {
    vr <- stamp::st_versions(file, alias = alias)
    if (nrow(vr) == 0) {
      cli::cli_abort("No versions found in {.path {file}}.")
    }
    vr[, vintage := (.I - 1) * -1]
    return(vr[])
  }

  # Protect against empty artifact
  vr <- stamp::st_versions(file, alias = alias)

  if (nrow(vr) == 0) {
    cli::cli_abort("No version files found in {.path {file}}.")
  }

  if (verbose) {
    ver_label <- if (is.null(version)) "latest" else as.character(version)
    cli::cli_alert_info(
      "Loading {.path {file}} (version = {.strong {ver_label}})"
    )
  }

  # Delegate loading to stamp
  stamp::st_load(file, version = version, alias = alias)
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
#' @param id Character. The artifact identifier or path (e.g., "myfile" or "data/myfile.qs2").
#'   Can include directory structure; format extension is optional.
#' @param format Character, optional. Format for serialization (`"qs2"`, `"rds"`, `"csv"`, `"fst"`, `"json"`).
#'   If `NULL`, the format is inferred from the file extension or `stamp` defaults.
#' @param metadata Named list of additional metadata to store with the artifact.
#' @param code Optional function, expression, or character. Its hash is stored with the artifact.
#'   If `FALSE` (default), a new version is written only when the content or code has changed.
#' @param alias Optional character. Passed to `stamp::st_save()` to specify the stamp root
#'   when saving; forwarded as-is. `NULL` (default) means no alias.
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
#' pip_write(1:5, id = "example_vector.qs2", alias = "my_project")
#'
#' # Save with directory structure
#' pip_write(mtcars, id = "data/cars.qs2", alias = "my_project")
#'
#' @export
pip_write <- function(
  x,
  id,
  format = "qs2",
  metadata = list(),
  code = NULL,
  alias = NULL,
  pk = NULL,
  verbose = TRUE,
  ...
) {
  # Set extension if not present
  if (is.null(fs::path_ext(id)) || identical(fs::path_ext(id), "")) {
    id <- fs::path_ext_set(path = id, ext = format)
  }

  # Use id as file path (can include directory structure)
  file <- id

  # NOTE: do not call stamp::st_init() here; caller should initialize stamp via alias if needed

  # declare st_path
  # sp <- stamp::st_path(file, alias = alias)

  # save with stamp
  out <- stamp::st_save(
    x = x,
    file = file,
    metadata = metadata,
    code = code,
    format = format,
    alias = alias,
    pk = pk,
    verbose = verbose,
    ...
  )

  invisible(out)
}
