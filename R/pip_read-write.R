#' @title Read a versioned artifact (wrapper around {stamp})
#'
#' @description
#' Load a versioned artifact previously saved with `pip_write()`.
#' This function delegates to the {stamp} package for version discovery and
#' loading, while providing a small convenient API for the package.
#'
#' @param id Character. Artifact name or path (e.g., "myfile" or "data/myfile.qs2").
#'   Can include directory structure; format extension is optional.
#' @param format Character. File format to read, e.g. `"qs2"` (default) or `"fst"`.
#'   Must match the extension of the stored artifact.
#' @param version An integer or a quoted directive. Retrieve a specific version
#'   of an artifact. See details in the function body for supported directives.
#' @param alias Optional character. Passed to {stamp} functions (`st_versions`,
#'   `st_load`) to specify the stamp root; forwarded as-is. `NULL` (default)
#'   means use the default stamp root.
#' @param verbose Logical. Whether to print status messages (informational only).
#'
#' @return The loaded R object (the artifact content), or a `data.table` of
#'   available versions when `version = "available"`.
#'
#' @details
#' The `version` argument supports the following behaviours:
#' - `NULL` (default): loads the most recent version available.
#' - negative integers (e.g., `-1`): load older versions relative to latest.
#' - a specific version ID (character): load that version directly.
#' - `"select"` / `"pick"` / `"choose"`: interactive selection (interactive
#'   sessions only).
#' - `"available"`: return a `data.table` with version metadata and a
#'   computed `vintage` column describing relative age.
#'
#' This function intentionally does not call `stamp::st_init()`; callers
#' should initialize the stamp root (optionally with an `alias`) before
#' calling `pip_read()` so that storage is deterministic and testable.
#'
#' @examples
#' if (interactive()) {
#'   # Initialize a local root and use pip_write/pip_read normally
#'   tmp <- fs::path_temp("my_project")
#'   fs::dir_create(tmp)
#'   stamp::st_init(tmp, alias = "my_project")
#'   pip_write(1:3, id = "example", alias = "my_project")
#'   pip_read("example", alias = "my_project")
#' }
#'
#' @export
#' @importFrom stamp st_versions st_load st_save
pip_read <- function(
  id,
  format = "qs2",
  version = NULL,
  alias = NULL,
  verbose = getOption("pipload.verbose")
) {
  # NOTE: do not call stamp::st_init() here; caller should initialize stamp if needed via alias

  # Use id as file path (can include directory structure)
  file <- id

  # Handle format parameter: if a format is explicitly requested and the
  # id contains an extension, ensure they match; otherwise set the
  # extension on the id so stamp will use the requested format.
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
  stamp::st_load(file, version = version, alias = alias, verbose = verbose)
}


#' @title Save an R object as a versioned artifact (wrapper around {stamp})
#'
#' @description
#' Save an R object to a versioned artifact using the {stamp} package.
#' This wrapper provides a minimal, package-consistent API while forwarding
#' the heavy lifting (file layout, metadata, version ids) to {stamp}.
#'
#' @param x The R object to save.
#' @param id Character. The artifact identifier or path (e.g., "myfile" or "data/myfile.qs2").
#'   Can include directory structure; format extension is optional.
#' @param format Character. Format for serialization (`"qs2"`, `"rds"`, `"csv"`, `"fst"`, `"json"`).
#'   If `NULL`, the format is inferred from the file extension or {stamp}'s defaults.
#' @param metadata Named list of additional metadata to store with the artifact.
#' @param code Optional function, expression, or character. Its hash is stored with the artifact.
#'   If `FALSE` (default), a new version is written only when the content or code has changed.
#' @param alias Optional character. Passed to `stamp::st_save()` to specify the stamp root
#'   when saving; forwarded as-is. `NULL` (default) means use the default stamp root.
#' @param pk Optional primary key (passed to `stamp::st_save()` when storing data frames).
#' @param verbose Logical. Whether to print status messages.
#' @param ... Additional arguments forwarded to [stamp::st_save()].
#'
#' @return Invisibly, the list returned by [stamp::st_save()] including `path`, `metadata`,
#' and `version_id`.
#'
#' @details
#' `pip_write()` intentionally does not initialize stamp roots. Callers should
#' call `stamp::st_init(dir, alias = ...)` prior to saving if they wish to use
#' a specific alias root. This keeps initialization explicit and avoids
#' surprising side effects during package function calls or tests.
#'
#' @examples
#' if (interactive()) {
#'   tmp <- fs::path_temp("my_project_write")
#'   fs::dir_create(tmp)
#'   stamp::st_init(tmp, alias = "my_project_write")
#'   pip_write(mtcars[1:3, ], id = "data/cars.qs2", alias = "my_project_write")
#' }
#'
#' @export
#' @importFrom stamp st_save
pip_write <- function(
  x,
  id,
  format = "qs2",
  metadata = list(),
  code = NULL,
  alias = NULL,
  pk = NULL,
  verbose = getOption("pipload.verbose"),
  ...
) {
  # Set extension if not present
  if (is.null(fs::path_ext(id)) || identical(fs::path_ext(id), "")) {
    id <- fs::path_ext_set(path = id, ext = format)
  }

  # Use id as file path (can include directory structure)
  file <- id

  # NOTE: do not call stamp::st_init() here; caller should initialize stamp via alias if needed

  # Delegate to stamp::st_save which handles file creation, metadata and
  # version_id generation. We forward `alias` so stamp can place the file
  # in the correct alias root.
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
