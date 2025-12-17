#' Read a versioned artifact saved with {stamp}
#'
#' @description
#' `pip_read()` loads an artifact written by `pip_write()`.
#' It is a thin wrapper around `stamp::st_load()` and preserves
#' all version semantics (negative versions, "select", etc.)
#'
#' @param id Character. Artifact name (file name stem).
#' @param dir Directory where artifact is stored.
#' @param version An integer or a quoted directive. Retrieve a specific version
#' of an artifact. See details.
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
    dir = ".",
    format = NULL,
    version = NULL,
    verbose = TRUE
) {

  # Defenses
  if (!fs::dir_exists(dir)) {
    cli::cli_abort("Artifact folder {.path {dir}} does not exist.")
  }

  # Initiate stamp
  stamp::st_init(dir)

  # Construct artifact path
  file <- fs::path(dir, id)

  if(is.null(format)){
    format  <- fs::path_ext(file)
  }

  # Check if file exists when format is not attached
  if(is.na(format) || identical(format, "")){

    ext        <- tolower(stamp::st_formats())
    files_tbl  <- fs::dir_info(dir, recurse = FALSE)
    paths_ext  <- tolower(fs::path_ext(files_tbl$path))
    all_paths  <- files_tbl$path[files_tbl$type == "file" & paths_ext %in% ext]
    paths_base <- fs::path_file(fs::path_ext_remove(all_paths))
    id_base <- fs::path_file(fs::path_ext_remove(id)) 
    matched_paths <- all_paths[paths_base == id_base]

    if (length(matched_paths) == 0L) {
      cli::cli_abort(c(
        x = "Artifact {.field {id}} not found in {.path {dir}}.",
        i = "If the artifact is stored under a different name or format, pass the correct {.arg id} or use {.code format = NULL} to inspect available files."
      ))
    }

    file_ext <- fs::path_ext(matched_paths)|> tolower()|> collapse::funique()

    if(length(file_ext)==1){
      file <- fs::path_ext_set(path = file, ext = file_ext)
    }else{
      cli::cli_abort(c(x = "Multiple formats found for artifact {.field {id}}: {.val {file_ext}}.", 
      i = "Specify which format to load using the {.arg format} argument."))
    }
  }else{
    # Change format to the one requested
    file <- fs::path_ext_set(path = file, ext = format)
  }

   # Make sure file exists (use absolute path to be safe)
  file_abs <- fs::path_abs(file)
  if (!fs::file_exists(file_abs)) {
    cli::cli_abort(c(
      x = "File {.path {file_abs}} does not exist.",
      i = "Try {.code format = NULL} to list available formats or verify the {.arg id}/{.arg dir} combination."
    ))
  }
  # List available versions
  if (identical(version, "available")) {
    vr <- stamp::st_versions(file)
    if (nrow(vr) == 0)
      cli::cli_abort("No versions found in {.path {file}}.")
    vr[, vintage := (.I - 1) * -1]
    return(vr[])
  }

  # Protect against empty artifact
  vr <- stamp::st_versions(file)

  if (nrow(vr) == 0)
    cli::cli_abort("No version files found in {.path {file}}.")

  if (verbose) {
    ver_label <- if (is.null(version)) "latest" else as.character(version)
    cli::cli_alert_info("Loading {.path {file}} (version = {.strong {ver_label}})")
  }

  # Delegate loading to stamp
  stamp::st_load(file, version = version)
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
#' @param id Character. The artifact identifier (used as the file name).
#' @param dir Character. Directory where the artifact will be saved. Defaults to `"."`.
#' @param format Character, optional. Format for serialization (`"qs2"`, `"rds"`, `"csv"`, `"fst"`, `"json"`).
#'   If `NULL`, the format is inferred from the file extension or `stamp` defaults.
#' @param metadata Named list of additional metadata to store with the artifact.
#' @param code Optional function, expression, or character. Its hash is stored with the artifact.
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
    format = "NULL",
    metadata = list(),
    code = NULL,
    ...
) {
  # ensure directory exists
  # if (!fs::dir_exists(dir))
  # {
  #   cli::cli_abort("Provided directory path does not exist")
  # }

  fs::dir_create(dir, recurse = TRUE)

  # determine file path
  file <- fs::path(dir, id, ext = format)

  # Initiate stamp
  stamp::st_init(dir)

  # declare st_path
  sp <- stamp::st_path(file, format = format)

  # save with stamp
  out <- stamp::st_save(
    x        = x,
    file     = sp,
    metadata = metadata,
    code     = code,
    format   = format,
    ...
  )

  invisible(out)
}
