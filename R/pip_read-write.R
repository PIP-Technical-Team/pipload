#' Read and write objects in the PIP ecosystem using `pins`
#'
#' @description A short description...
#'
#'   `pip_write()` and `pip_read()` are just wrappers of `pins_write()` and
#'   `pins_read()`, respectively, with predefined arguments.
#'
#' @inheritParams pins::pin_read
#' @param version An integer or a quoted directive. Retrieve a specific version
#'   of a pin. This argument is more powerful than the one in [pins::pin_read].
#'   See details.
#' @param pin_name character: pin name. It is the same as `name` argument in
#'   [pins::pin_read]. The reason it is is different is to make it clear in
#'   higher-level functions.
#' @param verbose logical:  display information
#'
#' @returns pip_read() returns an R object in the pip board; pip_write() returns
#'   the fully qualified name of the new pin, invisibly.
#'
#' @details The **version** argument supports several options
#'   * NULL: loads most recent version
#'   * "available": displays list of available versions for `name`.
#'   * "select", "pick", or "choose": allows user to select the pins version of `name`.
#'   * Negative integer (e.g., `-1`) or zero (0): loads that number of versions
#'   before the most recent version available. So, if `0`, it loads the current
#'   version, which is equivalent to NULL. If `-1`, it will load the version
#'   right before the current one, `-2` loads two versions before the current
#'   one, and so on.
#'   * Positive numbers: Error.
#'   * Any other character will be treated as a `pins` version, which is usual a
#'   string of the form "20250801T162739Z-d86e8", where the part to the left of
#'   the hyphen refers to the date and time, and the part to the right is a
#'   short hash of `name`.
#'
#' @export
#'
#' @examples
#' board <- pins::board_temp(versioned = TRUE)
#' name <- "x"
#'
#' # Writing pins
#' pip_write(board, x =1:5,  name = name)
#' Sys.sleep(3)
#' pip_write(board, x =1:10, name = name)
#' Sys.sleep(3)
#' pip_write(board, x =4:9, name = name)
#'
#' # Reading pins
#' pip_read(board, name)
#' (vers <- pip_read(board, name, version = "available"))
#' version <- vers[3, version]
#' pip_read(board, name, version = -1)
#' pip_read(board, name, version = -2)
#' pip_read(board, name, version = version)
#' \dontrun{
#' pip_read(board, name, version = "select")
#' }
pip_read <- function(board,
                     pin_name,
                     version = NULL,
                     hash = NULL,
                     verbose = TRUE,
                     ...) {
  # defenses
  stopifnot(exprs = {
    length(version) == 1 || is.null(version)
    })

  if (is.numeric(version) && version > 0) {
    cli::cli_abort("{.arg version} can't be a positive number")
  }


  # get available versions for pin
  vr <- get_pin_versions(board = board, pin_name = pin_name)

  # NULL or 0 → load version where vintage == 0
  if (is.null(version) || identical(version, 0)) {
    version <- vr[vintage == 0, ver]

     # return version metadata
  } else if (identical(version, "available")) {
    vr <- vr[, .(vintage, version = ver, created)]
    return(vr[])

    # If select version manually - (interactive menu)
  } else if (version %in% c("select", "pick", "choose")) {
    vr_dates <- vr[, created]
    selection <- menu(choices = vr_dates,
                      title = "select the version to load")
    version <- vr[selection, vintage]

    # resolve version - for all other case
  } else {
    version <- filter_version(vr = vr, version = version)
  }

  # read pin
  if (verbose)  cli::cli_alert_info("Reading pin {.val {pin_name}} with version {.val {version}}")
  pins::pin_read(
    board   = board,
    name    = pin_name,
    version = version,
    hash    = hash,
    ...
  )

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
    force_identical_write = FALSE,
    ...
) {
  # determine path
  if (!fs::dir_exists(dir)) fs::dir_create(dir)

  file <- fs::path(dir, id)

  # enforce file extension or let stamp choose
  sp <- stamp::st_path(file, format = format)

  # choose stamp versioning policy
  versioning_mode <- if (isTRUE(force_identical_write)) {
    "timestamp"   # always write a new version
  } else {
    "content"     # write only if content changed
  }

  # temporarily override stamp options
  out <- withr::with_options(
    list(stamp.versioning = versioning_mode),
    stamp::st_save(
      x       = x,
      file    = sp,
      metadata = metadata,
      code     = code,
      ...
    )
  )

  invisible(out)
}


#' get pins_versions slightly modified
#'
#' @inheritParams pip_read
#'
#'
#' @returns pins_version modified invisible
#' @keywords internal
get_pin_versions <- function(board, pin_name) {
  ver <- pins::pin_versions(board = board,
                            name = pin_name) |>
    setDT()

  setorder(ver, -created)

  ver[, vintage := (.I-1)*(-1)]
  setnames(ver, "version", "ver")

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
