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
                     name,
                     version = NULL,
                     hash = NULL,
                     ...) {
  # defenses
  stopifnot(exprs = {
    length(version) == 1 || is.null(version)
    })

  if (is.numeric(version) && version > 0) {
    cli::cli_abort("{.arg version} can't be a positive number")
  }


  # Treat version
  if (!is.null(version) && version != 0) {
    vr <- get_pin_versions(board = board,
                            name = name)

    # If Available
    if (version == "available") {
      vr <- vr[, .(vintage, version = ver, created)]
      return(vr[])
    }

    # If select version manually
    if (version %in% c("select", "pick", "choose")) {
      vr_dates <- vr[, created]
      selection <- menu(choices = vr_dates,
                        title = "select the version to load")
      version <- vr[selection, vintage]
    }

    # Filter versions.
    version <- filter_version(vr = vr, version = version)

  }

  pins::pin_read(board   = board,
                 name    = name,
                 version = version,
                 hash    = hash,
                 ...)

}

#' @inheritParams pins::pin_write
#' @inheritDotParams pins::pin_write  title description metadata tags urls
#'
#' @export
#' @rdname pip_read
pip_write <- function(board,
                      x,
                      name = NULL,
                      force_identical_write = FALSE,
                      ...) {

  pins::pin_write(board                 = board,
                  x                     = x,
                  name                  = name,
                  force_identical_write = force_identical_write,
                  type                  = "qs",
                  versioned             = TRUE,
                  ...)


}

#' get pins_versions slightly modified
#'
#' @inheritParams pip_read
#'
#'
#' @returns pins_version modified invisible
#' @keywords internal
get_pin_versions <- function(board, name) {
  ver <- pins::pin_versions(board, name) |>
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
