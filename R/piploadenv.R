#' .piploadenv environment
#' @name piploadenv
NULL
#> NULL


#' Get function: Returns the entire .piploadenv environment
#'
#' @return The .piploadenv environment
#' @rdname piploadenv
#' @family piploadenv utilities
#' @export
#'
#' @examples
#' env <- get_piploadenv()
get_piploadenv <- function() {
  .piploadenv
}

#' Get for a specific key from .piploadenv
#'
#' @return The value associated with the key in .piploadenv
#' @rdname piploadenv
#' @family piploadenv utilities
#' @export
#'
#' @examples
#' set_in_piploadenv("example_key", 42)
#' get_from_piploadenv("example_key") # returns 42
get_from_piploadenv <- function(key, verbose =  FALSE) {
  x <- rlang::env_get(.piploadenv, key, default = NULL) |>  # Returns NULL if key doesn't exist
    copy() # make sure it does not get modified in the piploadenv
  if (verbose && !is.null(x)) {
    cli::cli_alert_info("Returning {key} from .piploadenv")
  }
  x
}

#' Setter function: Assign a value in .piploadenv
#'
#' @param key A character string representing the key
#' @param value The value to store in .piploadenv
#' @param verbose logical: if TRUE display information.
#'
#' @rdname piploadenv
#' @family piploadenv utilities
#' @return The assigned value (invisibly)
#' @export
#'
#' @examples
#' set_in_piploadenv("example_key", 42)
set_in_piploadenv <- function(key,
                          value,
                          verbose =  FALSE) {
  rlang::env_poke(.piploadenv, key, value)
  if (verbose) {
    cli::cli_alert_info("saving {key} in .piploadenv")
  }
  invisible(value)  # Return value invisibly to avoid clutter in console
}
