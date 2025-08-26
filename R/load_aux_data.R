#' Load any auxiliary data
#'
#' Load auxiliary data files available in aux_repository
#'
#' @param measure character: name of measure to load e.g., "cpi" or "ppp"
#' @inheritParams pip_read
#'
#' @param ppp_defaults logical: If TRUE, wider format ppp data will be returned
#'
#'
#'
#' @param apply_label logical: if TRUE, predefined labels will apply to data
#'   loaded using `file_to_load` argument. Default TRUE. Tip: change to FALSE if
#'   the main structure of data has changed and labels have not been updated
#' @param verbose logical: whether to display message. Default is TRUE
#'
#' @return data.table with aux data
#' @export
load_aux_data <- function(measure  = NULL,
                          version  = NULL,
                          #pin_name = measure,  #to check how to use it, depending on how it's saved
                          #apply_label = FALSE,
                          ppp_defaults = TRUE,
                          hash      = NULL,
                          verbose  = getOption("pipload.verbose")) {

  # Defenses
  stopifnot(exprs = {
    !is.null(measure) || !is.null(pin_name)
  })


  # Get release
  pipfun::get_wrk_release(verbose = FALSE)

  # Get board
  br <- pipfun::get_pins_boards(board = "aux_data")

  pin_name <- measure


  # Read pin
  dt <- pip_read(board    = br,
                 pin_name = pin_name,
                 version  = version,
                 hash     = hash
  )

  # Apply labels optionally
  # if (apply_label) {
  #   dt <- pip_add_aux_labels(df,
  #                            measure = measure,
  #                            verbose = verbose)
  #
  #   if (verbose) {cli::cli_alert_info("Labels applied to data")}
  #
  # }

  # Keep only ppp default years
  if (measure == "ppp" && ppp_defaults == TRUE) {

    dt <- dt[ppp_default_by_year == TRUE]

  }


  # Return
  return(dt)

}
