load_pip_data <- function(country_code   = NULL,
                          year           = NULL,
                          survey         = NULL,
                          vermast        = NULL,
                          veralt         = NULL,
                          collection     = "GMD",
                          module         = "GPWG",
                          latest_version = TRUE,
                          latest_year    = FALSE,
                          pin_name       = NULL,
                          version        = NULL,
                          hash           = NULL,
                          verbose        =  getOption("pipload.verbose")) {

  # Defenses
  stopifnot(exprs = {
    !is.null(country_code) || !is.null(pin_name)
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get board
  br <- pipfun::get_pins_boards(board = "pip_data")

  # When pin name is defined   ------
  if (!is.null(pin_name)) {
    pin_name <- check_pin_name(pin_name, "pip")

  } else {
    # filter   ---------

    fd <- find_dlw_data(board          = br,
                        latest_version = latest_version,
                        latest_year    = latest_year,
                        verbose        = verbose,
                        country_code   = country_code,
                        year           = year,
                        survey         = survey,
                        vermast        = vermast,
                        veralt         = veralt,
                        collection     = collection,
                        module         = module)

    pin_name <- fd[, pin_name]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(TRUE)

}

#' check that pin_name is correct
#'
#' @param pin_name pin name data
#' @param format either dlw or pip (for now)
#'
#' @returns character with pin_name
#' @keywords internal
#'
#' @examples
#' check_pin_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs", "dlw")
#' check_pin_name("LCA_2015_SLCHBS_D1_INC_GPWG.qs","pip")
check_pin_name <- \(pin_name,
                    format = c("dlw","pip")) {

  # Defenses
  stopifnot(format %in% c("dlw","pip"))

  pin_name <- pin_name |>
    fs::path_ext_remove() |>
    fs::path(ext = "qs")

  if(format == "dlw"){
    ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_M_[Vv][0-9]{2}_A_[^_]+_[^_]+\\.[A-Za-z]+$"

    if (!grepl(ptt, pin_name)) {
      cli::cli_abort(c(x = "Wrong {.arg pin_name} specification",
                       i = "it should follow the pattern {.field {ptt}}",
                       i = "like in {.file HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs}"))
    }

  }else if(format == "pip"){

    ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_(D1|D2)_(INC|CON)+_[^_]+\\.[A-Za-z]+$"

    if (!grepl(ptt, pin_name)) {
      cli::cli_abort(c(x = "Wrong {.arg pin_name} specification",
                       i = "it should follow the pattern {.field {ptt}}",
                       i = "like in {.file LCA_2015_SLCHBS_D1_INC_GPWG.qs}"))
    }
  }

  invisible(pin_name)
}
