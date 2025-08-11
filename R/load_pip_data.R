#' Load data from pip repository
#'
#' @param country_code Character: country ISO 3 code.
#' @param year numeric: four digit year
#' @param module character: module of GMD collection (e.g., ALL, GPWG, L).
#'   Default is GPWG
#' @param survey character: survey acronym
#' @param pin_name character: File name
#' @param pin_version character: version given by pins
#' @param vermast  character: Version of the master data in the form "vXX" where
#'   X is a number of two digits like "01" or "02".
#' @param veralt character: Version of the alternative  data in the form "vXX"
#'   where X is a number of two digits like "01" or "02".
#' @param collection character: It should always be "GMD"
#' @param latest_version logical: If TRUE and  `vermast` and `veralt` are NULL,
#'   it will use the most recent version of the data given pins.
#' @param latest_year logical: If `TRUE` and  `year` is NULL, it retrieves the
#'   most recent year. Otherwise, it will return the calls for all the years
#'   available. This is the default.
#' @param verbose logical. If TRUE display information. Default is option
#'   "pipload.verbose"
#' @inheritParams pip_read
#'
#' @returns data table with pip data
#' @export
#'
#' @examples
#' \dontrun{
#' lr <- pipfun::get_latest_pip_release()
#' pipfun::setup_working_release(release = lr$release,
#'                               identity = lr$identity,
#'                               verbose = FALSE)
#'
#' # Using pin_name
#' load_pip_data(pin_name = "LCA_2015_SLCHBS_D1_INC_GPWG")
#'
#' # country and year
#' load_pip_data(country_code = "HRV", year = 2011)
#' }
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
    pin_name <- check_pin_name(pin_name, "pip_data")

  } else {
    # filter   ---------

    inv <- find_data(board          = br,
                        latest_version = latest_version,
                        latest_year    = latest_year,
                        verbose        = verbose,
                        country_code   = country_code,
                        surveyid_year  = year,
                        survey_acronym = survey,
                        vermast        = vermast,
                        veralt         = veralt,
                        collection     = collection,
                        module         = module)

    pin_name <- inv[, pip_id]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (verbose) {
    cli::cli_alert_info("Loading {.field {pin_name}}")
  }
  return(pip_read(board = br,
                  pin_name = pin_name,
                  version = version,
                  hash    = hash))

}

#' check that pin_name is correct
#'
#' @param pin_name pin name data
#' @param board_name either dlw_data or pip_data (for now)
#'
#' @returns character with pin_name
#' @keywords internal
#'
#' @examples
#' check_pin_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs", "dlw")
#' check_pin_name("LCA_2015_SLCHBS_D1_INC_GPWG.qs","pip")
check_pin_name <- \(pin_name,
                    board_name = c("dlw_data","pip_data")) {

  # Defenses
  stopifnot(board_name %in% c("dlw_data","pip_data"))

  pin_name <- pin_name |>
    fs::path_ext_remove() |>
    fs::path(ext = "qs")

  if(board_name == "dlw_data"){
    ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_M_[Vv][0-9]{2}_A_[^_]+_[^_]+\\.[A-Za-z]+$"

    if (!grepl(ptt, pin_name)) {
      cli::cli_abort(c(x = "Wrong {.arg pin_name} specification",
                       i = "it should follow the pattern {.field {ptt}}",
                       i = "like in {.file HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs}"))
    }

  }else if(board_name == "pip_data"){

    ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_(D1|D2)_(INC|CON)+_[^_]+\\.[A-Za-z]+$"

    if (!grepl(ptt, pin_name)) {
      cli::cli_abort(c(x = "Wrong {.arg pin_name} specification",
                       i = "it should follow the pattern {.field {ptt}}",
                       i = "like in {.file LCA_2015_SLCHBS_D1_INC_GPWG.qs}"))
    }
  }

  invisible(pin_name)
}


#' Find data available in pin board `
#' @param board pin board
#' @inheritParams load_pip_data
#' @param ... Just the following: country_code, year, module, survey, vermast,
#'   veralt, collection, module. They work exactly the same as the ones in
#'   [load_pip_data]
#'
#' @returns data from with filter data
#' @export
#' @examples
#' \dontrun{
#' lr <- pipfun::get_latest_pip_release()
#' pipfun::setup_working_release(release = lr$release,
#'                               identity = lr$identity,
#'                               verbose = FALSE)
#'
#' board_pip <- pipfun::get_pins_boards(board = "pip_data")
#'
#' # Find data
#' find_data(board = board_pip, country_code = "HRV")
#'
#' # Latest year in EACH module
#' find_data(board = board_pip, country_code = "HRV", latest_year = TRUE)
#' }
find_data <- function(board,
                      latest_version = TRUE,
                      latest_year    = FALSE,
                      verbose        =  getOption("pipload.verbose"),
                      ...) {
  # Defenses


  # Capture ... arguments as a list
  dots <- list(...)
  # Combine country and ... into a single list of arguments
  args <- lapply(dots, \(.) {
    if (is.character(.)) {
      toupper(.)
    } else {
      .
    }
  })

  args_info <- Filter(Negate(is.null), args) |>
    names()

  # Load inventory
  if(grepl("dlw_data", board$path)){

    # board_inv <- pipfun::get_pins_boards(board = "dlw_inventory")
    # inv       <- pins::pin_read(board = board_inv ,name = "dlw_inventory")
    cli::cli_abort("Cannot be done until dlw_inventory exist in board dlw_inventory")

  }else if(grepl("pip_data", board$path)){

    board_inv <- pipfun::get_pins_boards(board = "pip_inventory")
    inv       <- pins::pin_read(board = board_inv ,name = "pip_inventory")
  }

  if(!is.data.table(inv)){
    inv <- as.data.table(inv)
  }

  for (nm in args_info) {
    inv <- inv[get(nm) %in% args[[nm]]]
  }

  inv <- unique(inv)

  if (latest_year == TRUE && !("surveyid_year" %in% args_info)) {
    inv <- inv[,
               #  for each collection and module, the row(s) with the maximum Year
               .SD[surveyid_year == max(surveyid_year, na.rm = TRUE)],
               by = .(collection, module)
    ]
  }

  if (!("vermast" %in% args_info) &&
      !("veralt" %in% args_info) &&
      latest_version == TRUE) {
    inv <- inv[ ,
                #  for each year, the row(s) with the maximum Vermast.
                .SD[vermast == max(vermast, na.rm = TRUE)],
                by = .(surveyid_year, collection, module)
    ][,
      #It should return only one row per year (even if there are ties)
      .SD[veralt == max(veralt, na.rm = TRUE)],
      by = .(surveyid_year, collection, module)]
  }

  return(inv)
}

