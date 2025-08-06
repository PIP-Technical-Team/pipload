#' Load data from dlw repository
#'
#' @param country_code Character: country ISO 3 code.
#' @param year numeric: four digit year
#' @param module character: module of GMD collection (e.g., ALL, GPWG, L).
#'   Default is GPWG
#' @param survey character: survey acronyn
#' @param filename character: File name
#' @param vermast  character: Version of the master data in the form "vXX" where
#'   X is a number of two digits like "01" or "02".
#' @param veralt character: Version of the alternative  data in the form "vXX"
#'   where X is a number of two digits like "01" or "02".
#' @param collection character: It should always be "GMD"
#' @param latest_version logical: If TRUE and  `vermast` and `veralt` are NULL,
#'   it will use the most recent version of the data for a particular year.
#' @param  latest_year logical: If `TRUE` and  `year` is NULL, it retrieves the
#'   most recent year. Otherwise, it will return the calls for all the years
#'   available. This is the default.
#' @param verbose logical. If TRUE display information. Default is option
#'   "pipload.verbose"
#' @inheritParams pip_read
#'
#' @returns data table with dlw data
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
#' load_dlw_data(pin_name = "HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs")
#'
#' # without ext also works
#' load_dlw_data(pin_name = "HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG")
#'
#' # country and year
#' load_dlw_data(country_code = "HRV", year = 2011)
#'
#' # Find data
#' find_dlw_data(country_code = "HRV")
#'
#' # Latest year in EACH module
#' find_dlw_data(country_code = "HRV", latest_year = TRUE)
#' }
load_dlw_data <- function(country_code   = NULL,
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

  # defenses   ---------
  stopifnot(exprs = {
    !is.null(country_code) || !is.null(pin_name)
  })

  # Get board
  br <- pipfun::get_pins_boards(board = "dlw_data")

  # When pin name is defined   ------
  if (!is.null(pin_name)) {
    pin_name <- check_data_pin_name(pin_name)

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
  if (verbose) {
    cli::cli_alert_info("Loading {.field {pin_name}}")
  }
  return(pip_read(board = br,
                  pin_name = pin_name,
                  version = version,
                  hash    = hash))

}


#' Find data available in DLW data board `
#' @param board board from `pipfun::get_pins_boards(board = "dlw_data")`
#' @inheritParams load_dlw_data
#' @param ... Just the following: country_code, year, module, survey, vermast,
#'   veralt, collection, module. They work exactly the same as the ones in
#'   [load_dlw_data]
#'
#' @returns data from with filter data
#' @export
#'
#' @rdname load_dlw_data
find_dlw_data <- function(board = NULL,
                          latest_version = TRUE,
                          latest_year    = FALSE,
                          verbose        =  getOption("pipload.verbose"),
                          ...) {


  if (is.null(board)) {
    board <- pipfun::get_pins_boards(board = "dlw_data")
  }

  find_data(board = board,
            latest_version = latest_version,
            latest_year    = latest_year,
            verbose        =  verbose,
            ...)
}


