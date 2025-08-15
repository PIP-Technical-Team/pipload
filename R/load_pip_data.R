#' Set of functions to interact with PIP data.
#'
#' [load_pip_data] Load data from pip repository. [find_pip_data] Finds data
#' available in pin board. [load_pip_inventory_release] Loads pip inventory for
#' corresponding release. [load_pip_master_inventory] Loads pip master inventory
#' for corresponding release. [check_pip_pin_name] checks that pip pin_name is
#' correct (INTERNAL)
#'
#' @param country_code Character: country ISO 3 code.
#' @param surveyid_year numeric: four digit year
#' @param survey_acronym character: survey acronym
#' @param welfare_type character: either "INC" for income or "CON" for
#'   consumption. This is only useul when there are two welfare types in the
#'   same year for the same country
#' @param module character: module of GMD collection (e.g., ALL, GPWG, L).
#'   Default is GPWG
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
#' load_pip_data(pin_name = "LCA_2015_SLCHBS_INC_GPWG")
#'
#' }
load_pip_data <- function(country_code   = NULL,
                          surveyid_year  = NULL,
                          survey_acronym = NULL,
                          welfare_type   = NULL,
                          module         = "GPWG",
                          pin_name       = NULL,
                          vermast        = NULL,
                          veralt         = NULL,
                          collection     = "GMD",
                          latest_version = TRUE,
                          latest_year    = FALSE,
                          where          = c("release", "master"),
                          version        = NULL,
                          hash           = NULL,
                          verbose        =  getOption("pipload.verbose")) {

  # Defenses
  stopifnot(exprs = {
    !is.null(country_code) || !is.null(pin_name)
  })

  # computations   --------
  where <- match.arg(where)

  # Get board
  br <- pipfun::get_pins_boards(board = "pip_data")

  # When pin name is defined   ------
  if (!is.null(pin_name)) {
    pin_name <- check_pip_pin_name(pin_name)

  } else {
  # filter   ---------

    inv <- find_pip_data(board          = br,
                        latest_version = latest_version,
                        latest_year    = latest_year,
                        where          = where,
                        verbose        = verbose,
                        country_code   = country_code,
                        surveyid_year  = surveyid_year,
                        survey_acronym = survey_acronym,
                        welfare_type   = welfare_type,
                        module         = module,
                        vermast        = vermast,
                        veralt         = veralt,
                        collection     = collection,
                        )

    pin_name <- inv[, pip_id]
  }

  if (length(pin_name) != 1) {
    cli::cli_abort(c(x = "Wrong numer of data to load.",
                     i = "It should be only 1. You attempt to load
                     {.field {length(pin_name)}}:",
                     "{.field {pin_name}}"))
  }

  # Return   ---------
  if (verbose) {
    cli::cli_alert_info("Loading {.field {pin_name}}")
  }
  return(pip_read(board = br,
                  pin_name = pin_name,
                  version = version,
                  hash    = hash))

}


#' @param board pin board
#' @inheritParams load_pip_data
#' @param where character: Either "release" or "master". Se details.
#' @param ... Just the following: country_code, year, survey, welfare_type,and
#'   module. They work exactly the same as the ones in [load_pip_data]
#'
#' @returns filtered data table from inventory
#' @rdname load_pip_data
#' @export
#'
#' @details
#' **where** specifies *where* to find data. It could be at the release level or at the master level. Release refers to the surveys specified in the Price Framework data of each release. Master refers to all the pip cleaned data
#'
#'
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
#' find_pip_data(board = board_pip, country_code = "HRV")
#'
#' # Latest year in EACH module
#' find_pip_data(board = board_pip, country_code = "HRV", latest_year = TRUE)
#' }
find_pip_data <- function(board = pipfun::get_pins_boards(board = "pip_data"),
                          latest_year    = FALSE,
                          where          = c("release", "master"),
                          verbose        =  getOption("pipload.verbose"),
                          ...) {
  where <- match.arg(where)
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

  vars <- get_from_piploadenv("pip_id_vars") |>
    tolower()
  args <- setNames(args[vars], vars)

  # Build regex pattern
  pattern <- lapply(vars, \(.) {
    if (is.null(args[[.]])) {
      "[^_]+" # anything but _
    } else {
      paste0("(", paste(args[[.]],collapse = "|"), ")")
    }
  }) |>
    # append them together
    paste(collapse = "_")

  if (where == "master") {
    ctl <- load_pip_master_inventory() |>
      setDT()
  } else {
    ctl <- load_pip_inventory_release() |>
      setDT()
  }
  ctl <- ctl[grepl(pattern, pip_id)]

  if (latest_year == TRUE && !("surveyid_year" %in% args_info)) {
    ctl <- ctl[,
               #  for each collection and module,
               # the row(s) with the maximum Year
               .SD[surveyid_year == max(surveyid_year, na.rm = TRUE)],
               by = .(module)
    ]
  }


  ## This part is to refine filter and print it pretty.
  # this should also depend on argument `where`

  return(ctl)
}


#' @returns data.table with PIP inventory
#' @rdname load_pip_data
#' @export
#'
#' @examples
#' load_pip_inventory()
load_pip_inventory_release <- \() {
  binv <- pipfun::get_pins_boards(board = "pip_inventory")
  pip_read(binv, "pip_inventory")
}


#' @returns data.table with PIP inventory
#' @rdname load_pip_data
#' @export
#'
#' @examples
#' load_pip_master_inventory()
load_pip_master_inventory <- \() {
  binv <- pipfun::get_pins_boards(board = "pip_master_inventory")
  pip_read(binv, "pip_master_inventory")
}



#' @param pin_name pin name of dlw data
#'
#' @returns character with pin_name
#' @rdname load_pip_data
#' @keywords internal
#'
#' @examples
#' check_dlw_pin_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs")
#' check_dlw_pin_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG")
check_pip_pin_name <- \(pin_name) {
  pin_name <- pin_name |>
    fs::path_ext_remove() |>
    fs::path(ext = "qs")

  ptt <- get_from_piploadenv("pip_name_pattern")

  if (!grepl(ptt, pin_name)) {
    cli::cli_abort(c(x = "Wrong {.arg pin_name} specification",
                     i = "it should follow the pattern {.field {ptt}}",
                     i = "like in {.file LCA_2015_SLCHBS_INC_GPWG}"))
  }
  invisible(pin_name)
}
