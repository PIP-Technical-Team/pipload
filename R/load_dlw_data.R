#' Load and find data from dlw repository
#'
#' [load_dlw_data] loads data from dlw. [find_dlw_data] Find data available in
#' DLW data board. [load_dlw_gmd_inventory] loads inventory of GMD data from
#' DLW. [load_dlw_gmd_log] loads GMD log of GMD from DLW. [load_gmd_valid_inv]
#' loads GMD validated inventory. [load_gmd_valid_log] loads GMD validation
#'   workflow log. [load_gmd_valid_report] loads GMD validation report.
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
#' # Using id_name
#' load_dlw_data(id_name = "HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs")
#'
#' # without ext also works
#' load_dlw_data(id_name = "HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG")
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
                          surveyid_year  = NULL,
                          survey_acronym = NULL,
                          vermast        = NULL,
                          veralt         = NULL,
                          collection     = "GMD",
                          module         = "GPWG",
                          latest_version = TRUE,
                          latest_year    = FALSE,
                          id_name       = NULL,
                          version        = NULL,
                          verbose        =  getOption("pipload.verbose")) {

  # defenses   ---------
  stopifnot(exprs = {
    !is.null(country_code) || !is.null(id_name)
  })

  # Get dir
  dir <- pipfun::get_pip_folders(folder = "dlw_data")

  # When id name is defined   ------
  if (!is.null(id_name)) {
    id_name <- check_dlw_id_name(id_name)

  } else {
    # filter   ---------

    fd <- find_dlw_data(dir            = dir,
                        latest_version = latest_version,
                        latest_year    = latest_year,
                        verbose        = verbose,
                        country_code   = country_code,
                        surveyid_year  = surveyid_year,
                        survey_acronym = survey_acronym,
                        vermast        = vermast,
                        veralt         = veralt,
                        collection     = collection,
                        module         = module)

    id_name <- fd[, id_name]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (verbose) {
    cli::cli_alert_info("Loading {.field {id_name}}")
  }
  return(pip_read(id = id_name,
                  dir = dir,
                  version = version))

}


#' @param dir dir from `pipfun::get_pip_folders(folder = "dlw_data")`
#' @inheritParams load_dlw_data
#' @param ... Just the following: country_code, year, module, survey, vermast,
#'   veralt, collection, module. They work exactly the same as the ones in
#'   [load_dlw_data]
#'
#' @returns data from with filter data
#' @rdname load_dlw_data
#' @export
find_dlw_data <- function(dir = NULL,
                          latest_version = TRUE,
                          latest_year    = FALSE,
                          verbose        =  getOption("pipload.verbose"),
                          ...) {
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

  # get names of arguments that are not null
  args_info <- Filter(Negate(is.null), args) |>
    names()

  cnds <- lapply(args_info, \(.) {
    paste(simpleCap(.), ., sep = " %in% ")
  }) |>
    # append them together
    paste(collapse = " & ") |>
    # convert to expression
    rlang::parse_expr()


  if (is.null(dir)) {
    dir <- pipfun::get_pip_folders(folder = "dlw_data")
  }
  # PATCH -> Need to create function
  bl <- setdiff(list.files(pipfun::get_pip_folders()$pip_data),
                list.files(pipfun::get_pip_folders()$pip_data, pattern = "\\.lock$"))

  # Build catalog
  ctl <- data.table(id_name = bl)

  vars <- c(
    "Country_code",
    "surveyid_year",
    "survey_acronym",
    "Vermast",
    "M",
    "Veralt",
    "A",
    "Collection",
    "Module",
    "ext"
  )

  ctl[, (vars) := tstrsplit(id_name, split = "_|[.]", fill = NA)
  ][,
    c("M", "A") := NULL]


  ctl <- ctl[rlang::eval_tidy(cnds, data = args)] |>
    unique()

  if (latest_year == TRUE && !("year" %in% args_info)) {
    ctl <- ctl[,
               #  for each collection and module, the row(s) with the maximum Year
               .SD[Year == max(Year, na.rm = TRUE)],
               by = .(Collection, Module)
    ]
  }

  if (!("vermast" %in% args_info) &&
      !("veralt" %in% args_info) &&
      latest_version == TRUE) {
    ctl <- ctl[ ,
                #  for each year, the row(s) with the maximum Vermast.
                .SD[Vermast == max(Vermast, na.rm = TRUE)],
                by = .(Year, Collection, Module)
    ][,
      #It should return only one row per year (even if there are ties)
      .SD[Veralt == max(Veralt, na.rm = TRUE)],
      by = .(Year, Collection, Module)]
  }

  return(ctl)
}



#' check that dlw id_name is correct
#'
#' @param id_name id name of dlw data
#'
#' @returns character with id_name
#' @keywords internal
#' @rdname load_dlw_data
#'
#' @examples
#' check_dlw_id_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs")
#' check_dlw_id_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG")
check_dlw_id_name <- \(id_name) {
  id_name <- id_name |>
    fs::path_ext_remove() |>
    fs::path(ext = "qs")

  ptt <- get_from_piploadenv("dlw_name_pattern")

  if (!grepl(ptt, id_name)) {
    cli::cli_abort(c(x = "Wrong {.arg id_name} specification",
                     i = "it should follow the pattern {.field {ptt}}",
                     i = "like in {.file HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs}"))
  }
  invisible(id_name)
}





#' @returns [load_dlw_gmd_inventory] data.table with inventory of GMD data from
#'   DLW
#' @rdname load_dlw_data
#' @export
#'
#' @examples
#' load_dlw_gmd_inventory()
load_dlw_gmd_inventory <- \() {
  binv <- pipfun::get_pip_folders(folder = "dlw_inventory")
  pip_read("dlw_gmd_inv",dir = binv)
}


#' @returns [load_dlw_gmd_log] data.table with log of retrieval process from DLW
#' @rdname load_dlw_data
#' @export
#'
#' @examples
#' load_dlw_gmd_log()
load_dlw_gmd_log <- \() {
  binv <- pipfun::get_pip_folders(folder = "dlw_inventory")
  pip_read("dlw_gmd_log", dir = binv)
}

#' @returns [load_gmd_valid_inv] data.table with inventory of validated GMD data
#' @rdname load_dlw_data
#' @export
#'
#' @examples
#' load_gmd_valid_inv()
load_gmd_valid_inv <- \() {
  binv <- pipfun::get_pip_folders(folder = "dlw_metadata")
  pip_read("gmd_valid_inv", dir = binv)
}

#' @returns [load_gmd_valid_log] data.table with log of GMD validated workflow
#' @rdname load_dlw_data
#' @export
#'
#' @examples
#' load_gmd_valid_log()
load_gmd_valid_log <- \() {
  binv <- pipfun::get_pip_folders(folder = "dlw_metadata")
  pip_read("dlw_validation_log", dir = binv)
}

#' @returns [load_gmd_valid_report] data.table with validation report data
#' @rdname load_dlw_data
#' @export
#'
#' @examples
#' load_gmd_valid_report()
load_gmd_valid_report <- \() {
  binv <- pipfun::get_pip_folders(folder = "dlw_metadata")
  pip_read("validation_report", dir = binv)
}
