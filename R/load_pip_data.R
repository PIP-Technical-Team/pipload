#' Set of functions to interact with PIP data.
#'
#' [load_pip_data] Load data from pip repository. [find_pip_data] Finds data
#' available. [load_pip_inventory_release] Loads pip inventory for
#' corresponding release. [load_pip_master_inventory] Loads pip master inventory
#' for corresponding release. [check_pip_id_name] checks that pip id_name is
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
#' @param id_name character: id name
#' @param vermast  character: Version of the master data in the form "vXX" where
#'   X is a number of two digits like "01" or "02".
#' @param veralt character: Version of the alternative  data in the form "vXX"
#'   where X is a number of two digits like "01" or "02".
#' @param collection character: It should always be "GMD"
#' @param latest_version logical: If TRUE and  `vermast` and `veralt` are NULL,
#'   it will use the most recent version of the data given by stamp.
#' @param latest_year logical: If `TRUE` and  `year` is NULL, it retrieves the
#'   most recent year. Otherwise, it will return the calls for all the years
#'   available. This is the default.
#' @param verbose logical. If TRUE display information. Default is option
#'   "pipload.verbose"
#' @param metadata logical: If TRUE, it will load metadata instead of data. Default is FALSE
#' @param where character: Either `"release"` or `"master"` indicating where to look for data. Default is `"release"`.
#' @param version character: Specific data version to load; forwarded to `pip_read`. Default is `NULL` (latest if available).
#' @param format character: Data format to read (for example, `"qs2"`). Default is `"qs2"`.
#'
#' @return data.table with pip data. Note: one of `country_code` or `id_name` must be provided.
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
#' load_pip_data(id_name = "LCA_2015_SLCHBS_INC_GPWG")
#'
#' }
load_pip_data <- function(
  country_code = NULL,
  surveyid_year = NULL,
  survey_acronym = NULL,
  welfare_type = NULL,
  module = NULL,
  id_name = NULL,
  vermast = NULL,
  veralt = NULL,
  collection = NULL,
  latest_version = TRUE,
  latest_year = FALSE,
  where = c("release", "master"),
  version = NULL,
  verbose = getOption("pipload.verbose"),
  format = "qs2",
  metadata = FALSE
) {
  # Defenses
  if (is.null(country_code) && is.null(id_name)) {
    cli::cli_abort(c(
      x = "One of {.arg country_code} or {.arg id_name} must be provided."
    ))
  }

  # computations   --------
  where <- match.arg(where)

  # Get folder path
  if (metadata) {
    dir <- pipfun::get_pip_folders(folder = "pip_metadata", verbose = FALSE)
  } else {
    dir <- pipfun::get_pip_folders(folder = "pip_data", verbose = FALSE)
  }

  # When id name is defined   ------
  if (!is.null(id_name)) {
    id_name <- check_pip_id_name(id_name)
  } else {
    # filter   ---------

    inv <- find_pip_data(
      latest_version = latest_version,
      latest_year = latest_year,
      where = where,
      verbose = verbose,
      country_code = country_code,
      surveyid_year = surveyid_year,
      survey_acronym = survey_acronym,
      welfare_type = welfare_type,
      module = module,
      vermast = vermast,
      veralt = veralt,
      collection = collection
    )

    id_name <- inv[, pip_id]
  }

  if (length(id_name) != 1) {
    cli::cli_abort(c(
      x = "Wrong number of data to load.",
      i = "It should be only 1. You attempt to load {.field {length(id_name)}}:",
      "{.field {id_name}}"
    ))
  }

  # Return   ---------
  if (verbose) {
    cli::cli_alert_info("Loading {.field {id_name}}")
  }

  # Look up alias for pip_data folder
  alias_list <- stamp::st_alias_list()
  alias <- alias_list[alias_list$root == dir, "alias"]

  if (length(alias) == 0) {
    cli::cli_abort(c(
      x = "PIP data folder not initialized in stamp.",
      i = "Run {.code pipfun::setup_working_release()} first and make sure the pip_data folder is set."
    ))
  }

  return(pip_read(
    id = id_name,
    alias = alias,
    version = version,
    format = format,
    verbose = verbose
  ))
}


#' @param where character: Either "release" or "master". Se details.
#' @param ... Just the following: country_code, year, survey, welfare_type,and
#'   module. They work exactly the same as the ones in [load_pip_data]
#'
#' @return filtered data table from inventory
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
#' # Find data
#' find_pip_data(country_code = "HRV")
#'
#' # Latest year in EACH module
#' find_pip_data(country_code = "HRV", latest_year = TRUE)
#' }
find_pip_data <- function(
  latest_year = FALSE,
  where = c("release", "master"),
  verbose = getOption("pipload.verbose"),
  ...
) {
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
      paste0("(", paste(args[[.]], collapse = "|"), ")")
    }
  }) |>
    # append them together
    paste(collapse = "_")

  if (where == "master") {
    ctl <- load_pip_master_inventory(verbose = verbose) |>
      setDT()
  } else {
    ctl <- load_pip_release_inventory(verbose = verbose) |>
      setDT()
  }
  ctl <- ctl[grepl(pattern, pip_id)]

  if (latest_year == TRUE && !("surveyid_year" %in% names(args))) {
    #need to check because it was args_info before instead of names(args)
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


#' @return data.table with PIP inventory for the current release
#' @rdname load_pip_data
#' @export
#'
#' @examples
#' load_pip_release_inventory()
load_pip_release_inventory <- \(
  version = NULL,
  verbose = getOption("pipload.verbose"),
  format = "qs2"
) {
  dir_inv <- pipfun::get_pip_folders(folder = "pip_inventory", verbose = FALSE)

  alias_list <- stamp::st_alias_list()
  alias <- alias_list[alias_list$root == dir_inv, "alias"]

  if (length(alias) == 0) {
    cli::cli_abort(c(
      x = "PIP inventory folder not initialized in stamp.",
      i = "Run {.code pipfun::setup_working_release()} first."
    ))
  }

  pip_read(
    "pip_release_inventory",
    alias = alias,
    version = version,
    verbose = verbose,
    format = format
  )
}


#' @return data.table with PIP master inventory
#' @rdname load_pip_data
#' @export
#'
#' @examples
#' load_pip_master_inventory()
load_pip_master_inventory <- \(
  format = "qs2",
  version = NULL,
  verbose = getOption("pipload.verbose")
) {
  dir_inv <- pipfun::get_pip_folders(
    folder = "pip_master_inventory",
    verbose = FALSE
  )

  alias_list <- stamp::st_alias_list()
  alias <- alias_list[alias_list$root == dir_inv, "alias"]

  if (length(alias) == 0) {
    cli::cli_abort(c(
      x = "PIP master inventory folder not initialized in stamp.",
      i = "Run {.code pipfun::setup_working_release()} first."
    ))
  }

  pip_read(
    "pip_master_inventory",
    alias = alias,
    version = version,
    verbose = verbose,
    format = format
  )
}


#' @param id_name id name of pip data
#'
#' @return character with id_name
#' @rdname load_pip_data
#' @keywords internal
#'
#' @examples
#' check_pip_id_name("AGO_2000_HBS_CON_GPWG.qs2")
#' check_pip_id_name("AGO_2000_HBS_CON_GPWG")
check_pip_id_name <- \(id_name) {
  id_name <- id_name |>
    fs::path_ext_remove() |>
    fs::path(ext = "qs2")

  ptt <- get_from_piploadenv("pip_name_pattern")

  if (!grepl(ptt, id_name)) {
    cli::cli_abort(c(
      x = "Wrong {.arg id_name} specification",
      i = "it should follow the pattern {.field {ptt}}",
      i = "like in {.file LCA_2015_SLCHBS_INC_GPWG}"
    ))
  }
  invisible(id_name)
}
