#' pip_find_data
#' Find surveys available for PIP
#'
#' @param country character: vector of ISO3 country codes.
#' @param year    numeric: survey year
#' @param survey_acronym  character: Survey acronym
#' @param vermast character: Master version in the form v## or ##
#' @param veralt  character: Alternative version in the form v## or ##
#' @param module  character: combination of `tool` and `source` separated by
#' a hyphen (e.g., PC-GPWG)
#' @param tool    character: PIP tool in which data will be used. It could be
#' `PC` for Poverty Calculator or `TB` for Table Maker. Others will be added
#' @param source  character: Source of data. It could be `GPWG`, `HIST`, `GROUP`,
#' `synth`, `BIN`, and `ALL`. The latter is used only in Table Maker.
#' @param maindir character: Main directory
#'
#' @return data.frame: list of filenames to be loaded with pcn_load()
#' @import data.table
#' @export
#'
#' @examples
#' # all years for one country
#' pip_find_data(country = "ARG")
#'
#' #' # all years for more than one country
#' pip_find_data(country = c("COL", "ARG"))
#'
#' # specific years for one country
#' pip_find_data(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_find_data(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_find_data(country = "PRY",
#'              year = 2010,
#'              module = "PC-GPWG")
#'
#' # Load different sources
#' pip_find_data(country = "COL",
#'              source = "HIST")
#
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_find_data(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_find_data()
#' }

pip_find_data <- function(country         = NULL,
                         year             = NULL,
                         survey_acronym   = NULL,
                         vermast          = NULL,
                         veralt           = NULL,
                         module           = NULL,
                         tool             = NULL,
                         source           = NULL,
                         maindir          = getOption("pip.maindir")
                         ) {
  #--------- Initial conditions

  # drive and main dir

  if (!dir.exists(maindir)) {
    st_msg <- paste0("main directory `",
                     maindir,
                     "` not reachable.")
    rlang::abort(c(
                  st_msg,
                  i = "Check connection"
                  ),
                  class = "pipload_error"
                  )

  }


  #----------------------------------------------------------
  #   Country condition
  #----------------------------------------------------------

  if (is.null(country)) {
    argum <- c(year, survey_acronym, vermast, veralt)
    if (length(argum)) {
      rlang::inform(
        paste("if `country` is NULL, arguments `year`, `survey_acronym`\n",
              "`vermast`, and `veralt` should be NULL as well\n",
              "These arguments are coerced to NULL")
      )

    }

    # Load country names when no country is selected
    countries <- fs::dir_ls(maindir)
    countries <- gsub(maindir, "", countries)
    countries <- countries[!grepl("^_", countries)]  # remove _aux folder

    years      <- NULL

  } else { # country is selected
    lyear      <- length(year)
    lcountry   <- length(country)

    if ( lyear != 0 ) {       # if year is selected along with country

      if (lcountry != 1) {    # if more than one country selected

        if ( lyear != 1) {
          rlang::warn(c(
                      paste0("Since `length(country)` is greater than 1,\n",
                             "the first value of `year` (",
                             year[[1]], ") wille be used"),
                        i = paste("length(country) == " , lcountry)
                        ),
                        class = "pipload_warning"
                        )
        }

        countries  <- country
        years      <- rep(year[[1]], lcountry)

      } else {              # if only one country selected

        countries <- rep(country, lyear)
        years     <- year

      }
    } else  { # if lyear == 0
      countries  <- country
      years      <- NULL
    }
  }   # end of country no NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Load Inventory database   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  #--------- create conditions ---------
  # Country and year are special because the names are different
  condi <- "country_code %chin% (countries)"

  if (!(is.null(years))) {
    condi <- paste(condi, "& year %chin% as.character(years)")
  }

  # The other arguments work fine. Just add "alt_" prefix
  argus <-
    c("survey_acronym",
      "vermast",
      "veralt",
      "module",
      "tool",
      "source")

  for (i in seq_along(argus)) {

    # if argument is NOT NULL
    y <- argus[i]
    if (!(is.null(get(y)))) {

      assign(paste0("alt_", y), toupper(get(y)))
      condi <- paste(condi, create_cond(y))

    }
  }

  #--------- load data ---------
  condi <- parse(text = condi)

  df <- pip_load_inventory()
  df <- df[ eval(condi)]

  return(df)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

create_cond <- function(x) {
  cd <- paste0("& toupper(", x, ") %chin% (alt_", x, ")")
  return(cd)
}

