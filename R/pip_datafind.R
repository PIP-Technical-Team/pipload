#' pip_datafind
#' Find surveys available for PIP
#'
#' @param country character: vector of ISO3 country codes.
#' @param year    numeric: survey year
#' @param survey  character: Survey acronym
#' @param vermast character: Master version in the form v## or ##
#' @param veralt  character: Alternative version in the form v## or ##
#' @param type    character: Type of alternative version
#' @param maindir character: Main directory
#' @param drive   character: mapped drive
#'
#' @return data.frame: list of filenames to be loaded with pcn_load()
#' @import data.table
#' @export
#'
#' @examples
#' # all years for one country
#' pip_datafind(country = "ARG")
#'
#' #' # all years for more than one country
#' pip_datafind(country = c("COL", "ARG"))
#'
#' # specific years for one country
#' pip_datafind(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_datafind(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_datafind(country = "PRY",
#'              year = 2010,
#'              module = "GPWG")
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_datafind(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_datafind()
#' }

pip_datafind <- function(country          = NULL,
                         year             = NULL,
                         survey           = NULL,
                         vermast          = NULL,
                         veralt           = NULL,
                         characteristics  = TRUE,
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
    argum <- c(year, survey, vermast, veralt)
    if (length(argum)) {
      rlang::inform(
        paste("if `country` is NULL, arguments `year`, `survey`\n",
              "`vermast`, and `veralt` should be NULL as well\n",
              "These arguments are coerced to NULL")
      )

    }

    # Load country names when no country is selected
    countries <- dir(maindir)
    countries <- countries[!grepl("^_", countries)]  # remove _aux folder

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
    }
  }   # end of country no NULL

  invisible()
}


#----------------------------------------------------------
#   Auxiliary functions
#----------------------------------------------------------

find_filename <- function(country = NULL,
                          year    = NULL,
                          maindir) {


  modules <- c("PCN", "GROUP")
  nmods <- length(modules)
  filename <- NULL
  i <- 1
  while (length(filename) == 0 && i <= nmods) {

    pattern <- paste0(modules[i], "[\\-]?[URNA]?\\.dta$")

    if (length(country) > 0) {
      country <- toupper(country)
      cdir <- paste0(maindir, "/", country)

      if (length(year) > 0) {
        pattern <- paste0(".*", year, ".*", modules[i], "[\\-]?[URNA]?\\.dta$")
      }

    } else {
      cdir <- maindir
      if (length(year) > 0) {
        warning("argument `year` ignored when no country is selected")
      }
    }


    filepath <- list.files(path = cdir,
                           pattern = pattern,
                           recursive = TRUE,
                           full.names = TRUE)

    filename <- gsub(pattern = paste0("(.*[Dd]ata/)(.*)\\.dta$"),
                     replacement = "\\2",
                     x =  filepath)
    i <- i + 1
  } # end of while


  if (length(filename) == 0) {

    if (length(year) > 0) {
      filename <- filepath <- paste(country, year, "NotFound", sep = "_")
    } else {
      filename <- filepath <- paste(country, "NotFound", sep = "_")
    }
  } # if no file was found on any module

  return(data.table::data.table(filepath,
                                filename))
  # return(tibble::tibble(filename))
} # end of find_filename


