#' Title
#'
#' @param country_code Character: country ISO 3 code.
#' @param year numeric: four digit year
#' @param module character: module of GMD collection (e.g., ALL, GPWG, L)
#' @param survey character: survey acronyn
#' @param filename character: File name
#' @param vermast  character: Version of the master data in the form "vXX" where
#'   X is a number of two digits like "01" or "02".
#' @param veralt character: Version of the alternative  data in the form "vXX"
#'   where X is a number of two digits like "01" or "02".
#' @param latest_version logical: If TRUE and  `vermast` and `veralt` are NULL,
#'   it will use the most recent version of the data for a particular year.
#' @param  latest_year logical: If `TRUE` and  `year` is NULL, it retrieves the
#'   most recent year. Otherwise, it will return the calls for all the years
#'   available. This is the default.
#' @param verbose logical. If TRUE display information. Default is option "pipload.verbose"
#' @param ...
#'
#' @returns data table with dlw data
#' @export
#'
#' @examples
load_dlw_data <- function(country_code   = NULL,
                          year           = NULL,
                          module         = NULL,
                          survey         = NULL,
                          filename       = NULL,
                          vermast        = NULL,
                          veralt         = NULL,
                          latest_version = TRUE,
                          latest_year    = FALSE,
                          pin_name       = NULL,
                          version        = NULL,
                          verbose        =  getOption("pipload.verbose"),
                          ...) {

  # defenses   ---------

  ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_M_[Vv][0-9]{2}_A_[^_]+_[^_]+\\.[A-Za-z]+$"

  # computations   ---------

  # Get board
  br <- pipfun::get_pins_boards(board = "dlw_data")

  bl <- pins::pin_list(br)
  bd <- data.table(pin_name = bl)

  vars <- c(
    "Country_code",
    "Survey_year",
    "Survey_acronym",
    "Vermast",
    "M",
    "Veralt",
    "A",
    "Collection",
    "Module",
    "ext"
  )

  bd[, (vars) := tstrsplit(pin_name, split = "_|[.]", fill = NA)
  ][,
    c("M", "A") := NULL]


  ctl[grepl(ptt, FileName),
      (vars) := tstrsplit(FileName, split = "_|[.]", fill = NA)
  ][,
    c("M", "A") := NULL]




  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(TRUE)

}
