#' A package for loading PIP data
#'
#'
#' @section pipload functions:
#' The pipaux functions ...
#'
#' @docType package
#' @name pipload
#' @import data.table
#' @importFrom magrittr "%>%" "%<>%"
#' @export
magrittr::`%>%`

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      "cache_id",
      "country_code",
      "data",
      "filename",
      "filtered",
      "maxalt",
      "maxmast",
      "menu",
      "n_source",
      "orig",
      "survey_id_real",
      "veralt",
      "vermast",
      "menu",
      "creationtime",
      "fullname",
      "lastwritetime",
      "module",
      "survey_id",
      "surveyid_year",
      ".",
      "!!",
      ":="
    ),
    package = utils::packageName()
  )
}


NULL

