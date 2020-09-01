#' Load PIP microdata in different way
#'
#' @inheritParams pip_data_find
#' @param type character: Either `dataframe` or `list`. Defaults is `dataframe`.
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' # all years for one country
#' pip_data_load(country = "PRY")
#'
#' # specific years for one country
#' pip_data_load(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_data_load(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_data_load(country = "PRY",
#'              year = 2010,
#'              module = "PC-GPWG")
#'
#' # Load different sources
#' pip_data_load(country = "COL",
#'              source = "HIST")
#
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_data_load(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_data_load()
#' }
pip_data_load <- function(country          = NULL,
                          year             = NULL,
                          survey_acronym   = NULL,
                          vermast          = NULL,
                          veralt           = NULL,
                          module           = NULL,
                          tool             = NULL,
                          source           = NULL,
                          type             = "dataframe",
                          maindir          = getOption("pip.maindir")
                          ) {

  # Call data find to get inventory
  df <- pip_data_find(country          = country       ,
                      year             = year          ,
                      survey_acronym   = survey_acronym,
                      vermast          = vermast       ,
                      veralt           = veralt        ,
                      module           = module        ,
                      tool             = tool          ,
                      source           = source        ,
                      maindir          = maindir)

  #--------- Filter most recent version ---------
  # master version
  if (is.null(vermast)) {
    df[,
       maxmast := vermast == max(vermast),
       by = .(country_code, year, survey_acronym, module)
       ][
         maxmast == 1
         ][,
           maxmast := NULL
         ]
  }

  # Alternative version
  if (is.null(veralt)) {
    df[,
       maxalt := veralt == max(veralt),
       by = .(country_code, year, survey_acronym, module)
       ][
         maxalt == 1
         ][,
           maxalt := NULL
         ]
  }


  if (type == "dataframe") {
    dt <- purrr::map2(.x = df$orig,
                      .y = df$filename,
                      .f = data_to_df)
    dt <- rbindlist(dt, fill = TRUE)
    return(dt)

  } else if (type == "list") {

    dl <- purrr::map(.x = df$orig,
                     .f = haven::read_dta)

    y  <- gsub("\\.dta", "", unique(df$filename))
    names(dl) <- y
    return(dl)

  } else {
    rlang::abort(c(
                  "The `type` selected is not a valid name",
                  i = "you can use `dataframe` or `list`", # update this message automatically
                  x = paste("you specified", type)
                ),
                class = "pipload_error"
                )
  }

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_to_df <- function(x, y) {
  df <- haven::read_dta(x)
  y  <- gsub("\\.dta", "", y)
  df$survey_id <- y
  setDT(df)
  return(df)
}
