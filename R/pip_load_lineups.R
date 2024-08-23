# pip_load_refy


#' Load lineup data for specific reference year
#'
#' This function loads a `.qs` file containing lineup distribution for a specific country and year.
#'
#' @param country_code A string representing the country code (e.g., "ZAF", "COL" for South Africa and Colombia respectively). This is used to locate the appropriate file.
#' @param year A numeric or string value representing the year (e.g., 2010). This is combined with `country_code` to form the filename.
#' @param path A string representing the directory path where the `.qs` files are stored.
#'
#' @return The function returns the data loaded from the `.qs` file. The format of the data depends on the contents of the file.
#' @export
#'
#' @examples
#' \dontrun{
#' ZAF_2020_lineups <- load_refy("ZAF", 2020)
#' COL_2015_lineups <- load_refy("COL", "2015")
#' }
load_refy <- function(country_code,
                      year,
                      path = Sys.getenv("PIP_LINEUPS_DIR")) {
  qs::qread(file =
              fs::path(path,
                       paste0(country_code,
                              "_",
                              year,
                              ".qs")))

}




#' Load and append lineup distributions for given country-years
#'
#' This function loads and appends multiple data frames containing lineup distribution data for specific countries and years.
#' It also stores the attributes of each loaded dataset in the returned data frame.
#'
#' @inheritParams transform_input
#' @inheritParams load_refy
#'
#' @return A data table containing the appended lineup data for all specified countries and years. The attributes of each loaded dataset are stored as additional attributes in the returned data table.
#' @export
#'
#' @examples
#' \dontrun{
#' input_list <- list(
#'   list(country_code = "ZAF", year = 2020),
#'   list(country_code = "COL", year = 2015)
#' )
#' all_lineups <- load_append_refy(input_list)
#' }
load_append_refy <- function(input_list,
                             path = Sys.getenv("PIP_LINEUPS_DIR")) {

  # transform input list
  input_list <- transform_input(input_list)

  # envir for attributes
  e <- rlang::new_environment()

  # appended data
  dt <- lapply(input_list,
               FUN = \(x) {
                 d <-
                   load_refy(country_code = x$country_code,
                             year         = x$year,
                             path         = path)
                 dattr <- attributes(d)
                 assign(x     = paste0(x$country_code,
                                       x$year,
                                       "_attr"),
                        value = dattr,
                        envir = e)
                 d |>
                   fmutate(country_code = dattr$country_code,
                           year         = dattr$reporting_year)
               })

  # rowbind
  dt <- rowbind(dt)

  # lsit of attributes
  dattr <- as.list(e)
  attributes(dt) <- c(attributes(dt),
                      as.list(e))

  dt

}

#' Load only attributes from lineup data frame
#'
#' This function loads the attributes data frame containing lineup distribution data for a specific country and year.
#'
#' @inheritParams load_refy
#'
#' @return A list containing the attributes of the specified `.qs` file.
#' @export
#'
#' @examples
#' \dontrun{
#' zaf_attrs <- load_attr("ZAF", 2020)
#' col_attrs <- load_attr("COL", 2015)
#' }
load_attr <- function(country_code,
                      year,
                      path = Sys.getenv("PIP_LINEUPS_DIR")) {

  qs::qattributes(fs::path(path,
                           paste0(country_code,
                                  "_",
                                  year,
                                  ".qs")))

}




#' Load distribution statistics from lineup data frame
#'
#' This function extracts the distribution statistics from the attributes of a file containing lineup distribution data for a specific country and year.
#'
#' @inheritParams load_refy
#'
#' @return A list containing the distribution statistics from the attributes of the specified `.qs` file.
#' @export
#'
#' @examples
#' \dontrun{
#' zaf_dist_stats <- load_dist_stats("ZAF", 2020)
#' col_dist_stats <- load_dist_stats("COL", 2015)
#' }
load_dist_stats <- function(country_code,
                            year,
                            path = Sys.getenv("PIP_LINEUPS_DIR")) {

  all_attr <- load_attr(country_code,
                        year,
                        path)

  all_attr$dist_stats

}





#' Load auxiliary data from lineup data frame
#'
#' This function extracts the auxiliary data from the attributes of a file containing lineup distribution data for a specific country and year.
#'
#' @inheritParams load_refy
#'
#' @return A list containing the auxiliary data from the attributes of the specified `.qs` file.
#' @export
#'
#' @examples
#' \dontrun{
#' zaf_aux_data <- load_aux_data("ZAF", 2020)
#' col_aux_data <- load_aux_data("COL", 2015)
#' }
load_aux_data <- function(country_code,
                          year,
                          path = Sys.getenv("PIP_LINEUPS_DIR")) {

  all_attr <- load_attr(country_code,
                        year,
                        path)

  all_attr$aux_data

}





