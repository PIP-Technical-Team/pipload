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
                              year
                              ), ext = "qs"))

}




#' Load lineup distributions as a list for given country-years
#'
#' This function loads multiple data frames as a list containing lineup distribution data for specific countries and years.
#' It also stores the attributes of each loaded dataset in the returned data frame.
#'
#' @inheritParams transform_input
#' @inheritParams load_refy
#'
#' @return A list containing the appended lineup data for all specified countries and years. The attributes of each loaded dataset are stored as additional attributes in the returned data table.
#' @export
#'
#' @examples
#' \dontrun{
#' input_list <- list(
#'   country_code = c("ZAF", "COL"),
#'   year = list(c(2020, 2021), c(2015, 2016))
#' )
#' all_lineups <- load_append_refy(input_list)
#' }
load_list_refy <- function(input_list,
                           path = Sys.getenv("PIP_LINEUPS_DIR")) {

  # transform input list
  input_list <- transform_input(input_list)

  # appended data
  dl <- lapply(input_list,
               FUN = \(x) {
                 d <-
                   load_refy(country_code = x$country_code,
                             year         = x$year,
                             path         = path)

                 d |>
                   fmutate(country_code = x$country_code,
                           year         = x$year)
               })

  names(dl) <- vapply(input_list,
                      FUN = \(x) {
                        paste0(x$country_code,
                               x$year)
                      },
                      FUN.VALUE = character(1))

  dl

}


#' Append reference year data and store attributes
#'
#' @param d_list list of data frames, output from [load_list_refy]
#' @param add_columns character vector: contains a combination of "reporting_level",
#' "welfare_type", and "survey_years"
#'
#' @return data frame: single appended data frame
#' @export
#'
#' @examples
#' input_list <- list(country_code = c("ZAF", "COL"),
#'                    year         = list(c(2020, 2021),
#'                                        c(2015, 2016)))
#' x <- load_list_refy(input_list) |>
#'         append_refy_dt(add_columns = c("reporting_level",
#'                                         "welfare_type"))
#'
append_refy_dt <- function(d_list, add_columns) {

  # envir for attributes
  e <- rlang::new_environment()

  d_list <- lapply(d_list,
         FUN = \(x) {

           dattr <- attributes(x)
           assign(x     = paste0(dattr$country_code,
                                 dattr$reporting_year,
                                 "_attr"),
                  value = dattr,
                  envir = e)

           if ("reporting_level" %in% add_columns) {
             rp <- rep(dattr$reporting_level_rows$reporting_level,
                       times = dattr$reporting_level_rows$rows)
             x <- x |>
               fmutate(reporting_level = rp)
           }

           if ("survey_years" %in% add_columns) {
             sy <- rep(dattr$survey_years_rows$survey_years,
                       times = dattr$survey_years_rows$rows)
             x <- x |>
               fmutate(survey_years = sy)
           }

           if ("welfare_type" %in% add_columns) {
             x <- x |>
               fmutate(welfare_type = dattr$welfare_type)
           }

           x

  })

  # rowbind
  dt <- rowbind(d_list)

  # list of attributes
  dattr <- as.list(e)
  attributes(dt) <- c(attributes(dt), # set attributes
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






# helper functions


#' Transform Input List for Country-Year Combinations
#'
#' @param input_list A list with two elements: `country_code` and `year`.
#'   - `country_code`: A vector of country codes (e.g., "ZAF", "COL").
#'   - `year`: Either a single value (numeric or string) representing a year to be applied to all country codes, or a list of years where each element corresponds to a country code. If `year` is provided as a list, its length must match the length of `country_code`.
#'
#' @return A list where each element is a list containing `country_code` and `year`, representing all appropriate combinations of the input `country_code` and `year`.
#'
#' @examples
#' \dontrun{
#' # Example with a single year for all countries
#' input_list <- list(
#'   country_code = c("ZAF", "COL"),
#'   year = 2020
#' )
#' transformed_list <- transform_input(input_list)
#'
#' # Example with different years for each country
#' input_list <- list(
#'   country_code = c("ZAF", "COL"),
#'   year = list(c(2020, 2021), c(2015, 2016))
#' )
#' transformed_list <- transform_input(input_list)
#' }
transform_input <- function(input_list) {

  country_codes <- input_list$country_code
  years         <- input_list$year

  # years as list
  if (!is.list(years)) {
    years <- lapply(country_codes,
                    function(x) years)
  } else {
    # Check if the length of the year list matches the length of the country_codes
    if (length(years) != length(country_codes)) {
      stop("The length of the 'year' list must match the length of the 'country_code' vector.")
    }
  }

  # each element one country-year
  output_list <- lapply(seq_along(country_codes), function(i) {
    lapply(years[[i]], function(y) {
      list(country_code = country_codes[i], year = y)
    })
  })

  # flatten
  output_list <- unlist(output_list, recursive = FALSE)

  return(output_list)
}




