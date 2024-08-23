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



