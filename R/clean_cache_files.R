
#' Create key for cache data attributes
#'
#' Columns in cache data will be made into attributes. These attributes will be
#' mapped to the correct row through the use of a unique key identifier.
#'
#' @param df_cache cache data frame for which key should be created
#'
#' @return cache data frame with added key
#' @export
create_cache_key <- function(df_cache) {

  # create key column
  country <- df_cache$country_code |>
    funique()
  year    <- df_cache$surveyid_year
  key     <- paste0(country,
                    "_",
                    year,
                    "_",
                    c(1:nrow(df_cache)))

  df_cache <-
    df_cache |>
    fmutate(key = key)

  data.table::setkey(df_cache,
                     key)
  df_cache

}
