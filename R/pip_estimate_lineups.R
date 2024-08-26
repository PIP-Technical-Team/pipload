#' Get multiplication factor and add to refy data frame
#'
#' Multiplication factor
#'
#' @param df_refy data frame: reference year (refy) table with estimation type, monotonicity, national accounts growth rates, means, etc. already calculated for each country ref-year
#'
#' @return data frame: refy table with a mult_factor column
#' @export
get_refy_mult_factor <- function(df_refy) {

  df_refy |>
    fmutate(
      lineup_approach = fcase(
        estimation_type == "extrapolation" ,
        "extrapolation",
        estimation_type == "interpolation" & monotonic == TRUE & same_direction == TRUE, "interpolation_same",
        estimation_type == "interpolation" & !(monotonic == TRUE & same_direction == TRUE),
        "interpolation_diverge",
        default = "survey"
      ),
      mult_factor = fcase(
        lineup_approach == "extrapolation" | lineup_approach == "interpolation_diverge",
        nac / nac_sy,
        lineup_approach == "interpolation_same",
        predicted_mean_ppp / svy_mean,
        default = 1
      )
    )

}

