
#' Estimate reference year distribution for given country, year
#'
#' @param df_refy data frame: output from [get_refy_mult_factor]
#' @param cntry_code country code
#' @param ref_year reference year for lineup
#' @param gls global list
#'
#' @return data frame:
#' @export
get_refy_distributions <- function(df_refy, cntry_code, ref_year, gls) {

  # ensure no factors
  lapply(df_refy,
         FUN = function(x) {
           if (is.factor(x)) {
             as.character(x)
           } else {
             x
           }
         }) |>
    qDT()
  # Filter df_refy
  df_refy <-
    df_refy |>
    fsubset(country_code == cntry_code &
              reporting_year == ref_year) |>
    fselect("country_code",
            "reporting_level",
            "welfare_type",
            "income_group_code",
            "survey_year",
            "reporting_year",
            #"nac",
            #"nac_sy",
            "relative_distance",
            #"estimation_type",
            #"lineup_case",
            #"interpolation_id",
            #"predicted_mean_ppp",
            #"reporting_gdp",
            #"reporting_pce",
            "reporting_pop",
            #"monotonic",
            #"same_direction",
            #"svy_mean",
            "survey_id",
            "cache_id",
            "wb_region_code",
            "pcn_region_code",
            "survey_acronym",
            #"survey_coverage",
            #"survey_comparability",
            #"comparable_spell",
            #"surveyid_year",
            #"survey_time",
            #"survey_mean_lcu",
            #"survey_mean_ppp",
            #"ppp",
            #"cpi",
            #"pop_data_level",
            #"gdp_data_level",
            #"pce_data_level",
            #"cpi_data_level",
            #"ppp_data_level",
            "distribution_type",
            #"gd_type",
            "is_interpolated",
            #"is_used_for_line_up",
            #"is_used_for_aggregation",
            #"display_cp",
            "lineup_approach",
            "mult_factor")


  # reduce df_refy
  df_refy <-
    df_refy |>
    vars_to_attr(vars = c(#"country_code",
      #"reporting_level",
      #"welfare_type",
      "income_group_code",
      #"survey_year",
      "reporting_year",
      "survey_id",
      #"cache_id",
      "wb_region_code",
      "pcn_region_code",
      "survey_acronym",
      "distribution_type",
      "is_interpolated",
      "lineup_approach"#,
      #"mult_factor"
    ))

  # Load surveys
  cache_id <- df_refy$cache_id |>
    funique()
  gv(df_refy,
     "cache_id") <- NULL
  survey_year_rows <- list()
  df_svy <- collapse::rowbind(lapply(as.list(cache_id),
                                     FUN = function(x){
                                       pipload::pip_load_cache(cache_id = x,
                                                               version  = gls$vintage_dir) |>
                                         fselect(country_code, surveyid_year, survey_acronym,
                                                 survey_year, welfare_ppp, weight,
                                                 reporting_level, welfare_type, imputation_id)

                                     }))

  # Make survey year rows an attribute
  survey_years_rows <- df_svy |>
    fselect(survey_year) |>
    fmutate(rows = 1:fnrow(df_svy)) |>
    fgroup_by(survey_year) |>
    fsummarise(rows = fmax(rows))

  survey_years_rows <-
    list(survey_years = survey_years_rows$survey_year,
         rows         = survey_years_rows$rows)

  # Make reporting level rows an attribute
  reporting_level_rows <- df_svy |>
    fselect(reporting_level) |>
    fmutate(rows = 1:fnrow(df_svy)) |>
    fgroup_by(reporting_level) |>
    fsummarise(rows = fmax(rows))

  reporting_level_rows <-
    list(reporting_level = as.character(reporting_level_rows$reporting_level),
         rows            = reporting_level_rows$rows)

  # Join welfare & weights vectors from surveys to df_refy
  df <-
    df_refy |>
    joyn(y          = df_svy,
         by         = c("country_code",
                        "reporting_level",
                        "welfare_type",
                        "survey_year"),
         keep       = "left",
         match_type = "1:m",
         verbose    = FALSE,
         reportvar  = FALSE) |>
    # Group by survey year
    fgroup_by(survey_year) |>
    # number of imputations per survey year (if micro data then n_imp = 1)
    fmutate(n_imp      = data.table::uniqueN(imputation_id),
            # population at survey (decimal) year found by summing survey weights
            svy_pop    = fsum(weight),
            # weights scaled by ratio of ref year and svy year pop
            #         to make relative to reference year
            # weights adj by rel dist to get weighted average of population at ref year
            weight_refy = weight * (reporting_pop / svy_pop) * # "adjust to WDI population" --> Andres, your comment
              relative_distance#,
            # ref year weights divided by number of imputations
            #      this should sum to population amount
            #weight_refy_adj = weight_refy / n_imp
    ) |>
    fungroup() |>
    fmutate(welfare_refy = welfare_ppp * mult_factor)

  # temp
  setkey(df, NULL)

  # Make welfare type an attribute
  df <-
    df |>
    vars_to_attr(vars = c("welfare_type"))

  # keep attributes of df_refy
  attributes(df) <- c(attributes(df),
                           attributes(df_refy)[-which(names(attributes(df_refy)) %in%
                                                   c("dim",
                                                     "row.names",
                                                     "names",
                                                     "class",
                                                     ".internal.selfref",
                                                     names(attributes(df))))])
  df <- vars_to_attr(df, "n_imp")
  attr(df,
       "survey_years_rows")    <- survey_years_rows

  df <- df |>
    vars_to_attr(var = c("country_code",
                         "survey_acronym",
                         "survey_year"))

  dist_stats <- dist_stats(df = df)
  attr(df,
       "dist_stats") <- dist_stats

  attr(df,
       "reporting_level_rows") <- reporting_level_rows

  gv(df,
     c("svy_pop",
       "relative_distance",
       "reporting_pop",
       "surveyid_year",
       "mult_factor",
       "welfare_ppp",
       "weight",
       "reporting_level")) <- NULL

  df

}






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

