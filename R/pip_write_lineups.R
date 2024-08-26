#' Function to minimize and save reference year distribution for one
#' country and one reference year
#'
#' @param df_refy output from [refy_distributions]
#' @param path path to save the output - "P:\03.pip\lineup_distributions\output-lineup-ref-years"
#'
#' @return invisible logical - purpose is to save file
#' @keywords internal
write_refy_dist <- function(df_refy,
                            path = Sys.getenv("PIP_LINEUPS_DIR"),
                            inc_svy_year = FALSE) {

  cntry_code <- attributes(df_refy)$country_code
  ref_year   <- attributes(df_refy)$reporting_year

  # save
  qs::qsave(x = df_refy,
            file = fs::path(path,
                            paste0(cntry_code,
                                   "_",
                                   ref_year,
                                   ".qs")))

  invisible(TRUE)
}

