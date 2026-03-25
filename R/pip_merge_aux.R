#' Merge auxiliary PIP tables
#'
#' @param tables character: name of auxiliary tables available in
#'   `pip_load_aux()`
#' @param ppp_year numeric: PPP round year
#' @inheritParams pip_load_aux
#' @param ... Other arguments passed on to [base::merge()]. Yet, it actually
#'   uses the merge S3 method for data.table
#'
#' @return data.table
#' @export
#'
#' @examples
#' pip_merge_aux()
pip_merge_aux <- function(tables            = c("cpi", "ppp"),
                          ppp_year          = 2017,
                          branch            = c("DEV", "PROD", "main"),
                          root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                          maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                          ...) {

  branch <- match.arg(branch)


  #   ____________________________________________________
  #   on.exit                                         ####
  on.exit({

  })

  #   ____________________________________________________
  #   Defenses                                        ####
  if (length(tables) < 2) {
    cli::cli_abort("Parameter {.field tables} must be two or more elements.")
  }

  #   ____________________________________________________
  #   Early returns                                   ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________
  #   Computations                                     ####

  # load Id
  lid <- aux_ids(tables = tables)

  # load aux tables
  laux <-
    purrr::map(tables,
               pip_load_aux,
               branch            = branch,
               root_dir          = root_dir,
               maindir           = maindir,
               verbose           = FALSE) |>
    # rename all xxx_data_level variables to data_level
    purrr::map(rename_data_level_var, verbose = FALSE)
  names(laux) <- tables

  # fiter PPP based on ppp year
  if ("ppp" %in% names(laux)) {
    py <- ppp_year
    laux$ppp <- laux$ppp[ppp_year == py & ppp_default_by_year  == TRUE]
  }

  # filter CPI based on PPP year
  if ("cpi" %in% names(laux)) {
    cpi_var <- paste0("cpi", ppp_year)
    laux$cpi[, cpi := get(cpi_var)]
  }


  # Create by variables
  lby_vars <- uni_int(lid)

  # Merge aux tables
  dt    <- purrr::reduce2(laux, lby_vars, merge)
  dt_id <- purrr::reduce(lid, union)
  if (anyDuplicated(dt, by = dt_id) == 0) {
    attr(dt, "id")  <- dt_id
  } else {
    attr(dt, "id")  <- NA
  }

  #   ____________________________________________________
  #   Return                                           ####
  return(dt)

}


#' list of ID variables in auxiliary tables
#'
#' @inheritParams pip_merge_aux
#'
#' @return list of ids per table
#' @export
#'
#' @examples
#' aux_ids()
aux_ids <- function(tables = NULL) {

  l <- list()

  l[["cpi"]] <- c("country_code",
                  "survey_year",
                  "cpi_year",
                  "data_level",
                  "survey_acronym")

  l[["pfw"]] <- c("country_code",
                  "year",
                  "reporting_year",
                  "surveyid_year",
                  "survey_year",
                  "welfare_type",
                  "survey_acronym")

  l[["ppp"]]  <- c("country_code", "data_level")

  l[["gdm"]] <- c("country_code",
                  "data_level",
                  "survey_year",
                  "welfare_type",
                  "surveyid_year")


  if (!is.null(tables)) {
    miss_tbl <- tables[!tables %in% names(l)]
    if (length(miss_tbl)) {
      cli::cli_abort("{.field {miss_tbl}} {?is/are} not part of PIP auxiliary table")
    }

    l <- l[tables]
  }



  #   ____________________________________________________
  #   Return                                           ####
  return(l)

}


uni_int <- function(l) {

  #   ____________________________________________________
  #   on.exit                                         ####
  on.exit({

  })

  #   ____________________________________________________
  #   Defenses                                        ####
  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________
  #   Early returns                                   ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________
  #   Computations                                     ####

  x    <- NULL
  lres <- vector("list", length = length(l) - 1)
  for(i in seq_along(lres)) {
    y <- l[[i]]
    z <- l[[i + 1]]

    lres[[i]] <- union(x, y) |>
      intersect(z)
    x <- y
  }

  #   ____________________________________________________
  #   Return                                           ####
  return(lres)

}


rename_data_level_var <- function(dt,
                                  verbose = TRUE) {

  #   ____________________________________________________
  #   Defenses                                        ####
  stopifnot( exprs = {
    is.data.frame(dt)
  }
  )

  if (is.data.table(dt)) {
    df <- copy(dt)
  } else {
    df <- as.data.table(dt)
  }

  vars <- names(df)

  dl_var <- grep("data_level", vars, value = TRUE)


  #   ____________________________________________________
  #   Early returns                                   ####
  ldl <- length(dl_var)
  if (ldl == 0) {
    if (verbose)
      cli::cli_alert_warning("no {.code data_level} variable found")

    return(dt)
  } else if (ldl > 1) {
    if (verbose)
      cli::cli_alert_warning("Found {ldl} {.code data_level} variable{?s}: {.field {dl_var}}.
                           Rename only works when one variable is found")
    return(dt)
  }

  #   ____________________________________________________
  #   Computations                                     ####
  setnames(df, dl_var, "data_level")
  df[, data_level := tolower(data_level)]


  #   ____________________________________________________
  #   Return                                           ####
  return(df)

}
