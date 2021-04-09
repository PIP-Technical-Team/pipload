#' Load cache data
#'
#' @inheritParams pip_find_data
#' @inheritParams pip_load_data
#'
#' @param source
#' @param welfare_type
#' @param data_level
#' @param cache_id
#' @param pipedir
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
pip_load_cache <- function(country          = NULL,
                           year             = NULL,
                           survey_acronym   = NULL,
                           data_level       = NULL,
                           welfare_type     = NULL,
                           source           = NULL,
                           tool             = c("PC", "TM"),
                           cache_id         = NULL,
                           condition        = NULL,
                           type             = "dataframe",
                           pipedir          = getOption("pip.pipedir"),
                           verbose          = TRUE,
                           inv_file         = paste0(maindir,
                                                     "_inventory/inventory.fst"),
                           filter_to_pc = FALSE,
                           filter_to_tm = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # right arguments
  tool <- match.arg(tool)
  if (!is.null(welfare_type)) {
    wt_ok <- any(toupper(welfare_type) %in% c("CON", "INC"))

    if (isFALSE(wt_ok)) {
      if (verbose) {
        cli::cli_alert_danger("{.code welfare_type} must be either {.field CON} or
                              {.field INC}, not {.field {welfare_type}}", wrap = TRUE)
      }
      msg     <- "wrong specification in `welfare_type`"
      rlang::abort(c(msg),class = "pipload_error")
    }

  }

  if (tool == "PC") {
    cache_dir <- paste0(pipedir, "pc_data/cache/clean_survey_data/")
  } else {
    cli::cli_alert("There is no cache files yet for table maker. Return NULL")
    return(NULL)
  }






  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   If Cache_id is selected   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(cache_id)) {

  ### Load cache inventory ---------

  ri <- pip_load_cache_inventory(pipedir = pipedir,
                                 tool    = tool)
    #--------- Get full path ---------

    si <- data.table::data.table(cache_id = cache_id)

    df <- ri[si,
             on = .(cache_id)
             ][,
               cache_id]

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #             If parameters are selected   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##           find data --------
    df <- pip_find_cache(country          = country,
                         year             = year,
                         survey_acronym   = survey_acronym,
                         data_level       = data_level,
                         welfare_type     = welfare_type,
                         source           = source,
                         tool             = tool,
                         pipedir          = pipedir)

    ldf <- length(df)
    if (verbose) {
      if (ldf == 0) {
        cli::cli_alert_danger("No table matched the pattern. Return NULL")
      } else if (ldf > 0 && ldf <= 5) {
        cli::cli_alert_info("The following {ldf} table{?s} matched the pattern")
        cli::cli_ul(df)
      } else {
        cli::cli_alert_info("More than 5 tables matched the pattern")
      }
    } # end of verbose

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (ldf == 0) {
    return(NULL)
  }

  ps_load_chache <- purrr::possibly(.f = load_chache,
                                     otherwise = NULL)

  tryCatch({

    if (verbose) { # if user wants spinner

      cli::cli_process_start("Loading data and creating a {.field {type}}")
      dt <- purrr::map(.x      = paste0(cache_dir, df, ".fst"),
                       .f      = ps_load_chache,
                       verbose = verbose)
      sp$finish()
      cli::cli_process_done()

    } else { # if user does not want spinner... pipeline purposes
      dt <- purrr::map(.x      = paste0(cache_dir, df, ".fst"),
                       .f      = ps_load_chache,
                       verbose = verbose)
    }

    names(dt) <- df

  },
  error = function(err) {

    cli::cli_process_failed()
    cli::cli_alert_danger("Failed loading data")

  }
  ) # end of tryCatch

  return(dt)
}


#' Load cache data
#'
#' @param x
#' @param verbose
#'
#' @return data.table
#' @noRd
#' @keywords internal
load_chache <- function(x, verbose) {
  if (verbose) {
    sp$spin()
  }
  df <- fst::read_fst(x, as.data.table = TRUE)
  return(df)
}


# Make spinner
sp <- cli::make_spinner("dots", template = "Loading data {spin}")

