#' Load cache data
#'
#' @inheritParams pip_find_data
#' @inheritParams pip_load_data
#'
#' @param welfare_type character: Either "CON" for consumption or "INC" for income
#' @param data_level character: either "D1" for national, "D2" for urban/rural,
#'  "D3" for subnational
#' @param cache_id character: If user knows the precise cache ID
#' @param pipedir charater: directory of pipeline. Default is `getOption("pip.pipedir")`
#' @param verbose logical: If TRUE, display informative messages. Default TRUE
#'
#' @return
#' @export
#'
#' @examples
pip_load_cache <- function(country          = NULL,
                           year             = NULL,
                           tool             = c("PC", "TB"),
                           survey_acronym   = NULL,
                           data_level       = NULL,
                           welfare_type     = NULL,
                           source           = NULL,
                           cache_id         = NULL,
                           condition        = NULL,
                           type             = c("dataframe", "list"),
                           pipedir          = getOption("pip.pipedir"),
                           verbose          = TRUE,
                           inv_file         = paste0(maindir,
                                                     "_inventory/inventory.fst")
                           ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # right arguments
  type <- match.arg(type)
  tool <- match.arg(tool)


  # Correct tool

  if (!is.null(cache_id)) {

    if (all(grepl("ALL$", cache_id))) {

      tool <- "TB"

    } else if (all(!grepl("ALL$", cache_id))) {

      tool <- "PC"

    } else {
      msg     <- "you can't combine cache_id from tool 'PC' with those of tool 'TB'"
      rlang::abort(c(msg), class = "error_class")
    }

  }


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
    cache_dir <- paste0(pipedir, "tb_data/cache/clean_survey_data/")
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
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  #--------- data with problems ---------
  dt_errors = dt %>%
    purrr::keep(~is.null(.x) ) %>%
    names()

  if (length(dt_errors) > 0) {
    cli::cli_alert_warning("{length(dt_errors)} survey{?s} could not be loaded")
    cli::cli_ul(dt_errors)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert depending on type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (type == "dataframe") {
    #--------- getting rid of errors and create dataframe ---------
    dt <- purrr::compact(dt)

    # create data frame. If failed, list is returned.
    dt <- tryCatch(
      expr = {
        dt <- rbindlist(dt,
                        fill      = TRUE,
                        use.names	= TRUE)

        dist_type <- unique(dt$distribution_type)

        if (dist_type == 'micro') {

          dt <- as_pipmd(dt)

        } else if (dist_type == 'group') {

          dt <- as_pipgd(dt)

        } else if (dist_type == 'aggregate') {

          dt <- as_pipgd(dt)

        } else if (dist_type == 'imputed') {

          dt <- as_pipid(dt)

        } else {
          stop("`dist_type` not valid")
        }

      }, # end of expr section

      error = function(e) {
        cli::cli_alert_danger("Could not create data frame")
        cli::cli_alert_danger("{e$message}")
        cli::cli_alert_info("returning a list instead")
        dt
      } # end of finally section

    ) # End of trycatch

  }

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

  dist_type <- unique(df$distribution_type)

  if (dist_type == 'micro') {

    df <- as_pipmd(df)

  } else if (dist_type == 'group') {

    df <- as_pipgd(df)

  } else if (dist_type == 'aggregate') {

    df <- as_pipgd(df)

  } else if (dist_type == 'imputed') {

    df <- as_pipid(df)

  } else {
    stop("`dist_type` not valid")
  }

  return(df)
}


# Make spinner
sp <- cli::make_spinner("dots", template = "Loading data {spin}")


