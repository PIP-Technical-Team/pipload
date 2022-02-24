#' Loads datalibweb data
#'
#' @inheritParams pip_find_data
#' @inheritParams pip_load_aux
#' @inheritParams pip_load_dlw_inventory
#' @param type character: Either `dataframe` or `list`. Defaults is `dataframe`.
#' @param survey_id character: Vector with survey IDs like
#' 'HND_2017_EPHPM_V01_M_V01_A_PIP_PC-GPWG'
#'
#' @return data.table
#' @export
#' @import data.table
#'
#' @examples
#' # ONe year and one country
#' df <- pip_load_dlw(country = "PRY",
#'               year    = 2017)
#'
#' # specific years for one country
#' df <- pip_load_dlw(
#'             country = "PRY",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' df <- pip_load_dlw(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' df <- pip_load_dlw(country = "PRY",
#'              year = 2010,
#'              module = "GPWG")
#'
#' # Load using Survey ID
#' df <- pip_load_dlw(survey_id = c("HND_2017_EPHPM_V01_M_V02_A_GMD_GPWG",
#'                             "HND_2018_EPHPM_V01_M_V02_A_GMD_GPWG")
#'                             )
#' # Use condition argument
#' df <- pip_load_dlw(condition = "country_code %chin% c('PRY', 'KGZ') &
#'                     (surveyid_year >= 2012 & surveyid_year < 2014)")
#'
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_load_dlw(
#'        country = c("PRY", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_load_dlw()
#' }
pip_load_dlw <- function(country          = NULL,
                          year            = NULL,
                          survey_acronym  = NULL,
                          vermast         = NULL,
                          veralt          = NULL,
                          module          = NULL,
                          tool            = c("PC", "TB"),
                          survey_id       = NULL,
                          condition       = NULL,
                          type            = "dataframe",
                          root_dir        = Sys.getenv("PIP_ROOT_DIR"),
                          dlw_dir         = pip_create_globals(root_dir)$DLW_RAW_DIR,
                          filter_to_pc    = TRUE,
                          filter_to_tb    = FALSE,
                          verbose         = getOption("pipload.verbose")
) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Find Data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tool <- match.arg(tool)
  if (!is.null(survey_id)) {

    #--------- Get full path ---------
    # Raw inventory
    ri <- pip_load_dlw_inventory(root_dir = root_dir,
                                 dlw_dir  = dlw_dir)

    si <- data.table::data.table(survey_id = survey_id)

    df <- ri[si,
             on = .(survey_id)]

  } else { # if country and year are provided


    # Call data find to get inventory
    df <- pip_find_dlw(country          = country       ,
                       year             = year          ,
                       survey_acronym   = survey_acronym,
                       vermast          = vermast       ,
                       veralt           = veralt        ,
                       module           = module        ,
                       tool             = tool          ,
                       condition        = condition     ,
                       root_dir         = root_dir      ,
                       dlw_dir          = dlw_dir       ,
                       filter_to_pc     = filter_to_pc,
                       filter_to_tb     = filter_to_tb)

    #--------- Filter most recent version ---------

    # Tool
    if (!is.null(tool) & !is.null(condition)) {
      if (grepl("tool", condition)) {
        if (verbose)
          cli::cli_alert_warning("`tool` argument was specified as {.val {tool}},
                                 but it was
                                 also mentioned in the `condition` argument.
                                 The latter will take predominance.",
                                 wrap = TRUE)
      } else {
        alt_tool <- tool
        df <- df[tool == (alt_tool)]
      }
    } # end of confliciting arguments
  } # end of creation of df with survey names and IDs


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  poss_data_to_df <- purrr::possibly(.f = data_to_dt,
                                     otherwise = NULL)

  # Make sure all data that is possible to load is loaded
  tryCatch({

    if (verbose) { # if user wants spinner

      cli::cli_process_start("Loading data and creating a {.field {type}}")
      dt <- purrr::map2(.x = df$fullname,
                        .y = df$survey_id,
                        .f = poss_data_to_df,
                        verbose = verbose)
      sp$finish()
      cli::cli_process_done()

    } else { # if user does not want spinner... pipeline purposes
      dt <- purrr::map2(.x = df$fullname,
                        .y = df$survey_id,
                        .f = poss_data_to_df,
                        verbose = verbose)
    }

    names(dt) <- df[, unique(survey_id)]
    dt

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
    usethis::ui_warn("{length(dt_errors)} {usethis::ui_field('survey_id')}(s) could not be loaded")
    cli::cli_ul(dt_errors)
  }

  # If type is dataframe
  if (type == "dataframe") {
    #--------- getting rid of errors and create dataframe ---------
    dt <- purrr::compact(dt)

    # create data frame. If failed, list is returned.
    tryCatch(
      expr = {
        dt <- rbindlist(dt,
                        fill      = TRUE,
                        use.names	= TRUE)
        # add class
        dt <- assign_pipclass(dt)


      }, # end of expr section

      error = function(e) {
        cli::cli_alert_danger("Could not create data frame")
        cli::cli_alert_danger("{e$message}")
        cli::cli_alert_info("returning a list instead")
        y  <- gsub("\\.dta", "", unique(df$filename))
        names(dt) <- y
      } # end of finally section

    ) # End of trycatch

  } else if (type == "list") { # if output type if list

    y  <- gsub("\\.dta", "", unique(df$filename))
    names(dt) <- y

  } else {
    rlang::abort(c(
      "The `type` selected is not a valid name",
      i = "you can use `dataframe` or `list`", # update this message automatically
      x = paste("you specified", type)
    ),
    class = "pipload_error"
    )
  }

  # return NULL instead of an empty dataset.
  if (nrow(dt) == 0) {
    dt <- NULL
  }

  return(dt)

} # end of pip_load_dlw

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Convert origianl dta files to data frame format suitable for PIP
#'
#' @param x character: file path
#' @param y chradter: file name including format
#' @param verbose logical: If TRUE, messages will display in console.
#'
#' @return data.table
data_to_dt <- function(x, y, verbose) {
  if (verbose) {
    sp$spin()
  }
  df <- haven::read_dta(x)

  #--------- leaving just the 'label' attribute ---------
  nn  <- names(df)
  for (j in seq_along(nn)) {

    ats       <- attributes(df[[j]])
    atsn      <- names(ats)
    to_remove <- atsn[!grepl("label", atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[j]], to_remove[i]) <- NULL
    }

  }

  #--------- Survey ID and its components ---------

  df$survey_id <- y
  df <- survey_id_to_vars(df)

  ### Add class ---------
  df <- assign_pipclass(df)



  return(df)
}

# Make spinner

sp <- cli::make_spinner("dots", template = "Loading data {spin}")


pip_modules <-
  c("GPWG",
    "ALL",
    "BIN",
    "GROUP",
    "HIST")
