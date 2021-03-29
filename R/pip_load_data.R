#' Load PIP microdata in different way
#'
#' @inheritParams pip_find_data
#' @param type character: Either `dataframe` or `list`. Defaults is `dataframe`.
#' @param noisy logical: if FALSE, no loading messages will be displayed.
#' Default is TRUE.
#' @param survey_id character: Vector with survey IDs like
#' 'HND_2017_EPHPM_V01_M_V01_A_PIP_PC-GPWG'
#'
#' @return
#' @export
#' @import data.table
#' @import cli
#' @importFrom magrittr %>%
#'
#' @examples
#' # ONe year and one country
#' df <- pip_load_data(country = "PRY",
#'               year    = 2017)
#'
#' # specific years for one country
#' df <- pip_load_data(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' df <- pip_load_data(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' df <- pip_load_data(country = "PRY",
#'              year = 2010,
#'              module = "PC-GPWG")
#'
#' # Load different sources
#' df <- pip_load_data(country = "COL",
#'              source = "HIST")
#'
#' # Load using Survey ID
#' df <- pip_load_data(survey_id = c("HND_2017_EPHPM_V01_M_V01_A_PIP_PC-GPWG",
#'                             "HND_2018_EPHPM_V01_M_V01_A_PIP_PC-GPWG")
#'                             )
#' # Use condition argument
#' df <- pip_find_data(condition = "country_code %chin% c('PRY', 'KGZ') &
#'                     (surveyid_year >= 2012 & surveyid_year < 2014)")
#'
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_load_data(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_load_data()
#' }
pip_load_data <- function(country          = NULL,
                          year             = NULL,
                          survey_acronym   = NULL,
                          vermast          = NULL,
                          veralt           = NULL,
                          module           = NULL,
                          tool             = "PC",
                          source           = NULL,
                          survey_id        = NULL,
                          condition        = NULL,
                          type             = "dataframe",
                          maindir          = getOption("pip.maindir"),
                          noisy            = TRUE,
                          inv_file         = paste0(maindir,
                                                  "_inventory/inventory.fst"),
                          filter_to_pc = FALSE,
                          filter_to_tm = FALSE
                          ) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Find Data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(survey_id)) {

    #--------- Get full path ---------
    # Raw inventory
    ri <- pip_load_inventory(maindir = maindir)

    ri[,
       survey_id := gsub("\\.dta", "", filename)]

    si <- data.table::data.table(survey_id = survey_id)

    df <- ri[si,
             on = .(survey_id)]

  } else { # if country and year are provided


  # Call data find to get inventory
    df <- pip_find_data(country          = country       ,
                        year             = year          ,
                        survey_acronym   = survey_acronym,
                        vermast          = vermast       ,
                        veralt           = veralt        ,
                        module           = module        ,
                        tool             = tool          ,
                        source           = source        ,
                        condition        = condition     ,
                        maindir          = maindir       ,
                        inv_file         = inv_file      ,
                        filter_to_pc     = filter_to_pc,
                        filter_to_tm     = filter_to_tm)

    #--------- Filter most recent version ---------

    # Tool
    if (!is.null(tool) & !is.null(condition)) {
      if (grepl("tool", condition)) {
        cli::cli_alert_warning(c("`tool` argument was specified ({.val {tool}}), but it was
                                 also mentioned in the `condition` argument.
                                 Thus, only the latter will take predominance."),
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

  poss_data_to_df <- purrr::possibly(.f = data_to_df,
                                     otherwise = NULL)

  # Make sure all data that is possible to load is loaded
  tryCatch({

    if (noisy) { # if user wants spinner

      cli::cli_process_start("Loading data and creating a {.field {type}}")
      dt <- purrr::map2(.x = df$orig,
                        .y = df$filename,
                        .f = poss_data_to_df,
                        noisy = noisy)
      sp$finish()
      cli::cli_process_done()

    } else { # if user does not want spinner... pipeline purposes
      dt <- purrr::map2(.x = df$orig,
                        .y = df$filename,
                        .f = poss_data_to_df,
                        noisy = noisy)
    }

    names(dt) <- survey_id

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

} # end of pip_load_data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Convert origianl dta files to data frame format suitable for PIP
#'
#' @param x character: file path
#' @param y chradter: file name including format
#' @param noisy logical: If TRUE, messages will display in console.
#'
#' @return
data_to_df <- function(x, y, noisy) {
  if (noisy) {
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

  y  <- gsub("\\.dta", "", y)
  df$survey_id <- y
  data.table::setDT(df)

  # create variables for merging
  cnames <-
    c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection",
      "module"
    )

  df[,
     # Name sections of surveyID into variables
     (cnames) := tstrsplit(survey_id, "_", fixed=TRUE)
  ][,

    # create tool and source
    c("tool", "source") := tstrsplit(module, "-", fixed = TRUE)
  ][,
    # change to lower case
    c("vermast", "veralt") := lapply(.SD, tolower),
    .SDcols = c("vermast", "veralt")
  ][,
    surveyid_year := as.numeric(surveyid_year)
  ][
    ,
    # Remove unnecessary variables
    c("M", "A") := NULL
  ]

  return(df)
}

# Make spinner

sp <- cli::make_spinner("dots", template = "Loading data {spin}")

