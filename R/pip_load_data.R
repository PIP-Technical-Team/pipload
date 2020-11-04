#' Load PIP microdata in different way
#'
#' @inheritParams pip_find_data
#' @param type character: Either `dataframe` or `list`. Defaults is `dataframe`.
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
#' pip_load_data(country = "PRY",
#'               year    = 2017)
#'
#' # specific years for one country
#' pip_load_data(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_load_data(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_load_data(country = "PRY",
#'              year = 2010,
#'              module = "PC-GPWG")
#'
#' # Load different sources
#' pip_load_data(country = "COL",
#'              source = "HIST")
#'
#' # Load using Survey ID
#' pip_load_data(survey_id = c("HND_2017_EPHPM_V01_M_V01_A_PIP_PC-GPWG",
#'                             "HND_2018_EPHPM_V01_M_V01_A_PIP_PC-GPWG")
#'                             )
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
                          tool             = NULL,
                          source           = NULL,
                          survey_id        = NULL,
                          type             = "dataframe",
                          maindir          = getOption("pip.maindir")
                          ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Find Data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(survey_id)) {

    #--------- Get full path ---------
    # Raw inventory
    ri <- pip_load_inventory()
    setDT(ri)
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
                        maindir          = maindir)

    #--------- Filter most recent version ---------
    # master version
    if (is.null(vermast)) {
      df[,
         maxmast := vermast == max(vermast),
         by = .(country_code, year, survey_acronym, module)
      ][
        maxmast == 1
      ][,
        maxmast := NULL
      ]
    }

    # Alternative version
    if (is.null(veralt)) {
      df[,
         maxalt := veralt == max(veralt),
         by = .(country_code, year, survey_acronym, module)
      ][
        maxalt == 1
      ][,
        maxalt := NULL
      ]
    }

  } # end of creation of df with survey names and IDs


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  poss_data_to_df <- purrr::possibly(.f = data_to_df,
                                     otherwise = NULL)

  # Make sure all data that is possible to load is loaded
  tryCatch({

    cli::cli_process_start("Loading data and creating a {.field {type}}")
    dt <- purrr::map2(.x = df$orig,
                      .y = df$filename,
                      .f = poss_data_to_df)
    sp$finish()
    cli::cli_process_done()

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

  if (!is.null(dt_errors)) {

    cli::cli_rule(center = "{length(dt_errors)} dataset{?s} could not be loaded")
    cli::cli_text("{.file {dt_errors}}")
    cli::cli_rule(right = "end")

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
                        use.names	= TRUE,
                        idcol     = TRUE)
      }, # end of expr section

      error = function(e) {
        cli::cli_alert_danger("Could not create data frame")
        cli::cli_alert_danger("{e$message}")
        cli::cli_alert_info("returning object is a list instead")
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

    return(dt)

} # end of pip_load_data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data_to_df <- function(x, y) {
  sp$spin()
  df <- haven::read_dta(x)

  #--------- leaving just the 'label' attribute ---------
  nn  <- names(df)
  for (x in seq_along(nn)) {

    ats       <- attributes(df[[x]])
    atsn      <- names(ats)
    to_remove <- atsn[!grepl("label", atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[x]], to_remove[i]) <- NULL
    }

  }

  #--------- Survey ID and its components ---------

  y  <- gsub("\\.dta", "", y)
  df$survey_id <- y
  setDT(df)

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
