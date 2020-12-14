#' Load PIP microdata in different way
#'
#' @inheritParams pip_find_data
#' @param type character: Either `dataframe` or `list`. Defaults is `dataframe`.
#' @param survey_id character: Vector with survey IDs like
#' 'HND_2017_EPHPM_V01_M_V01_A_PIP_PC-GPWG'
#' @param noisy logical: if FALSE, no loading messages will be displayed.
#' Default is TRUE.
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
#' # Use condition argument
#' pip_find_data(condition = "country_code %chin% c('PRY', 'KGZ') &
#'                             year >= 2012 & year < 2014")
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
                          noisy            = TRUE
                          ) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Find Data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(survey_id)) {

    #--------- Get full path ---------
    # Raw inventory
    ri <- pip_load_inventory(maindir = maindir)
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
                        condition        = condition     ,
                        maindir          = maindir)

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
    }

    # master version
    if (is.null(vermast)) {
      df[,
         maxmast := vermast == max(vermast),
         by = .(country_code, surveyid_year, survey_acronym, module)
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
         by = .(country_code, surveyid_year, survey_acronym, module)
          ][
            maxalt == 1
          ][,
            maxalt := NULL
          ]
    }

    # Select right module (source) if more than one available
    if (is.null(source) && toupper(tool) == "PC") {
      # Create grouping variable
      df[,
         survey_id := paste(country_code, surveyid_year, survey_acronym, vermast, veralt,
                            sep = "_")
      ]

      du <- df[, # get unique source by ID
               .(source = unique(source)),
               by = survey_id
                ][, # count sources by ID
                  n_source := .N,
                  by = survey_id
                ]

      #--------- Only if there are more than one source in at least one ID ---------
      if (du[, max(n_source)] > 1) {

      # those with only one source
        du1 <- du[n_source == 1
                  ][,
                    n_source := NULL
                  ]

        # treatment for those with more than one source
        du2 <- du[n_source > 1
        ][, # nest data by Survey ID. one dataframe for each ID with
          # several sources.
          .(data = .nest(source)),
          by = survey_id

        ][, # Keep one source per data using rule in `keep_source()`
          filtered := purrr::map(data, ~sf_keep_source(df = .x))
        ]

        if (inherits(du2$filtered, "list")) {

          du2 <- du2[, # Unnest data again so we get one source per ID
                     .(source = .unnest(filtered)),
                     by = survey_id
          ]
        } else {
          du2[,
              source := filtered
          ][,
            c("data", "filtered") := NULL
          ]
        }

        # Append both sources in one
        dun <- data.table::rbindlist(list(du1, du2),
                                     use.names = TRUE,
                                     fill      = TRUE)

        # Filter df with only the value in dun
        df <- df[dun,
                 on = .(survey_id,source)]
      }
    } # end of source == NULL

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

  if (!is.null(dt_errors)) {
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

#--------- Keep source data ---------

#' Make sure we keep only one module per survey ID when loading.
#'
#' @param df dataframe
#'
#' @return dataframe
keep_source <- function(df){

  source_order <- c("GPWG", "HIST", "BIN", "GROUP", "synth")
  source_avail <- df[, unique(source)]

  out         <- FALSE
  i           <- 0
  maxi        <- length(source_order)
  source_keep <- NULL
  while(out == FALSE && i <= maxi) {

    i <- i + 1
    if (source_order[i] %in% source_avail) {
      source_keep <- source_order[i]
      out         <- TRUE
    }

  }

  if (!is.null(source_keep)) {
    df <- df[source == (source_keep)]
  }
  return(df)
}


# make sure function runs fine
sf_keep_source <- purrr::possibly(keep_source,
                                  otherwise = NULL)

