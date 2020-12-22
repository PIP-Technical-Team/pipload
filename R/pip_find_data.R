#' pip_find_data
#' Find surveys available for PIP
#'
#' @param country character: vector of ISO3 country codes.
#' @param year    numeric: survey year
#' @param survey_acronym  character: Survey acronym
#' @param vermast character: Master version in the form v## or ##
#' @param veralt  character: Alternative version in the form v## or ##
#' @param module  character: combination of `tool` and `source` separated by
#' a hyphen (e.g., PC-GPWG)
#' @param tool    character: PIP tool in which data will be used. It could be
#' `PC` for Poverty Calculator or `TB` for Table Maker. Others will be added
#' @param source  character: Source of data. It could be `GPWG`, `HIST`, `GROUP`,
#' `synth`, `BIN`, and `ALL`. The latter is used only in Table Maker.
#' @param condition character: logical condition that applies to all surveys.
#' For example, "year > 2012". Make sure the condition uses the names of the
#' variables in `pip_load_inventory()`: orig, filename, country_code, year,
#' survey_acronym, vermast, veralt, collection, module, tool, and source.
#' Can't be used with arguments `country`, `year`,
#' `survey_acronym` , `vermast`, `veralt`, `module` or `tool`.
#' @param maindir character: Main directory
#' @param filter_to_pc logical: If TRUE filter most recent data to be included
#' in the Poverty Calculator. Default if FALSE
#' @param filter_to_tm logical: If TRUE filter most recent data to be included
#' in the Table Maker. Default if FALSE
#' @inheritParams pip_load_inventory
#'
#' @return data.frame: list of filenames to be loaded with pcn_load()
#' @import data.table
#' @export
#'
#' @examples
#' # all years for one country
#' pip_find_data(country = "ARG")
#'
#' #' # all years for more than one country
#' pip_find_data(country = c("COL", "ARG"))
#'
#' # specific years for one country
#' pip_find_data(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_find_data(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_find_data(country = "PRY",
#'              year = 2010,
#'              module = "PC-GPWG")
#'
#' # Load different sources
#' pip_find_data(country = "COL",
#'              source = "HIST")
#
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_find_data(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_find_data()
#' }

pip_find_data <- function(country         = NULL,
                         year             = NULL,
                         survey_acronym   = NULL,
                         vermast          = NULL,
                         veralt           = NULL,
                         module           = NULL,
                         tool             = NULL,
                         condition        = NULL,
                         source           = NULL,
                         maindir          = getOption("pip.maindir"),
                         inv_file         = paste0(maindir,
                                           "_inventory/inventory.fst"),
                         filter_to_pc = FALSE,
                         filter_to_tm = FALSE
                         ) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Initial conditions   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # drive and main dir

  if (!dir.exists(maindir)) {
    st_msg <- paste0("main directory `",
                     maindir,
                     "` not reachable.")
    rlang::abort(c(
                  st_msg,
                  i = "Check connection"
                  ),
                  class = "pipload_error"
                  )
  }

  #--------- Filter conditions ---------
  if (filter_to_pc == TRUE && filter_to_tm == TRUE) {
    rlang::abort(c(
      "Syntax error",
      x = "`filter_to_pc` and `filter_to_tm` can't both be TRUE"
    ),
    class = "pipload_error"
    )
  }


  #----------------------------------------------------------
  #   Create conditions
  #----------------------------------------------------------

  if (!is.null(condition)) { # if condition is used
    condit <- condition
    condi  <- parse(text = condition)

  } else { # if condition is not used

    if (is.null(country)) {
      argum <- c(year, survey_acronym, vermast, veralt)
      if (length(argum)) {
        rlang::inform(
          paste("if `country` is NULL, arguments `year`, `survey_acronym`\n",
                "`vermast`, and `veralt` should be NULL as well\n",
                "These arguments are coerced to NULL")
        )

      }

      # Load country names when no country is selected
      countries <- fs::dir_ls(maindir)
      countries <- gsub(maindir, "", countries)
      countries <- countries[!grepl("^_", countries)]  # remove _aux folder

      years      <- NULL

    } else { # country is defined

      country    <- toupper(country)
      lyear      <- length(year)
      lcountry   <- length(country)

      if ( lyear != 0 ) {       # if year is selected along with country

        if (lcountry != 1) {    # if more than one country selected

          if ( lyear != 1) {
            rlang::warn(c(
              paste0("Since `length(country)` is greater than 1,\n",
                     "the first value of `year` (",
                     year[[1]], ") wille be used"),
              i = paste("length(country) == " , lcountry)
            ),
            class = "pipload_warning"
            )
          }

          countries  <- country
          years      <- rep(year[[1]], lcountry)

        } else {              # if only one country selected

          countries <- rep(country, lyear)
          years     <- year

        }
      } else  { # if lyear == 0
        countries  <- country
        years      <- NULL
      }
    }   # end of country no NULL

    #--------- create conditions ---------
    # Country and year are special because the names are different
    condi <- "country_code %chin% (countries)"

    if (!(is.null(years))) {
      condi <- paste(condi, "& surveyid_year %chin% as.character(years)")
    }

    # The other arguments work fine. Just add "alt_" prefix
    argus <-
      c("survey_acronym",
        "vermast",
        "veralt",
        "module",
        "tool",
        "source")

    for (i in seq_along(argus)) {

      # if argument is NOT NULL
      y <- argus[i]
      if (!(is.null(get(y)))) {

        assign(paste0("alt_", y), toupper(get(y)))
        condi <- paste(condi, create_cond(y))

      }
    }

    #--------- load data ---------
    condit <- condi
    condi  <- parse(text = condi)

  } # end of condition == NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Load Inventory   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  df <- pip_load_inventory(maindir  = maindir,
                           inv_file = inv_file)
  df <- df[ eval(condi)]

  if (nrow(df) == 0) {
    cli::cli_alert_danger("The inventory has no data for this condition,
                          {.field {condit}}")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- Filter Data to PC ingest   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (filter_to_pc == TRUE) {

    df <-
      df[ # keep just the ones used in PC
        tool == "PC"
      ][,
        # Get max master version and filter
        maxmast := vermast == max(vermast),
        by = .(country_code, surveyid_year, survey_acronym, module)
      ][
        maxmast == 1
      ][,
        # Get max veralt version and filter
        maxalt := veralt == max(veralt),
        by = .(country_code, surveyid_year, survey_acronym, module)
      ][,
        c("maxalt",  "maxmast") := NULL
      ][,
        # Select right module (source) if more than one available
        # Create grouping variable
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

    # those with only one source
    du1 <-
      du[n_source == 1
      ][,
        n_source := NULL
      ]

    # treatment for those with more than one source
    du2 <-
      du[n_source > 1
      ][, # nest data by Survey ID. one dataframe for each ID with
        # several sources.
        .(data = .nest(source)),
        by = survey_id

      ][, # Keep one source per data using rule in `keep_source()`
        filtered := purrr::map(data, ~sf_keep_pc_source(df = .x))
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


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- Filter Data to TM ingest   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (filter_to_pc == TRUE) {
    df <- df
  }

  return(df)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CReate conditions for filtering
#'
#' @param x argument parsed through `pip_find_data`
#'
#' @return character
#'
#' @examples
create_cond <- function(x) {
  cd <- paste0("& toupper(", x, ") %chin% (alt_", x, ")")
  return(cd)
}


#' Make sure we keep only one module per survey ID when loading.
#'
#' @param df dataframe from `pip_load_inventory()`
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
pip_keep_pc_source <- function(df){

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
sf_keep_pc_source <- purrr::possibly(pip_keep_pc_source,
                                     otherwise = NULL)

