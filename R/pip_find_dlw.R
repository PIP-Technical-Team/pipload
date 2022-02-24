#' Find surveys available for DatalibWeb
#'
#' @param country character: vector of ISO3 country codes.
#' @param year    numeric: survey year
#' @param survey_acronym  character: Survey acronym
#' @param vermast character: Master version in the form v## or ##
#' @param veralt  character: Alternative version in the form v## or ##
#' @param module  character: Source of data. It could be `GPWG`, `HIST`, `GROUP`,
#' `synth`, `BIN`, and `ALL`. The latter is used only in Table Maker.
#' @param tool    character: PIP tool in which data will be used. It could be
#' `PC` for Poverty Calculator or `TB` for Table Maker. Others will be added
#' @param condition character: logical condition that applies to all surveys.
#' For example, "year > 2012". Make sure the condition uses the names of the
#' variables in `pip_load_dlw_inventory()`: orig, filename, country_code, year,
#' survey_acronym, vermast, veralt, collection, module, tool, and source.
#' Can't be used with arguments `country`, `year`,
#' `survey_acronym` , `vermast`, `veralt`, `module` or `tool`.
#' @param dlw_dir character: Main directory
#' @param filter_to_pc logical: If TRUE filter most recent data to be included
#' in the Poverty Calculator. Default if FALSE
#' @param filter_to_tb logical: If TRUE filter most recent data to be included
#' in the Table Maker. Default if FALSE
#' @inheritParams pip_load_dlw_inventory
#' @inheritParams pip_find_dlw
#' @inheritParams pip_load_aux
#'
#' @return data.frame: list of filenames to be loaded with pcn_load()
#' @import data.table
#' @export
#'
#' @examples
#' # all years for one country
#' pip_find_dlw(country = "ARG")
#'
#' #' # all years for more than one country
#' pip_find_dlw(country = c("COL", "ARG"))
#'
#' # specific years for one country
#' pip_find_dlw(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_find_dlw(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_find_dlw(country = "PRY",
#'              year = 2010,
#'              module = "GPWG",
#'              tool = "PC")
#'
#' # Load different modules
#' pip_find_dlw(country = "COL",
#'              module = "HIST")
#
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_find_dlw(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_find_dlw()
#' }
pip_find_dlw <- function(country         = NULL,
                          year           = NULL,
                          survey_acronym = NULL,
                          vermast        = NULL,
                          veralt         = NULL,
                          module         = NULL,
                          tool           = NULL,
                          condition      = NULL,
                          root_dir       = Sys.getenv("PIP_ROOT_DIR"),
                          dlw_dir        = pip_create_globals(root_dir)$DLW_RAW_DIR,
                          filter_to_pc   = FALSE,
                          filter_to_tb   = FALSE,
                          verbose        = getOption("pipload.verbose")
) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Initial conditions   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # drive and main dir

  if (!fs::dir_exists(dlw_dir)) {

    msg     <- c(
      "DLW directory {.file {dlw_dir}} could not be reached",
      "i" = "check connection."
      )
    cli::cli_abort(msg,
                  class = "pipload_error"
                  )
  }

  #--------- Filter conditions ---------
  if (filter_to_pc == TRUE && filter_to_tb == TRUE) {
    rlang::abort(c(
      "Syntax error",
      x = "`filter_to_pc` and `filter_to_tb` can't both be TRUE"
    ),
    class = "pipload_error"
    )
  }


  #----------------------------------------------------------
  #   Create conditions
  #----------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load DLW inventory --------
  df <- pip_load_dlw_inventory(dlw_dir = dlw_dir)


  if (!is.null(condition)) { # if condition is used
    condit <- condition
    condi  <- parse(text = condition)

  } else { # if condition is not used

    if (is.null(country)) {

      argum <- c(year, survey_acronym, vermast, veralt)
      if (length(argum)) {

        if (verbose) {
          cli::cli_alert_info("if `country` is NULL, arguments `year`,
                              `survey_acronym`, `vermast`, and `veralt` should
                              be NULL as well These arguments are coerced to
                              NULL",
                              wrap = TRUE)
        }

      }

      # Load country names when no country is selected
      countries <- df[, unique(country_code)]
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
        "tool")

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

  df <- df[ eval(condi)]

  if (nrow(df) == 0) {
    if (verbose)
      cli::cli_alert_danger("The inventory has no data for this condition,
                          {.field {condit}}",
                          wrap = TRUE)
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
      ][
        maxalt == 1
      ][,
        c("maxalt",  "maxmast") := NULL
      ][,
        # Select right module (source) if more than one available
        # Create grouping variable
        vintage_id := paste(country_code, surveyid_year, survey_acronym, vermast, veralt,
                           sep = "_")
      ]

    du <- df[, # get unique source by ID
             .(module = unique(module)),
             by = vintage_id
    ][, # count sources by ID
      n_module := .N,
      by = vintage_id
    ]

    #--------- Only if there are more than one source in at least one ID ---------

    # those with only one source
    du1 <-
      du[n_module == 1
      ][,
        n_module := NULL
      ]

    # treatment for those with more than one source
    du2 <-
      du[n_module > 1
      ][, # nest data by Survey ID. one dataframe for each ID with
        # several sources.
        .(data = .nest(module)),
        by = vintage_id

      ][, # Keep one source per data using rule in `keep_source()`
        filtered := purrr::map(data, ~sf_keep_pc_module(df = .x))
      ]

    if (nrow(du2) >= 1) {

      if (inherits(du2$filtered, "list")) {

        du2 <- du2[, # Unnest data again so we get one source per ID
                   .(module = .unnest(filtered)),
                   by = vintage_id
        ]
      } else {
        du2[,
            module := filtered
        ][,
          c("data", "filtered") := NULL
        ]
      }

      # Append both sources in one
      dun <- data.table::rbindlist(list(du1, du2),
                                   use.names = TRUE,
                                   fill      = TRUE)
    } else {
      dun <- data.table::copy(du1)
    }

    # Filter df with only the value in dun
    df <- joyn::merge(df, dun,
                      by      = c("vintage_id", "module"),
                      verbose = FALSE,
                      keep    = "inner")

    df[,
       vintage_id := NULL]

  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- Filter Data to TB ingest   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (filter_to_tb == TRUE) {
    df <-
      df[ # keep just the ones used in PC
        tool == "TB"
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
        # Create grouping variable
        survey_id := gsub("(.*)(\\.dta$)", "\\1", filename)
      ]
  }

  return(df)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' CReate conditions for filtering
#'
#' @param x argument parsed through `pip_find_dlw`
#'
#' @return character
create_cond <- function(x) {
  cd <- paste0("& toupper(", x, ") %chin% (alt_", x, ")")
  return(cd)
}


#' Make sure we keep only one module per survey ID when loading.
#'
#' @param df dataframe from `pip_load_dlw_inventory()`
#'
#' @return data.table
pip_keep_pc_module <- function(df){

  module_order <- c("GPWG", "HIST", "BIN", "GROUP", "synth")
  module_avail <- df[, unique(module)]

  out         <- FALSE
  i           <- 0
  maxi        <- length(module_order)
  module_keep <- NULL
  while(out == FALSE && i <= maxi) {

    i <- i + 1
    if (module_order[i] %in% module_avail) {
      module_keep <- module_order[i]
      out         <- TRUE
    }

  }

  if (!is.null(module_keep)) {
    df <- df[module == (module_keep)]
  }
  return(df)
}



# make sure function runs fine
sf_keep_pc_module <- purrr::possibly(pip_keep_pc_module,
                                     otherwise = NULL)

