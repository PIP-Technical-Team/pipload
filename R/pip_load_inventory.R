#' Load inventory of welfare aggregate datasets
#'
#' @param inv_file character: file path to be loaded.
#' @param filter_to_pc logical: If TRUE filter most recent data to be included
#' in the Poverty Calculator. Default if FALSE
#' @param filter_to_tm logical: If TRUE filter most recent data to be included
#' in the Table Maker. Default if FALSE
#' @inheritParams pip_inventory
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_load_inventory <- function(maindir = getOption("pip.maindir"),
                               inv_file = paste0(maindir,
                                                 "_inventory/inventory.fst"),
                               inventory_version = NULL,
                               filter_to_pc = FALSE,
                               filter_to_tm = FALSE
                               ) {
  #--------- Conditions ---------
  if (filter_to_pc == TRUE && filter_to_tm == TRUE) {
    rlang::abort(c(
      "Syntax error",
      x = "`filter_to_pc` and `filter_to_tm` can't both be TRUE"
    ),
    class = "pipload_error"
    )
  }

  #--------- Load Data ---------

  if (file.exists(inv_file)) {
    df       <- fst::read_fst(inv_file)
    setDT(df)

    #--------- Filter Data to PC ingest ---------

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

    #--------- Filter Data to TM ingest ---------
    if (filter_to_pc == TRUE) {
      df <- df
    }

    return(df)

  } else {
    rlang::abort(c(
      paste("file", inv_file, "does not exist"),
      i = "Check your connection to the drives"
    ),
    class = "pipload_error"
    )
  }
}

# make sure function runs fine
sf_keep_pc_source <- purrr::possibly(pip_keep_pc_source,
                                     otherwise = NULL)

