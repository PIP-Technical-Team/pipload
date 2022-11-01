#' Add 'and' at the end of a character vector
#'
#' @param x character: Vector of object
#'
#' @return character vector
#' @noRd
#' @keywords internal
add_and <- function(x) {
  if (!(is.character(x))) {
    cli_alert("{.field x} must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], "and", x[2])
  }
  else {
    y <- c(x[1:lx-1], paste("and", x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}


#' List of countries avaialble in PIP_DATA_DIR
#'
#' @param root_dir character: root directory
#' @param maindir character: main directory
#'   `pip_create_globals(root_dir)$PIP_DATA_DIR`
#'
#' @return character vector with list of countries
#' @noRd
#' @keywords internal
list_of_countries <-
  function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
           maindir   = pip_create_globals(root_dir)$PIP_DATA_DIR
           ) {

  countries <- fs::dir_ls(path    = maindir,
                          recurse = FALSE,
                          regexp = "[A-Z]{3}$",
                          type    = "directory"
                          ) |>
    fs::path_file() |>
    sort()

  return(countries)
}



# Nest data.table
#'
#' @param ... variable names in data.table
#'
#' @return nested variable within data.table
#' @noRd
#' @keywords internal
.nest   <- function(...) {
  list(data.table::data.table(...))
}

#' Unnest data.table
#'
#' @param ... variable names in data.table
#'
#' @return data.table with unnested variable
#' @noRd
#' @keywords internal
.unnest <- function(...) {
    unlist(data.table::data.table(...))
}

#' take variable `survey_id` and split it into different vars
#'
#' @param dt dataframe with variable `survey_id` available.
#'
#' @return data.table
#' @export
survey_id_to_vars <- function(dt) {

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

  fnames <-
    c(
      "country_code",
      "surveyid_year",
      "survey_acronym",
      "vermast",
      "veralt",
      "collection",
      "module",
      "tool"
    )

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot(exprs = {
    is.data.frame(dt)
    "survey_id" %in% names(dt)
    }
  )

  # Early returns ------

  if (all(fnames %in% names(dt))) {
    cli::cli_alert_info("variables {.var {fnames}} are already in data frame.
                        {cli::col_blue('return the same dataframe')}")
    return(dt)
  }

  # Computations -------
  df <- data.table::as.data.table(dt)

  df[,
     # Name sections of surveyID into variables
     (cnames) := tstrsplit(survey_id, "_", fixed=TRUE)
  ][,

    # create tool and source
    tool := fifelse(module == "ALL", "TB", "PC")
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


  # Return -------------
  return(df)


}


#' Find last vintage of PROD directories
#'
#' @param tool_dir character: directory path of tool in use
#'
#' @return character with the most recent version of PROD data
#' @noRd
#' @keywords internal
last_prod_version <- function(tool_dir) {

#   ____________________________________________________________________________
#   on.exit                                                                 ####
  on.exit({

  })

#   ____________________________________________________________________________
#   Defenses                                                                ####
  stopifnot( exprs = {
    is.character(tool_dir)
    fs::dir_exists(tool_dir)
    }
  )

#   ____________________________________________________________________________
#   Early returns                                                           ####
  if (FALSE) {
    return()
  }

#   ____________________________________________________________________________
#   Computations                                                            ####
  versions <- fs::dir_ls(tool_dir) |>
    fs::path_file()

  # Get most recent production version
  version <-
    versions[grepl("PROD$", versions)] |>
    sort(decreasing = TRUE) |>
    {\(.) .[1]}()


#   ____________________________________________________________________________
#   Return                                                                  ####
  return(version)

}
