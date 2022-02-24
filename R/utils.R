add_and <- function(x) {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
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


list_of_countries <- function(root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                              maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR) {
  cli::cli_progress_step("getting list of countries")
  countries <- fs::dir_ls(path    = maindir,
                          recurse = FALSE,
                          regexp = "[A-Z]{3}$",
                          type    = "directory"
                          )

  cli::cli_progress_done()

  countries <- as.character(countries)
  country_list <- stringr::str_extract(countries, "[A-Z]{3}$")

  return(country_list)
}



# Nesting and unnesting data.tables
.nest   <- function(...) {
  list(data.table::data.table(...))
}

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
