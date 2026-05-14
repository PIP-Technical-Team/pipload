#' Assign PIP class to object
#'
#' @param x dataframe or list. If dataframe, the number of unique elements in the variable `module` should be 1
#'
#' @return data.frame or list
#' @export
as_pip <- function(x) {
  UseMethod("as_pip")
}


#' @export
#' @rdname as_pip
as_pip.data.frame <- function(x) {
  assign_pipclass(x)
}

#' @export
#' @rdname as_pip
as_pip.list <- function(x) {
  lapply(x, assign_pipclass)
}


#' assign correct class to DLW data
#'
#' @param df dataframe loaded from DLW flat structure
#'
#' @return data.table
#' @export
assign_pipclass <- function(df) {

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  stopifnot(exprs = {
    is.data.frame(df)
    "module" %in% names(df)
  }
  )


  # Early returns ------
  module <- unique(df$module)

  if (length(module) > 1) {
    cli::cli_alert_info("More than one module in dataframe ({.field {module}}).
                        {cli::col_blue('return the same dataframe')}")
    return(df)
  }


  if ("sim" %in% names(df)) {
    df <- as_pipid(df)
  } else if (grepl("GROUP", module)) {
    df <- as_pipgd(df)
  } else {
    df <- as_pipmd(df)
  }

  return(df)

}


#' Add pipmd class for microdata
#'
#' @param x data frame
#'
#' @return data frame with new class pipmd
#' @export
as_pipmd <- function(x) {
  stopifnot(is.data.frame(x))

  if (!(inherits(x, "data.table"))) {
    x <- as.data.table(x)
  }

  data.table::setattr(x, "class", c("pipmd", class(x)))
  return(invisible(x))

}

#' Add pipgd class for group data
#'
#' @param x data frame
#'
#' @return data frame with new class pipgd
#' @export
as_pipgd <- function(x) {
  stopifnot(is.data.frame(x))

  if (!(inherits(x, "data.table"))) {
    x <- as.data.table(x)
  }

  data.table::setattr(x, "class", c("pipgd", class(x)))
  return(invisible(x))

}

#' Add pipid class for imputed data
#'
#' @param x data frame
#'
#' @return data frame with new class pipid
#' @export
as_pipid <- function(x) {
  stopifnot(is.data.frame(x))

  if (!(inherits(x, "data.table"))) {
    x <- as.data.table(x)
  }

  data.table::setattr(x, "class", c("pipid", "pipmd", class(x)))
  return(invisible(x))

}


pipmd_class <- c("pipmd", "data.table", "data.frame")
pipgd_class <- c("pipgd", "data.table", "data.frame")
pipid_class <- c("pipid", "pipmd", "data.table", "data.frame")


#' Assign pip S3 class from pip_id suffix (new-pipeline data)
#'
#' Internal helper for data loaded from the new pipeline that lacks a `module`
#' column. Dispatches to [as_pipid()], [as_pipgd()], or [as_pipmd()] based on
#' the suffix of \code{pip_id} and presence of a `sim` column.
#'
#' @param survey data.table. Survey data without a `module` column.
#' @param pip_id character(1). Survey identifier (e.g. `"BOL_2022_EH_INC_ALL"`).
#'
#' @return The survey data.table with the correct pip S3 class.
#' @keywords internal
assign_pipclass_from_id <- function(survey, pip_id) {
  known_modules <- c("ALL", "GPWG", "HIST", "BIN", "GROUP", "SYNTH")
  pip_module <- sub(".*_", "", pip_id)
  if (!toupper(pip_module) %in% known_modules) {
    cli::cli_warn(
      c(
        "Unrecognised module token {.val {pip_module}} in pip_id {.val {pip_id}}.",
        "i" = "Expected one of: {.val {known_modules}}.",
        "i" = "Defaulting to {.cls pipmd} class."
      )
    )
  }
  if ("sim" %in% names(survey)) {
    return(as_pipid(survey))
  } else if (grepl("GROUP", pip_module, ignore.case = TRUE)) {
    return(as_pipgd(survey))
  } else {
    return(as_pipmd(survey))
  }
}


