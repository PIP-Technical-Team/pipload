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
  df <- assign_pipclass(x)
}

#' @export
#' @rdname as_pip
as_pip.list <- function(x) {
  df <- lapply(x, assign_pipclass)
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
  } else if (module == "GROUP") {
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

  structure(x,
            class = c("pipmd", class(x))
  )

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

  structure(x,
            class = c("pipgd", class(x))
            )

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

  structure(x,
            class = c("pipid","pipmd", class(x))
  )

}


pipmd_class <- c("pipmd", "data.table", "data.frame")
pipgd_class <- c("pipgd", "data.table", "data.frame")
pipid_class <- c("pipid", "pipmd", "data.table", "data.frame")


