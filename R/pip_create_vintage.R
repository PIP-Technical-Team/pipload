#' Create output directory name
#'
#' This function creates the name of the directory that used as vintage control
#' of the PIP project.
#'
#' @param vintage list or character: If list, all objects should be named and
#'   only the following names are accepted: `c("release", "ppp_year", "ppp_rv",
#'   "ppp_av", "identity")`. Alternatively, it could a single-object list called
#'   "names" like `vintage = list(name = "some_name")`.  If character, it should
#'   be of length equal to 1.
#'
#' @param  DATE character: date in the form %y%m%d.
#'
#' @return character  in the form "%Y%m%d_YYYY_##_##_SSS"
#' @export
pip_create_vintage <- function(vintage = list(),
                               DATE = format(Sys.Date(), "%Y%m%d")) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot( exprs = {
    is.list(vintage) || is.character(vintage)
  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # defaults   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # requires sections
  rqr_sect  <- c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if it is a character vector   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.character(vintage)) {

    # vintage <- "20220315_2017_01_01_TEST"

    stopifnot({

      length(vintage) == 1
      grepl("\\d{8}_\\d{4}_\\d{1,2}_\\d{1,2}_[[:alpha:]]+", vintage)
    }
    )

    vintage        <- data.table::tstrsplit(vintage, "_")

    names(vintage) <- rqr_sect

  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check inputs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Check that correct names are used -------


  # user sections
  user_sect <- names(vintage)

  # section no available
  no_sect  <- user_sect[!(user_sect %in% c("name", rqr_sect))]

  # error handling
  if (length(no_sect) > 0) {
    msg     <- c(
      "names provided in {.field vintage} are not allowed",
      "*" = "names must be {.emph {rqr_sect}}",
      "x" = "name{?s} {.emph {no_sect}} not allowed"
    )
    cli::cli_abort(msg,
                   class = "pipload_error"
    )
  }

  if ("name"  %in% user_sect) {

    if (length(user_sect) > 1) {

      msg     <- c(
        "you must select either {.field name} or {.field {rqr_sect}}",
        "x" = "You selected {.emph {user_sect}}"
      )
      cli::cli_abort(msg,
                     class = "pipload_error"
      )
    }

    final_name <- vintage$name

  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Load PPP vintage complementary data --------
    ppp_v <- pip_load_aux("ppp", verbose = FALSE)

    # remove Vs in case they are available and add zeros
    ver_vars <- c("ppp_rv", "ppp_av")
    ppp_v[,
          (ver_vars) := lapply(.SD, function(x) {
            x <-  gsub("[Vv]", "", x)

            x <- fifelse(nchar(x) == 1, paste0("0",x), x)
          }),
          .SDcols = ver_vars]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Release --------

    if (is.null(vintage$release)) {
      vintage$release <- DATE

    } else {

      # match only numbers
      if (grepl("\\D+", vintage$release)) {
        msg     <- c(
          "Object {.field release} in parameter {.field vintage} must contain
             digits only",
          "x" = "you provided {.emph {vintage$release}}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )
      }

      # match only 8 characters
      if (nchar(vintage$release) != 8) {
        msg     <- c(
          "{.field release} number must be 8-digit long",
          "x" = "you provided {vintage$release},
            which is {nchar(vintage$release)}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## PPP year --------


    if (is.null(vintage$ppp_year)) {
      vintage$ppp_year <- as.character(ppp_v[, max(ppp_year)])
    } else {

      # convert to character
      if (is.numeric(vintage$ppp_year)) {
        vintage$ppp_year <- as.character(vintage$ppp_year)
      }

      # match only numbers
      if (grepl("\\D+", vintage$ppp_year)) {
        msg     <- c(
          "Object {.field ppp_year} in parameter {.field vintage} must contain
             digits only",
          "x" = "you provided {.emph {vintage$ppp_year}}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )
      }

      # match only 8 characters
      if (nchar(vintage$ppp_year) != 4) {
        msg     <- c(
          "{.field release} number must be 4-digit long",
          "x" = "you provided {vintage$ppp_year}, which is {nchar(vintage$ppp_year)}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

      # check it is a valid year
      if (!(vintage$ppp_year %in% ppp_v[, unique(ppp_year)])) {
        msg     <- c(
          "{.field ppp_year} select, {.emph {vintage$ppp_year}}, is not available",
          "i" = "{.field ppp_year} must be one of {ppp_v[, unique(ppp_year)]}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## release version of PPP --------

    if (is.null(vintage$ppp_rv)) {
      m_rv <- ppp_v[ppp_year == vintage$ppp_year,
                    max(ppp_rv)]

      vintage$ppp_rv <- m_rv
    } else {

      # convert to character
      if (is.numeric(vintage$ppp_rv)) {
        vintage$ppp_rv <- as.character(vintage$ppp_rv)
      }

      # remove Vs if necessary
      vintage$ppp_rv <- gsub("[Vv]", "", vintage$ppp_rv)
      # make sure it is two-digit long
      if (nchar(vintage$ppp_rv) == 1) {
        vintage$ppp_rv <- paste0("0",vintage$ppp_rv)
      }

      # check it has digits only
      if (grepl("\\D+", vintage$ppp_rv)) {
        msg     <- c(
          "Object {.field ppp_rv} in parameter {.field vintage} must contain
             digits only",
          "x" = "you provided {.emph {vintage$ppp_rv}}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )
      }

      # match only 8 characters
      if (!(nchar(vintage$ppp_rv) %in% c(1, 2))) {
        msg     <- c(
          "{.field ppp_rv} number must be either 1 or  2-digit long",
          "x" = "you provided {vintage$ppp_rv}, which is {nchar(vintage$ppp_rv)}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

      # check it is a valid year
      u_rv <- ppp_v[ppp_year == vintage$ppp_year,
                    unique(ppp_rv)]

      if (!(vintage$ppp_rv %in% u_rv)) {
        msg     <- c(
          "{.field ppp_rv}, {.emph {vintage$ppp_rv}}, is not available",
          "i" = "{.field ppp_rv} must be {u_rv}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## adaptation version  of PPP --------

    if (is.null(vintage$ppp_av)) {
      m_av <-
        ppp_v[ppp_year == vintage$ppp_year & ppp_rv == vintage$ppp_rv,
              max(ppp_av)]

      vintage$ppp_av <- m_av

    } else {

      # convert to character
      if (is.numeric(vintage$ppp_av)) {
        vintage$ppp_av <- as.character(vintage$ppp_av)
      }

      # remove Vs if necessary
      vintage$ppp_av <- gsub("[Vv]", "", vintage$ppp_av)
      # make sure it is two-digit long
      if (nchar(vintage$ppp_av) == 1) {
        vintage$ppp_av <- paste0("0",vintage$ppp_av)
      }

      # check it has digits only
      if (grepl("\\D+", vintage$ppp_av)) {
        msg     <- c(
          "Object {.field ppp_av} in parameter {.field vintage} must contain
             digits only",
          "x" = "you provided {.emph {vintage$ppp_av}}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )
      }

      # match only 8 characters
      if (!(nchar(vintage$ppp_av) %in% c(1, 2))) {
        msg     <- c(
          "{.field ppp_av} number must be either 1 or  2-digit long",
          "x" = "you provided {vintage$ppp_av}, which is {nchar(vintage$ppp_av)}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

      # check it is a valid year
      u_av <- ppp_v[ppp_year == vintage$ppp_year,
                    unique(ppp_av)]

      if (!(vintage$ppp_av %in% u_av)) {
        msg     <- c(
          "{.field ppp_av} select, {.emph {vintage$ppp_av}}, is not available",
          "i" = "{.field ppp_av} must be {u_av}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }


    } # end of _av when is not null


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## identity --------

    if (is.null(vintage$identity)) {

      vintage$identity <- "PROD" # default

    } else { # end of is  null

      vintage$identity <- toupper(vintage$identity)

      # correct identity
      ident_opt <- c("PROD", "INT", "TEST")

      if (!(vintage$identity %in% ident_opt)) {
        msg     <- c(
          "{.field identity} {.emph {vintage$identity}} not available. ",
          "i" = "you must use one of {.emph {ident_opt}}"
        )
        cli::cli_abort(msg,
                       class = "pipload_error"
        )

      }

    } # end of is not NULL


  } # End of if name is not provided in list

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create vintage  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #sort
  vintage   <- as.list(vintage)[rqr_sect]
  final_name <- paste(vintage, collapse = "_")




  # Return -------------
  return(final_name)

}



