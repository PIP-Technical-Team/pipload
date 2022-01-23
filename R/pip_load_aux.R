#' Load any auxiliary data
#'
#' @param msrdir character: measure (CPI) directory. created on `pip_prices()`.
#' @param measure character: Measure to be used. e.g., "cpi" or "ppp".
#' @param version An integer or a quoted directive. "available": displays list
#'   of available versions for `measure`. "select"|"pick"|"choose": allows user
#'   to select the vintage of `measure`. if the integer is a zero or a negative
#'   number (e.g., `-1`), `pip_load_aux` will load that number of versions
#'   before the most recent version available. So, if `0`, it loads the current
#'   version. If `-1`, it will load the version before the current, `-2` loads two
#'   versions before the current one, and so on. If it is a positive number, it
#'   must be quoted (as character) and in the form "%Y%m%d%H%M%S".
#' @param file_to_load character: file path to load. Does not work with any
#'   other argument
#' @param apply_label logical: if TRUE, predefined labels will apply to data
#'   loaded using `file_to_load` argument. Default TRUE. Tip: change to FALSE if
#'   the main structure of data has changed and labels have not been updated
#' @param verbose logical: whether to display message. Default is TRUE
#' @param preferred_format character: preferred format. default is "fst".
#' @inheritParams pip_find_cache
#'
#' @return
#' @export
#'
#' @examples
#' # Load CPI
#' cpi <- pip_load_aux("cpi")
#'
#' # load PPP
#' ppp <- pip_load_aux("ppp")
#'
#' # Load GDP
#' gdp <- pip_load_aux("gdp")
#'
#' measure <- "cpi"
#' av      <- pip_load_aux(measure, version = "available")
#' head(av)
#' df      <- pip_load_aux(measure, version = av[1])
#' head(df)
#' df      <- pip_load_aux(measure, version = -1)
#' head(df)
#' \dontrun{
#' df      <- pip_load_aux(measure, version = "pick")
#' }
pip_load_aux <- function(measure           = NULL,
                         root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                         maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                         msrdir            = paste0(maindir,
                                              "_aux/",
                                              measure, "/"),
                         version           = NULL,
                         file_to_load      = NULL,
                         apply_label       = TRUE,
                         verbose           = getOption("pipload.verbose"),
                         preferred_format  = NULL
                         ) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   If file path IS provided   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  allowed_formats  <- c("fst", "rds", "dta")


  if (!(is.null(file_to_load))) {
    if (!(is.null(measure))) {
      msg     <- "Syntax error"
      hint    <- "provide either `measure` or `file_to_load`"
      problem <- "you provided a value in `measure` that might be inconsistent with `file_to_load`"
      rlang::abort(c(
                    msg,
                    i = hint,
                    x = problem
                    ),
                    class = "error_class"
                    )

    }

    measure  <- gsub("(.*/)([a-z]+)_?[0-9]*\\.fst$", "\\2", file_to_load)
    load_msg <- paste("Data loaded from file path")


  } else {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #---------   if file path is NOT provides   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (is.null(measure)) {
      msg     <- "Syntax Error"
      hint    <- "You need to provide either a `measure` of file path in `file_to_load`"
      problem <- "`measure` needs to be defined when `file_to_load` is NULL"
      rlang::abort(c(
                    msg,
                    i = hint,
                    x = problem
                    ),
                    class = "pipload_error"
                    )
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Find format --------

    av_files   <- list.files(path = msrdir, pattern = paste0("^", measure, "\\."))
    av_formats <- gsub("(\\w+\\.)([[:lower:]]+)$", "\\2", av_files)


    if (all(!av_formats  %in% allowed_formats)) {
      cli::cli_abort(c("all the format available are not allowed",
                       x = "Only {.field {allowed_formats}} are allowed.
                       Currently directory has formats {.field {av_formats}}"),
                     wrap = TRUE)
    }


    if (is.null(preferred_format)) {
      # get the first of the allowed formats that is available
      preferred_format <- allowed_formats[allowed_formats %in% av_formats][1]
    } else {
      if (!preferred_format %in% av_formats) {
        cli::cli_abort(c("Preferred format ({.field {preferred_format}}) is not available",
                         x = "Available formats are {.field {av_formats}}"),
                       wrap = TRUE)
      }
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## seelct version --------

    # select most recent version
    if (is.null(version) || as.numeric(version) == 0) {
      path_of_file <- paste0(msrdir, measure)
      file_to_load <- paste0(path_of_file , ".", preferred_format)
      load_msg     <- paste("Most recent version of data loaded")
      apply_label  <- TRUE

    } else {
      # Find Vintages options
      vint_dir <- paste0(msrdir, "_vintage")

      # Get all version available
      vers <- fs::dir_ls(
        path    = vint_dir,
        recurse = FALSE,
        type    = "file",
        regexp  = paste0(measure, "_[0-9]+\\.", preferred_format)
      )

      # Get just the dates
      vers      <- as.character(sort(vers, decreasing = TRUE))
      tvers     <-
        gsub(paste0("(.*", measure, "_)([0-9]+)(.*)"), "\\2", vers)
      ver_dates <-
        as.POSIXct(tvers, "%Y%m%d%H%M%S", tz = Sys.timezone())

      # If the user wants to pick the version.
      if (version == "available") {
        message(paste("Versions available for", measure))
        print(ver_dates)

        return(invisible(tvers))

      } else if (version %in% c("select", "pick", "choose")) {
        ans <- menu(
          ver_dates,
          title = paste(
            "There are", length(ver_dates), "versions available.\n",
            "Please select the one you want to load."
            )
          )

        # If user select x number of versions before the current one
      } else if (as.numeric(version) < 0) {

        ans <- (as.numeric(version) * -1) + 1 # position in the vector of available versions

        if (ans > length(ver_dates)) {
          msg     <- "Invalid number of version"
          hint    <-
            "Did you want to load a different version using one of the other two methods?"
          problem <-
            paste(
              "There are only",
              length(ver_dates),
              "versions available and you\n",
              "selected version",
              ans
            )
          rlang::abort(c(msg,
                         i = hint,
                         x = problem),
                       class = "pipload_error")
        }

        # If the user select a particular date or version.
      } else if (!(is.na(as.POSIXct(as.character(version), "%Y%m%d%H%M%S", tz = Sys.timezone())))) {

        if (any(grepl(version, vers))) {
          ans <- which(grepl(version, vers))

        } else {
          msg     <-
            "The date you provided is not an available vintage version"
          hint    <-
            paste0(
              "run `pip_load_aux('",
              measure,
              "', version('available')`",
              "\nto check for available versions"
            )
          problem <-
            paste("you selected",
                  as.POSIXct(version, "%Y%m%d%H%M%S", tz = Sys.timezone()))
          rlang::abort(c(msg,
                         i = hint,
                         x = problem),
                       class = "pipload_error")

        }


      } else {
        msg     <- "The version selected is not available"
        hint    <- paste(
          "Make sure `version` is either \n",
          "[1] a date character in the form %Y%m%d%H%M%S, \n",
          "[2] the words, `select`, `pick`, or `choose` to select a particular date.\n",
          "[3] a negative number, so that  `pip_load_aux` will load that number of versions before the current one."
        )
        problem <-
          paste0("you provided ",
                 version,
                 ", which is not one of the three options above.")
        rlang::abort(c(msg,
                       i = hint,
                       x = problem),
                     class = "pipload_error")
      }

      file_to_load <- vers[ans]
      path_of_file <- gsub(paste0(".", preferred_format), "", file_to_load)
      load_msg     <- paste("Version of data loaded:", ver_dates[ans])
      apply_label  <- FALSE

    } # End of condition if version is different to NULL

  } # end of condition if file_to_path is NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # check file exists
  if (file.exists(file_to_load)) {
    df <- read_by_format(preferred_format)(path_of_file)

    if (verbose) {
      cli::cli_alert_success("{load_msg}:
                             {.file {path_of_file}.{preferred_format}}")
    }

  } else {
    msg <- paste0("file `", measure, ".", preferred_format, "` does not exist.")
    rlang::abort(c(msg,
                   i = "check your connection or data availability"),
                 class = "pipload_error")

  }

  if (apply_label) {
    df <- pip_add_aux_labels(df,
                             measure = measure,
                             verbose = verbose)

  } else {
    if (verbose) {
      cli::cli_alert_info("Labels not applied to versioning data")
    }
  }
  return(df)
}


#' read file dependin on format and convert to data.table
#'
#' @param pformat character: format of the file.
#'
#' @return
#' @export
read_by_format <- function(pformat) {

  force(pformat)

  function(x) {
    file2read <- paste0(x, ".", pformat)

    if (pformat == "fst") {
      x <- fst::read_fst(file2read, as.data.table = TRUE)
    } else if (pformat == "rds") {

      x <- readr::read_rds(file2read)
    } else if (pformat == "dta") {
      x <- haven::read_dta(file2read)
    }

  if (is.data.frame(x)) {
    data.table::setDT(x)
  }

  return(x)
  }
}
