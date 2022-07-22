#' Load any auxiliary data
#'
#' @param measure character: Measure to be used. e.g., "cpi" or "ppp". It can't
#'   be `NULL` anymore as it requires a main auxiliary directory
#' @param branch  character: Either "DEV", "PROD", or "main". Refers to which
#'   version of the data is being updated. Default is DEV for development.
#' @param version An integer or a quoted directive. "available": displays list
#'   of available versions for `measure`. "select"|"pick"|"choose": allows user
#'   to select the vintage of `measure`. if the integer is a zero or a negative
#'   number (e.g., `-1`), `pip_load_aux` will load that number of versions
#'   before the most recent version available. So, if `0`, it loads the current
#'   version. If `-1`, it will load the version before the current, `-2` loads
#'   two versions before the current one, and so on. If it is a positive number,
#'   it must be quoted (as character) and in the form "%Y%m%d%H%M%S". If "00",
#'   it load the most recent version of the data (similar to `version = 0` or
#'   `version = NULL` or `version = "0"`). The difference is that `"00"` load
#'   the most recent version of the vintage folder, rather than the current
#'   version in the dynamic folder. Thus, attribute "version" in `attr(dd,
#'   "version")` is the actual version of the most recent vintage of the file
#'   rather that `attr(dd, "version")` equal to "current", which is the default.
#'   Option "00" is useful for vintage control
#' @param msrdir character: measure directory. Default is
#'   `fs::path(maindir,"_aux", match.arg(branch), measure)`
#' @param file_to_load `r lifecycle::badge("deprecated")` `file_to_load` has
#'   been superseded for a more convenient combination of `filename` and
#'   `msrdir`. Now, it defaults to `filename`
#' @param filename character: name of file if different from `measure`. Default
#'   is `measure`
#' @param apply_label logical: if TRUE, predefined labels will apply to data
#'   loaded using `file_to_load` argument. Default TRUE. Tip: change to FALSE if
#'   the main structure of data has changed and labels have not been updated
#' @param verbose logical: whether to display message. Default is TRUE
#' @param preferred_format character: preferred format. default is "fst".
#' @param suffix character: suffix to be added to main measure name. Some
#'   measures have complementary data that depend directly from the main measure
#'   file. They are intended to be used for metadata purposes and efficiency.
#'   These complementary data is not used for development of new tools. for
#'   instance, `pip_load_aux(measure = "ppp", suffix = "vintage")` loads the
#'   vintage available for the PPP database
#' @inheritParams pip_find_cache
#' @inheritParams pip_inventory
#'
#' @return data.table
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
#' \dontrun{
#' df      <- pip_load_aux(measure, version = -1)
#' head(df)
#' df      <- pip_load_aux(measure, version = "pick")
#' }
pip_load_aux <- function(
    measure           ,
    branch            = c("DEV", "PROD", "main"),
    version           = NULL,
    root_dir          = Sys.getenv("PIP_ROOT_DIR"),
    maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
    msrdir            = fs::path(maindir, "_aux", match.arg(branch), measure),
    filename          = measure,
    file_to_load      = filename,
    apply_label       = TRUE,
    verbose           = getOption("pipload.verbose"),
    preferred_format  = NULL
    ) {


  lifecycle::deprecate_warn("0.2.0",
                            "pip_load_aux(file_to_load)",
                            "pip_load_aux(filename)")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   If file path IS provided   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  branch <- match.arg(branch)

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  stopifnot( exprs = {
    fs::dir_exists(msrdir)
  }
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Find format --------

  file_paths <- fs::dir_ls(msrdir,
                           type = "file",
                           regexp = glue("/{filename}\\."))

  if (is.null(preferred_format)) {

    # get the first of the allowed formats that is available
    fp  <- find_path(file_paths)   # preferred file path
    ext <- fs::path_ext(fp)        # extension

  } else {

    av_formats <- fs::path_ext(file_paths)

    if (!preferred_format %in% av_formats) {
      cli::cli_abort(c("Preferred format ({.field {preferred_format}}) is
                         not available",
                       x = "Available formats are {.field {av_formats}}"),
                     wrap = TRUE)
    }

    ext <- preferred_format
    # fp <- grep(glue("{ext}$"), file_paths, value = TRUE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## select version --------

  # select most recent version
  if (is.null(version)) {
    version <- 0
  }

  if (version == 0) {

    file_to_load <- fs::path(msrdir, filename, ext = ext)
    load_msg     <- "Most recent version of data loaded"
    apply_label  <- TRUE
    ver_attr     <- "current"

  } else {

    if (version == "00") {
      version <- 0
    }

    # Find Vintages options
    vint_dir <- fs::path(msrdir, "_vintage")
    if (!fs::dir_exists(vint_dir)) {
      msg <- c("Vintage directory does not exist. {.file {vint_dir}}")
      cli::cli_abort(msg, class = "pipload_error")
    }

    # Get all version available
    ver_paths <- fs::dir_ls(
      path    = vint_dir,
      recurse = FALSE,
      type    = "file",
      regexp  = glue("{filename}_[0-9]+\\.{ext}")
    )

    ver_files <-
      fs::path_file(ver_paths) |>
      fs::path_ext_remove() |>
      sort(decreasing = TRUE) |>
      as.character()


    # Get just the dates
    tvers     <-
      gsub(glue("({filename}_)([0-9]+)"), "\\2", ver_files)

    ver_dates <-
      as.POSIXct(tvers, "%Y%m%d%H%M%S", tz = Sys.timezone())

    version_posix <-
      as.character(version) |>
      as.POSIXct("%Y%m%d%H%M%S", tz = Sys.timezone())


    # If the user wants to pick the version.
    if (version == "available") {

      cli::cli_alert("Versions available for {.field {measure}}:
                     {.file {ver_dates}}")

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
    } else if (as.numeric(version) <= 0) {

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
    } else if (!is.na(version_posix)) {

      if (version  %in% tvers) {

        ans <- which(tvers == version)

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

    file_to_load <- ver_paths[ans]
    load_msg     <- glue("Version of data loaded: {ver_dates[ans]}")
    apply_label  <- FALSE
    ver_attr     <- tvers[ans]

  } # End of condition if version is different to NULL



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # check file exists
  wfilename    <- fs::path_file(file_to_load)
  if (fs::file_exists(file_to_load)) {
    df  <- read_by_format(file_to_load)

    if (verbose) {
      cli::cli_alert_success("{load_msg}:
                             {.file {wfilename}}")
    }
  } else {

    cli::cli_abort(c("file {.file {wfilename}} does not exist.",
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

  data.table::setattr(df, "version", ver_attr)
  return(df)
}


#' read file dependin on format and convert to data.table
#'
#' @param file_to_load character: file to load
#'
#' @return data.table
#' @export
read_by_format <- function(file_to_load) {

  pformat <- fs::path_ext(file_to_load)

  x <-
    if (pformat == "qs") {

      qs::qread(file_to_load)

      } else if (pformat == "fst") {

      fst::read_fst(file_to_load, as.data.table = TRUE)

    } else if (pformat == "rds") {

      readr::read_rds(file_to_load)

    } else if (pformat == "dta") {

      haven::read_dta(file_to_load)
    }

  if (is.data.frame(x)) {
    data.table::setDT(x)
  }

  return(x)
}


#' Find path of file to be loaded depending on extension hierarchy
#'
#'
#' @param file_paths chracter: vector of file paths
#'
#' @return character vector of length 1 with preferred file path
find_path <- function(file_paths) {

  extensions <- fs::path_ext(file_paths)
  ext_order <- c("qs", "fst", "rds", "dta")

  f <- FALSE
  i <- 1
  while (f == FALSE) {

    ext <- ext_order[[i]]

    if(ext  %in% extensions) {
      p <- which(extensions == ext)
      f <- file_paths[[p]]
    } else {
      i <- i + 1
    }

  }

  if (f == FALSE) {
    msg     <- c(
      "File not found",
      "*" = "At least one of the following extension should be available: {.file {ext_order}}"
    )
    cli::cli_abort(msg,
                   class = "error_class")
  }

  return(f)

}



