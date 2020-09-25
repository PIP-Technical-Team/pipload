#' Load any auxiliary data
#'
#' @param msrdir character: measure (CPI) directory. created on `pip_prices()`.
#' @param measure character: Measure to be used. e.g., "cpi" or "ppp".
#'
#' @return
#' @export
#'
#' @examples
#' # Load CPI
#' pip_load_aux("cpi")
#'
#' # load PPP
#' pip_load_aux("ppp")
#'
#' # Load GDP
#' pip_load_aux("gdp")
pip_load_aux <- function(measure = NULL,
                         msrdir = paste0(getOption("pip.maindir"),
                                         "_aux/",
                                         measure, "/"),
                         version = NULL
                         ){

  if (is.null(measure)) {

    rlang::abort(c(
                  "`measure` must be defined, as it does not have default value",
                  i = "make sure `measure` is not NULL."
                  ),
                  class = "pipaux_error"
                  )


  }

  if (is.null(version)) {

    file_to_load <- paste0(msrdir, measure ,".fst")

  } else if (version %in% c("select", "pick", "choose")) {

    vint_dir <- paste0(msrdir, "_vintage")

    # Get all version avaiable
    vers <- fs::dir_ls(path    = vint_dir,
                       recurse = FALSE,
                       type    = "file",
                       regexp  = "cpi_[0-9]+")

    # Get just the dates
    vers      <- as.character(sort(vers, decreasing = TRUE))
    ver_dates <- gsub("(.*cpi_)([0-9]+)(.*)", "\\2", vers)
    ver_dates <- as.POSIXct(ver_dates, "%Y%m%d%H%M%S", tz=Sys.timezone())


    ans <- menu(ver_dates,
                title=paste("There are", length(ver_dates), "versions available.\n",
                            "Please select the one you want to load.")
    )

    file_to_load <- vers[ans]
  } else {
    msg     <- "The version selected is not available"
    hint    <- paste("Make sure `version` is either \n",
                     "[1] a date character in the form %Y%m%d%H%M%S, \n",
                     "[2] the words, `select`, `pick`, or `choose` to select a particular date.\n",
                     "[3] a negative number, so that  `pip_load_aux` will load that number of versions before the current one.")
    problem <- paste("you provided", version, ", which is not one of the three options above.")
    rlang::abort(c(
                  msg,
                  i = hint,
                  x = problem
                  ),
                  class = "pipload_error"
                  )

  }

  # check file exists
  if(file.exists(file_to_load)){

    df <- fst::read_fst(file_to_load)

  } else {
    msg <- paste("file `", measure,".fst` does not exist.")
    rlang::abort(c(
      msg,
      i = "check your connection or data availability"
    ),
    class = "pipaux_error"
    )

  }
  df <- pip_add_aux_labels(df,
                       measure = measure)
  return(df)
}
