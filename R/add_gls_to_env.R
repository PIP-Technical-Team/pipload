#' Add gls list to the global envirnment. To be used in zzz.R in other packages
#'
#' if you don't the official value in `Sys.getenv("PIP_ROOT_DIR")` you can
#' provide the object `root_dir  <- "<you directory>"` before executing the first
#' function. In this way, object `gls`, which is a promise, will be
#' created using with you `root_dir`. Otherwise, you can specify the complete
#' directory path for each function.
#' @inheritParams pip_create_globals
#'
#' @return TRUE
#' @export
add_gls_to_env <- function(root_dir = NULL,
                           out_dir  = NULL,
                           vintage  = "latest",
                           clean    = FALSE) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # evaluate global environment   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  ## defined values --------
  obj <-  ls(pos = ".GlobalEnv")

  # remove gls if it  exists
  rm(list = obj[obj %in% c("gls")], pos = ".GlobalEnv")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # define root_dir   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## if root_dir should come from Renviron --------

  if (is.null(root_dir)) {

    # If root_dir does not exist, create it
    if (!("root_dir" %in% obj)) {
      root_dir  <-  Sys.getenv("PIP_ROOT_DIR")
      # root_dir  <-  Sys.getenv("PIP_ROOT_DIRfff")

      # assign('root_dir', root_dir, envir = globalenv())

    } else {
      cli::cli_alert_info("object {.envvar root_dir} is already defined in
                   Global env to  {.url {get('root_dir', envir = globalenv())}}.
                   To get back to default
                   values, make sure you remove it from memory by typing
                   {.code rm(root_dir)}",
                   wrap = TRUE)
      root_dir <- get('root_dir', envir = globalenv())
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## If root_dir is provided by user or not found in Renviron --------

  if (is.null(out_dir)) {
    out_dir <- root_dir
  }

  # create promises and assign to global env
  if (root_dir != "") {
    # globals
    gls <- pip_create_globals(root_dir = root_dir,
                              out_dir  = out_dir,
                              vintage  = vintage,
                              clean    = clean)
    assign('gls', gls, envir = globalenv())

  } else {

    # assign('root_dir', "", envir = globalenv())
    delayedAssign(x          =  "gls",
                  value      = pip_create_globals(root_dir = root_dir,
                                                  out_dir  = out_dir,
                                                  vintage  = vintage,
                                                  clean    = clean),
                  assign.env =  globalenv(),
                  eval.env   = globalenv())
  }

  return(invisible(TRUE))

}


