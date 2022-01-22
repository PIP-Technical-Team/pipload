#' Add gls list to the global envirnment. To be used in zzz.R in other packages
#'
#' if you don't the official value in `Sys.getenv("PIP_ROOT_DIR")` you can
#' provide the object `root_dir  <- "<you directory>"` before executing the first
#' function. In this way, object `gls`, which is a promise, will be
#' created using with you `root_dir`. Otherwise, you can specify the complete
#' directory path for each function.
#'
#' @return TRUE
#' @export
add_gls_to_env <- function() {


  ## defined values --------
  obj <-  ls(pos = ".GlobalEnv")

  # remove gls it  exists
  rm(list = obj[obj %in% c("gls")], pos = ".GlobalEnv")

  # If root_dir does not exist, create it
  if (!("root_dir" %in% obj)) {
    root_dir  <-  Sys.getenv("PIP_ROOT_DIR")
    # root_dir  <-  Sys.getenv("PIP_ROOT_DIRfff")

    # assign('root_dir', root_dir, envir = globalenv())

  } else {
    cli::cli_alert_info("object {.envvar root_dir} is already defined in
                   Global env to  {.url {root_dir}}. To get back to default
                   values, make sure you remove it from memory by typing
                   {.code rm(root_dir)}",
                   wrap = TRUE)
  }

  # create promises and assign to global env
  if (root_dir != "") {
    # globals
    gls <- pip_create_globals(root_dir)
    assign('gls', gls, envir = globalenv())

  } else {

    # assign('root_dir', "", envir = globalenv())
    delayedAssign(x          =  "gls",
                  value      = pip_create_globals(root_dir),
                  assign.env =  globalenv(),
                  eval.env   = globalenv())
  }

  return(invisible(TRUE))

}


