#' Add gls list to the global envirnment. To be used in zzz.R in other packages
#'
#' `r lifecycle::badge("deprecated")`
#' `add_gls_to_env` has been moved to `pipfun::add_gls_to_env()`
#'
#' if you don't the official value in `Sys.getenv("PIP_ROOT_DIR")` you can
#' provide the object `root_dir  <- "<you directory>"` before executing the first
#' function. In this way, object `gls`, which is a promise, will be
#' created using with you `root_dir`. Otherwise, you can specify the complete
#' directory path for each function.
#' @inheritParams pip_create_globals
#'
#' @return TRUE
#' @keywords internal
add_gls_to_env <- function(root_dir = NULL,
                           out_dir  = NULL,
                           vintage  = "latest",
                           clean    = FALSE) {

  lifecycle::deprecate_soft("0.1.23",
                            "add_gls_to_env()",
                            "pipfun::add_gls_to_env()")

  # redirecting to pipfun::add_gls_to_env
  pipfun::add_gls_to_env(root_dir = root_dir,
                         out_dir  = out_dir,
                         vintage  = vintage,
                         clean    = clean)

}


