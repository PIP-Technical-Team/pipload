#' Create global variables for PIP data management
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `pip_create_globals` is now available in the `pipfun` package.
#'
#' @param root_dir character: root directory of the PIP data
#' @param out_dir character: Output Directory. Default is `root_dir`
#' @param vintage character: name of output folder. It could be "latest", "new",
#'   or any other name. if it is "latest" (default), the most recent version
#'   available in the vintage directory of the form "%Y%m%d" will be used. If it
#'   is "new", a new folder with a name of the form "%Y%m%d" will be created.
#'   All the names will be coerced to lower cases
#' @param clean logical: if TRUE it cleans all empty directories that have been
#'   created by mistake. Default is FALSE.
#' @param verbose logical: display messages. Default is
#'   `getOption("pipload.verbose")`
#' @param create_dir logical: If TRUE creates output directory or any other
#'   directory that is part of the returned global and that does not exist.
#'   Otherwise it just returns the directory path **even if**  the
#'   directory does not exist
#'
#' @return list
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' pipfun::pip_create_globals()
#' }
pip_create_globals <- function(root_dir   = Sys.getenv("PIP_ROOT_DIR"),
                               out_dir    = root_dir,
                               vintage    = NULL,
                               clean      = FALSE,
                               verbose    = getOption("pipload.verbose"),
                               create_dir = FALSE) {


  lifecycle::deprecate_warn("0.2.0.9009", "pip_create_globals()", "pipfun::pip_create_globals()")
  pipfun::pip_create_globals(root_dir = root_dir,
                             out_dir  = out_dir,
                             vintage  = vintage,
                             clean    = clean,
                             verbose  = verbose,
                             create_dir = create_dir)

}




