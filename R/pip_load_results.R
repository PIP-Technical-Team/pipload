# add globals
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c("menu")
  )
}

#' Load PIP pre-calculated data sets
#'
#' @param output character: name of the output file
#' @inheritParams pip_load_cache
#' @inheritParams pip_create_globals
#'
#' @return data.table
#' @export
#'
#' @examples
#' pip_load_results("interpolated_means")
pip_load_results <- function(output   = NULL,
                             country  = NULL,
                             year     = NULL,
                             tool     = c("PC", "TB"),
                             type     = c("dataframe", "list"),
                             root_dir = Sys.getenv("PIP_ROOT_DIR"),
                             verbose  = getOption("pipload.verbose")
                             ) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # check arguments   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  tool <- match.arg(tool)
  type <- match.arg(type)

  if (tool == "PC") {
    resdir   = pip_create_globals(root_dir)$OUT_EST_DIR_PC
  } else{
    resdir   = pip_create_globals(root_dir)$OUT_EST_DIR_TB
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## In case output is NULL provide menu --------

  if (is.null(output)) {

    av_files  <- list.files(resdir, pattern = "fst$")
    av_files  <- gsub("\\.fst", "", av_files)
    selection <- menu(av_files, title = "Select file to load")
    output    <- av_files[selection]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## load file --------

  full_path <- fs::path(resdir, output, ext = "fst")

  dt <- fst::read_fst(full_path, as.data.table = TRUE)

  if (verbose)
    cli::cli_alert_info("{.path {output}} has been loaded")

  return(dt)

}

