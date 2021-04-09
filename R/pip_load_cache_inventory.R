#' Load cache inventory
#'
#' @param pipedir character: directory path of pipe lines ingestiion. Default is
#'   `getOption("pip.pipedir")`
#' @param tool character: Either "PC" for Poverty calculator or "TM" for table
#'   maker
#' @return
#' @export
pip_load_cache_inventory <- function(pipedir = getOption("pip.pipedir"),
                                     tool    = c("PC", "TM")
                                     ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # right arguments
  tool <- match.arg(tool)

  if (tool == "PC") {
    inv_file <- paste0(pipedir, "pc_data/cache/clean_survey_data/_crr_inventory/crr_inventory.fst")
  } else {
    cli::cli_alert("There is not cache inventory file yet for table maker. Return NULL")
    return(NULL)
  }

  #--------- Load Data ---------

  if (file.exists(inv_file)) {
    df       <- fst::read_fst(inv_file,
                              as.data.table = TRUE)
    return(df)

  } else {
    rlang::abort(c(
      paste("file", inv_file, "does not exist"),
      i = "Check your connection to the drives"
    ),
    class = "pipload_error"
    )
  }
}

