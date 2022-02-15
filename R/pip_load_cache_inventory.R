#' Load cache inventory
#'
#' @param tool character: Either "PC" for Poverty calculator or "TB" for table
#'   maker
#' @inheritParams pip_find_cache
#' @export
#' @return data frame
#' \dontrun{
#' pip_load_cache_inventory()
#' }
pip_load_cache_inventory <- function(root_dir         = Sys.getenv("PIP_ROOT_DIR"),
                                     pipedir          = pip_create_globals(root_dir)$PIP_PIPE_DIR,
                                     tool    = c("PC", "TB")
                                     ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # right arguments
  tool <- match.arg(tool)

  if (tool == "PC") {
    inv_file <- fs::path(pipedir, "pc_data/cache/clean_survey_data/_crr_inventory/crr_inventory.fst")
  } else {
    inv_file <- fs::path(pipedir, "tb_data/cache/clean_survey_data/_crr_inventory/crr_inventory.fst")
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

