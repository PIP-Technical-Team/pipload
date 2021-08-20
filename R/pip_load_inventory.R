#' Load inventory of welfare aggregate datasets
#'
#' @param inv_file character: file path to be loaded.
#' @inheritParams pip_inventory
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
#' \dontrun{
#' pip_load_inventory()
#' }
pip_load_inventory <- function(root_dir          = Sys.getenv("PIP_DATA_ROOT_FOLDER"),
                               maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                               inv_file = paste0(maindir,
                                                 "_inventory/inventory.fst")
                               ) {

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
