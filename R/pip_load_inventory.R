#' Load inventory of welfare aggregate datasets
#'
#' @param inv_file character: file path to be loaded.
#' @inheritParams pip_inventory
#' @inheritParams pip_create_globals
#'
#' @return data.table
#' @export
#' @import data.table
#'
#' @examples
#' \dontrun{
#' pip_load_inventory()
#' }
pip_load_inventory <- function(root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                               maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                               inv_file = fs::path(maindir,
                                                 "_inventory/inventory.fst")
                               ) {

  #--------- Load Data ---------

  if (file.exists(inv_file)) {
    df       <- fst::read_fst(inv_file,
                              as.data.table = TRUE)

    # add root directory back
    df[, orig := fs::path((root_dir), orig)]
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
