#' Title
#'
#' @param inv_file character: file path to be loaded.
#' @inheritParams pip_inventory
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_inventory_load <- function(inv_file = paste0(getOption("pip.maindir"),
                                                 "_inventory/inventory.fst"),
                               inventory_version = NULL
                               ) {
  if (file.exists(inv_file)) {
    df       <- fst::read_fst(inv_file)
    setDT(df)
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
