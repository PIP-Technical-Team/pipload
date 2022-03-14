# @description `r lifecycle::badge("deprecated")`

#' Load inventory of welfare aggregate datasets
#'
#'
#' This function was deprecated because DLW data will loaded directly from a
#' flat folder structure which allows bypassing the use of `datalibweb` in
#' Stata, making the `pipdp` Stata package useless.From now on, use the `dlw`
#' functions: `pip_load_dlw_inventory`, `pip_find_dlw`, and `pip_load_dlw`.
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
pip_load_inventory <- function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
                               maindir  = pip_create_globals(root_dir)$PIP_DATA_DIR,
                               inv_file = fs::path(maindir,
                                                 "_inventory/inventory.fst")
                               ) {

  # lifecycle::deprecate_soft("0.1.13",
  #                           "pip_load_inventory()",
  #                           "pip_load_dlw_inventory()")

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
