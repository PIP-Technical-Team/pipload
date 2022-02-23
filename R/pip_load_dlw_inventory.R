#' @title Prepare DatalibWeb inventory
#'
#' @description takes dlw inventory in csv form in the official folder structure and format
#' it to be included in the pipeline. The original csv file is updated each time
#' the dlw inventory is updated
#'
#' @param dlw_dir character: path of datalibweb raw data
#'
#' @return data.table
#' @export
#'
#' @examples
#' pip_load_dlw_inventory()
pip_load_dlw_inventory  <- function(
  root_dir = Sys.getenv("PIP_ROOT_DIR"),
  dlw_dir  = pip_create_globals(root_dir)$DLW_RAW_DIR
  ){

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # directories and paths   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  dlw_inv_path <- fs::path(dlw_dir,"_Inventory")
  dlw_inv_file <- fs::path(dlw_inv_path, "dlw_inventory", ext = "fst")

  if (!fs::file_exists(dlw_inv_file)) {

    msg     <- c(
      "File does not exists",
      "x" = "{dlw_inv_file} not found.",
      "i" = "check connection or {.field pipload} globals"
    )
    cli::cli_abort(msg,
                   class = "pipdata_error"
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # load data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  dlw_inv <-fst::read_fst(dlw_inv_file,
                          as.data.table = TRUE)

  dlw_inv[,
          fullname := fs::path(root_dir, fullname)]

  return(dlw_inv)
}
