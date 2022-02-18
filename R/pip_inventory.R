#' Load or Update PIP microdata inventory. Wrapper of function pip_load_inventory and
#' pip_update_inventory
#'
#' @param action character: Either `load` or `update`
#' @param country character: vector with ISO3 country code to update
#' @param maindir character: main directory. By default it
#' uses `pip_create_globals(root_dir)$PIP_DATA_DIR`
#' @param force  logical: If TRUE, forced update implemented
#' @param inventory_version date: date of inventory version to load
#' @inheritParams pip_find_cache
#' @inheritParams pip_create_globals
#'
#' @return data.table
#' @export
#'
#' @examples
#' \dontrun{
#' pip_inventory("load")
#' }
pip_inventory <- function(action            = "load",
                          country           = NULL,
                          root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                          maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                          force             = FALSE,
                          inventory_version = NULL
                          ) {



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ---------   Initial parameters  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # inventory file to be used everywhere
  inv_file <- fs::path(maindir, "_inventory/inventory.fst")

  # get processing time for all data signatures
  time <- format(Sys.time(), "%Y%m%d%H%M%S")

  # User
  user <- Sys.info()[8]

  if (action == "load" ) {
    # ---------   Load data  ---------

    return(pip_load_inventory(inv_file = inv_file))

  } else if (action == "update") {
    #--------- Update data   ---------

    inventory <- pip_update_inventory(country = country,
                                      maindir = maindir,
                                      force   = force,
                                      time    = time,
                                      user    = user)
    return(invisible(inventory))

  } else {
    rlang::abort(c(
                "The action selected is not a valid name",
                i = "you can use `load` or `update`", # update this message automatically
                x = paste("you specified", action)
              ),
              class = "pipload_error"
              )
  }
}


