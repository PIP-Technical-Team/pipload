#' Load cache inventory
#'
#' @param tool character: Either "PC" for Poverty calculator or "TB" for table
#'   maker
#' @param version character: version of data in the form
#'   `yyyy_xx_xx_[PROD|INT|TEST]`. Default is the most recent PROD version
#' @inheritParams pip_find_cache
#' @export
pip_load_cache_inventory <-
  function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
           gls      = pipfun::pip_create_globals(root_dir),
           pipedir  = gls$PIP_PIPE_DIR,
           tool     = c("pc", "PC", "TB", "tb"),
           version  = NULL
         ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # right arguments
  tool <- match.arg(tool)  |>
    tolower()

  tool_dir <- fs::path(pipedir, glue("{tool}_data/cache/clean_survey_data/"))

  if (is.null(version)) {
    version <- last_prod_version(tool_dir)

  }

  # inventory file
  inv_file <- fs::path(tool_dir, version, "_crr_inventory/crr_inventory.fst")

  #--------- Load Data ---------

  if (fs::file_exists(inv_file)) {
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

