#' Create global variables for PIP data management
#'
#' @param root_dir character: root directory of the PIP data
#' @param vintage character: name of output folder. It could be "lattest",
#'   "new", or any other name. if it is "lattest" (default), the most recent
#'   version available in the vintage directory of the form "%Y%m%d" will be
#'   used. If it is "new", a new folder with a name of the form "%Y%m%d" will be
#'   created. All the names will be coerced to lower cases
#' @param suffix character: suffix to be added to the name of the vintage
#'   folder. Useful for testing purposes. Something of the form "%Y%m%d_test"
#'   won't be taken into account if the `vintage = "lattest"`
#' @param clean logical: if TRUE it cleans all empty direcotories that have been
#'   created by mistake. Default is FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pip_create_globals()
#' }
pip_create_globals <- function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
                               vintage  = "lattest",
                               suffix   = NULL,
                               clean    = FALSE) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # setup   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(vintage)){
    vintage <- tolower(vintage)
  } else {
    vint_type <- typeof(vintage)
    cli::cli_abort("{.field vintage} must be character, not {vint_type}")
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read from Renviron --------

  if (root_dir == "" || is.null(root_dir)) {
    cli::cli_alert_warning("{.field root_dir} is not defined. Directory paths
                           will lack network-drive root directory",
                           wrap = TRUE)
  } else if (root_dir != Sys.getenv("PIP_ROOT_DIR")) {
    cli::cli_alert_info("Alternative root directory for {.field root_dir} is
                        set to {.url {root_dir}}",
                           wrap = TRUE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## init list and basic inputs --------

  glbs <- list()

  glbs$TIME <- format(Sys.time(), "%Y%m%d%H%M%S")
  glbs$DATE <- format(Sys.Date(), "%Y%m%d")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # input dirs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # welfare data dir
  glbs$PIP_DATA_DIR     <- fs::path(root_dir, 'PIP-Data_QA/')

  # '//w1wbgencifs01/pip/pip_ingestion_pipeline/' # Output dir
  glbs$PIP_PIPE_DIR     <- fs::path(root_dir, 'pip_ingestion_pipeline/')

    # Cached survey data dir
  glbs$CACHE_SVY_DIR_PC <- fs::path(glbs$PIP_PIPE_DIR, 'pc_data/cache/clean_survey_data/')

  # Old POVCalnet
  glbs$POVCALNET        <-  "//wbntpcifs/povcalnet/01.PovcalNet/"

  # Povcalnet master
  glbs$PCN_MASTER       <- fs::path(glbs$POVCALNET, "00.Master/02.vintage/")



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # OUTPUT dirs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Poverty calculator --------

  # Main output folder
  glbs$OUT_DIR_PC   <- fs::path(glbs$PIP_PIPE_DIR, 'pc_data/output/')
  create_dir(glbs)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## vintage directories --------

  # create vintage dir
  available_paths <- fs::dir_ls(path = glbs$OUT_DIR_PC,
                               type = "directory")

  available_dirs  <- gsub("(.+)/([^/]+)", "\\2",  available_paths)
  vintages        <- gsub("(.*)([0-9]{8})(.*)", "\\2",  available_dirs)
  if (length(vintages) == 0) {
    lattest_vintage <- glbs$DATE
  } else {
    lattest_vintage <- max(vintages)

  }

  if (vintage  == "lattest") {

      out_dir <- lattest_vintage

  } else if (vintage == "new") {

    out_dir <- glbs$DATE

  } else {
    out_dir <- vintage
  }

  out_path <- fs::path(glbs$OUT_DIR_PC, out_dir)
  if (fs::dir_exists(out_path)) {

    cli::cli_alert("directory {.url {out_dir}} already exist")

  } else {

    cli::cli_alert("directory {.url {out_dir}} will be created")
    fs::dir_create(out_path, recurse = TRUE)

  }

  if (isTRUE(clean)) {
    for (i in seq_along(available_paths)) {
      x <- available_paths[[i]]
      di <- fs::dir_info(x)
      if (nrow(di) == 0) {
        fs::dir_delete(x)
      }
    }

    available_paths <- fs::dir_ls(path = glbs$OUT_DIR_PC,
                                  type = "directory")

  }

  glbs$available_OUT_DIR_PC <- available_paths

  # Final survey data output dir
  glbs$OUT_SVY_DIR_PC   <- fs::path(out_path, '/survey_data/')

  #  Estimations output dir
  glbs$OUT_EST_DIR_PC   <- fs::path(out_path, '/estimations/')

  # aux data output dir
  glbs$OUT_AUX_DIR_PC   <- fs::path(out_path, '/_aux/')


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Table Maker --------

  #  Main TB output folder
  glbs$OUT_DIR_TB   <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data/output/')


  #  Estimations output dir of table baker
  glbs$OUT_EST_DIR_TB   <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data/output/estimations/')


  #  Estimations output dir of table baker
  glbs$OUT_EST_DIR_TB   <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data/output/estimations/')


  # Table Maker paths
  glbs$TB_DATA          <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data/')

  glbs$TB_ARROW         <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data/arrow/')

  glbs$CACHE_SVY_DIR_TB <- fs::path(glbs$TB_DATA, 'cache/clean_survey_data/')

  create_dir(glbs)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Max dates   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  max_year_country   <- 2019
  if (is.null(max_year_country)) {

    c_month  <- as.integer(format(Sys.Date(), "%m"))
    max_year <- ifelse(c_month >= 8,  # August
                       as.integer(format(Sys.Date(), "%Y")) - 1, # After august
                       as.integer(format(Sys.Date(), "%Y")) - 2) # Before August

  } else {

    max_year <- max_year_country

  }

  glbs$PIP_YEARS        <- 1977:(max_year + 1) # Years used in PIP
  glbs$PIP_REF_YEARS    <- 1981:max_year # Years used in the interpolated means table

  glbs$FST_COMP_LVL     <- 100 # Compression level for .fst output files

  glbs$max_year_aggregate <- 2017

  return(glbs)
}

#' Check if path is fs_path and then create folder
#'
#' @param glbs list of object. Some of them are fs_paths
#'
create_dir <- function(glbs) {
  is_fs_path <- which(purrr::map_lgl(glbs, inherits, "fs_path"))
  purrr::walk(glbs[is_fs_path], fs::dir_create, recurse = TRUE)
  return(invisible(TRUE))
}

