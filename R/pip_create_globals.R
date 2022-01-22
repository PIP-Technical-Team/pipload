#' Create global variables for PIP data management
#'
#' @param root_dir character: root directory of the PIP data
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pip_create_globals()
#' }
pip_create_globals <- function(root_dir = Sys.getenv("PIP_ROOT_DIR")) {

  if (root_dir == "" || is.null(root_dir)) {
    cli::cli_alert_warning("{.field root_dir} is not defined. Directory paths
                           will lack network-drive root directory",
                           wrap = TRUE)
  } else if (root_dir != Sys.getenv("PIP_ROOT_DIR")) {
    cli::cli_alert_info("Alternative root directory for {.field root_dir} set
                           to {.url {root_dir}}",
                           wrap = TRUE)
  }
  glbs <- list()

  # Input dir
  glbs$PIP_DATA_DIR     <- paste0(root_dir, 'PIP-Data_QA/')

  # '//w1wbgencifs01/pip/pip_ingestion_pipeline/' # Output dir
  glbs$PIP_PIPE_DIR     <- paste0(root_dir, 'pip_ingestion_pipeline/')

  # Cached survey data dir
  glbs$CACHE_SVY_DIR_PC <- paste0(glbs$PIP_PIPE_DIR, 'pc_data/cache/clean_survey_data/')

  # Final survey data output dir
  glbs$OUT_SVY_DIR_PC   <- paste0(glbs$PIP_PIPE_DIR, 'pc_data/output/survey_data/')

  #  Estimations output dir
  glbs$OUT_EST_DIR_PC   <- paste0(glbs$PIP_PIPE_DIR, 'pc_data/output/estimations/')

  # aux data output dir
  glbs$OUT_AUX_DIR_PC   <- paste0(glbs$PIP_PIPE_DIR, 'pc_data/output/_aux/')

  #  Estimations output dir of table baker
  glbs$OUT_EST_DIR_TB   <- paste0(glbs$PIP_PIPE_DIR, 'tb_data/output/estimations/')

  glbs$TIME             <- format(Sys.time(), "%Y%m%d%H%M%S")

  # Table Maker paths
  glbs$TB_DATA          <- paste0(glbs$PIP_PIPE_DIR, 'tb_data/')

  glbs$TB_ARROW         <- paste0(glbs$PIP_PIPE_DIR, 'tb_data/arrow/')

  glbs$CACHE_SVY_DIR_TB <- paste0(glbs$TB_DATA, 'cache/clean_survey_data/')

  # Old POVCalnet
  glbs$POVCALNET        <-  "//wbntpcifs/povcalnet/01.PovcalNet/"

  # Povcalnet master
  glbs$PCN_MASTER       <- paste0(glbs$POVCALNET, "00.Master/02.vintage/")


  ### Max dates --------
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
