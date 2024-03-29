#' Create global variables for PIP data management
#'
#' @param root_dir character: root directory of the PIP data
#' @param out_dir character: Output Directory. Default is `root_dir`
#' @param vintage character: name of output folder. It could be "latest",
#'   "new", or any other name. if it is "latest" (default), the most recent
#'   version available in the vintage directory of the form "%Y%m%d" will be
#'   used. If it is "new", a new folder with a name of the form "%Y%m%d" will be
#'   created. All the names will be coerced to lower cases
#' @param suffix character: suffix to be added to the name of the vintage
#'   folder. Useful for testing purposes. Something of the form "%Y%m%d_test"
#'   won't be taken into account if the `vintage = "latest"`
#' @param clean logical: if TRUE it cleans all empty directories that have been
#'   created by mistake. Default is FALSE.
#' @param verbose logical: display messages. Default is `getOption("pipload.verbose")`
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' pip_create_globals()
#' }
pip_create_globals <- function(root_dir = Sys.getenv("PIP_ROOT_DIR"),
                               out_dir  = root_dir,
                               vintage  = "latest",
                               suffix   = NULL,
                               clean    = FALSE,
                               verbose  = getOption("pipload.verbose")) {


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
  if (verbose) {
    if (root_dir == "" || is.null(root_dir)) {
      cli::cli_alert_warning("{.field root_dir} is not defined. Directory paths
                             will lack network-drive root directory",
                             wrap = TRUE)
    } else if (root_dir != Sys.getenv("PIP_ROOT_DIR")) {
      cli::cli_alert_info("Alternative root directory for {.field root_dir} is
                          set to {.url {root_dir}}",
                             wrap = TRUE)
    }
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
  si <- Sys.info()
  authorized <- c("wb384996", "wb499754", "wb561460") # temporal solution
  # if (grepl("^wb", tolower(si[["user"]]))) {
  if (tolower(si[["user"]]) %in% authorized) {
    glbs$POVCALNET        <-  "//wbntpcifs/povcalnet/01.PovcalNet/"
    # Povcalnet master
    glbs$PCN_MASTER       <- fs::path(glbs$POVCALNET, "00.Master/02.vintage/")
  }

  #
  glbs$DLW_RAW_DIR          <- fs::path(root_dir,"DLW-RAW")



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # OUTPUT dirs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Poverty calculator --------

  # Main output folder
  glbs$OUT_DIR_PC   <- fs::path(out_dir, 'pip_ingestion_pipeline/pc_data/output/')
  create_dir(glbs)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## vintage directories --------

  # create vintage dir for PC
  out_path_pc <- check_and_create(dir     = glbs$OUT_DIR_PC,
                                  vintage = vintage,
                                  DATE    = glbs$DATE,
                                  clean   = clean,
                                  verbose = verbose)


  available_paths <- fs::dir_ls(path = glbs$OUT_DIR_PC,
                               type = "directory")

  glbs$available_OUT_DIR_PC <- available_paths

  # Final survey data output dir
  glbs$OUT_SVY_DIR_PC   <- fs::path(out_path_pc, '/survey_data/')

  #  Estimations output dir
  glbs$OUT_EST_DIR_PC   <- fs::path(out_path_pc, '/estimations/')

  # aux data output dir
  glbs$OUT_AUX_DIR_PC   <- fs::path(out_path_pc, '/_aux/')


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Table Maker --------
  #  Main TB output folder
  glbs$OUT_DIR_TB   <- fs::path(out_dir, 'pip_ingestion_pipeline/tb_data/output/')
  create_dir(glbs)

  # create vintage dir for PC
  out_path_tb <- check_and_create(dir     = glbs$OUT_DIR_TB,
                                  vintage = vintage,
                                  DATE    = glbs$DATE,
                                  clean   = clean,
                                  verbose = verbose)


  #  Estimations output dir of table baker
  glbs$OUT_EST_DIR_TB   <- fs::path(out_path_tb, 'estimations/')


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

  purrr::walk(.x = glbs[is_fs_path],
              .f = ~{
                if (!fs::dir_exists(.x[length(.x)])) {
                  fs::dir_create(path    = .x,
                                 recurse = TRUE)
                }
              })

  return(invisible(TRUE))
}





#' check vintage version and create
#'
#' @param dir character: output directory
#' @param DATE character: date of the form "%Y%m%d"
#' @inheritParams pip_create_globals
#'
check_and_create <- function(dir, vintage, DATE, clean, verbose) {

  # on.exit ------------
  on.exit({

  })

  # Computations -------
  # create vintage dir for PC
  available_paths <- fs::dir_ls(path = dir,
                                type = "directory")

  available_dirs  <- gsub("(.+)/([^/]+)", "\\2",  available_paths)
  vintages        <- gsub("(.*)([0-9]{8})(.*)", "\\2",  available_dirs)
  if (length(vintages) == 0) {
    latest_vintage <- DATE
  } else {
    latest_vintage <- max(vintages)

  }

  if (vintage  == "latest") {

    out_dir <- latest_vintage

  } else if (vintage == "new") {

    out_dir <- DATE

  } else {
    out_dir <- vintage
  }

  out_path <- fs::path(dir, out_dir)

  if (!(fs::dir_exists(out_path))) {

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

  }

  # Return -------------
  return(out_path)



}




#' Create output directory name
#'
#' This function creates the name of the directory that used as vintage control
#' of the PIP project.
#'
#' @param vintage list or character: If list, all objects should be named and
#'   only the following names are accepted: `c("release", "ppp_year", "ppp_rv",
#'   "ppp_av", "identity")`. Alternatively, it could a single-object list called
#'   "names" like `vintage = list(name = "some_name")`.  If character, it should
#'   be of length equal to 1.
#'
#' @return
#' @export
create_vintage <- function(vintage = list(name = "latest")) {


  # Defenses -----------
  stopifnot( exprs = {

    is.list(vintage) || is.character(vintage)
    is.character(vintage) && (length(vintage) == 1)

  }
  )

  TIME <- format(Sys.time(), "%Y%m%d%H%M%S")
  DATE <- format(Sys.Date(), "%Y%m%d")



  # Check that correct names are used -------
  if (is.list(vintage)) {
    # user sections
    user_sect <- names(vintage)
    # requires sections
    rqr_sect  <- c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")
    # section no available
    no_sect  <- user_sect[!(user_sect %in% c("name", rqr_sect))]

    # error handling
    if (length(no_sect) > 0) {
      msg     <- c(
        "names provided in {.field vintage} are not allowed",
        "*" = "names must be {.emph {rqr_sect}}",
        "x" = "name{?s} {.emph {no_sect}} not allowed"
      )
      cli::cli_abort(msg,
                     class = "pipload_error"
      )
    }

    if ("name"  %in% user_sect && length(user_sect) > 1) {
      msg     <- c(
        "you must select either {.field name} or {.field {rqr_sect}}",
        "x" = "You selected {.emph {user_sect}}"
      )
      cli::cli_abort(msg,
                     class = "pipload_error"
      )

    }

    if (user_sect == "name") {
      out_dir <- vintage$name
    } else {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Release --------

      if (is.null(vintage$release)) {
        vintage$release <- DATE

      } else {

        # match only numbers
        if (grepl("\\D+", vintage$release)) {
          msg     <- c(
            "Object {.field release} in parameter {.field vintage} must contain
             digits only",
            "x" = "you provided {.emph {vintage$release}}"
          )
          cli::cli_abort(msg,
                         class = "pipload_error"
          )
        }

        if (length(vintage$release) != 8) {
          msg     <- c(
            "{.field release} number must be 8-digit long",
            "x" = "you provided {length(vintage$release)},
            which is {length(vintage$release)}"
            )
          cli::cli_abort(msg,
                        class = "pipload_error"
                        )

        }

      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## PPP year --------




    }

  }





  # Return -------------
  return(out_dir)

}



