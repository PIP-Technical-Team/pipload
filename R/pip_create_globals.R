#' Create global variables for PIP data management
#'
#' @param root_dir character: root directory of the PIP data
#' @param out_dir character: Output Directory. Default is `root_dir`
#' @param vintage character: name of output folder. It could be "latest", "new",
#'   or any other name. if it is "latest" (default), the most recent version
#'   available in the vintage directory of the form "%Y%m%d" will be used. If it
#'   is "new", a new folder with a name of the form "%Y%m%d" will be created.
#'   All the names will be coerced to lower cases
#' @param clean logical: if TRUE it cleans all empty directories that have been
#'   created by mistake. Default is FALSE.
#' @param verbose logical: display messages. Default is
#'   `getOption("pipload.verbose")`
#' @param create_dir logical: If TRUE creates output directory or any other
#'   directory that is part of the returned global and that does not exist.
#'   Otherwise it just returns the directory path **even if**  the
#'   directory does not exist
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' pip_create_globals()
#' }
pip_create_globals <- function(root_dir   = Sys.getenv("PIP_ROOT_DIR"),
                               out_dir    = root_dir,
                               vintage    = NULL,
                               clean      = FALSE,
                               verbose    = getOption("pipload.verbose"),
                               create_dir = FALSE) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defenses   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # vintage
  stopifnot( exprs = {
    is.list(vintage) || is.character(vintage) || is.null(vintage)
  }
  )

  if (is.character(vintage)) {

    if (length(vintage) == 1) {

      if (!(vintage %in% c("latest", "new"))) {
        # pattern that identifies folders
        vintage_pattern <- "\\d{8}_\\d{4}_\\d{1,2}_\\d{1,2}_(PROD|TEST|INT)$"
        if (!grepl(vintage_pattern, vintage)) {
          msg     <- c(
            "Incorrect vintage name",
            "x" = "Vintage must check TRUE with {.field '{vintage_pattern}'}",
            "i" = "vintage must follow this convention,
            {.field %Y%m%d_YYYY_##_##_SSS}"
            )
          cli::cli_abort(msg,
                        class = "pipload_error",
                        wrap = TRUE
                        )
        }

      } # end of pattern check

    } else if (length(vintage) == 2) {

      if(!(any(c("latest", "new") %in% vintage) &&
           any(c("prod", "test", "int") %in% tolower(vintage)))) {
        msg     <- c(
          "When Vintage is character vector of length 2, it must meet the
          following",
          "*" = "One of its elements should be either {.field latest}
          or {.field new}",
          "*" = "One of its elements should be either {.field prod},
          {.field test}, or {.field int}",
          "x" = "you provided {.field {vintage}}"
          )
        cli::cli_abort(msg,
                      class = "pipload_error",
                      wrap  = TRUE
                      )

      }
    } else {
      msg     <- c(
        "When vintage is character vector, it must be
          of {.fn legth} 1 or 2",
        "x" = "you provided {.field vintage} of length {length(vintage)}")
      cli::cli_abort(msg,
                     class = "error_class",
                     wrap  = TRUE
      )

    }
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Read from Renviron --------
  if (verbose) {
    if (root_dir == "" || is.null(root_dir)) {
      cli::cli_alert_warning("{.field root_dir} is not defined.
                             Directory paths
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
  # INPUT dirs   ---------
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
  if (isTRUE(create_dir)) {
    create_dir(glbs)
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # OUTPUT dirs   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(vintage)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Poverty calculator --------

    # Main output folder
    glbs$OUT_DIR_PC   <- fs::path(out_dir, 'pip_ingestion_pipeline/pc_data/output/')

    if (isTRUE(create_dir)) {
      create_dir(glbs)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## vintage directories --------

    # create vintage dir for PC
    vintage_dir <- check_and_create(dir        = glbs$OUT_DIR_PC,
                                    vintage    = vintage,
                                    DATE       = glbs$DATE,
                                    clean      = clean,
                                    verbose    = verbose,
                                    create_dir = create_dir)

    glbs$vintage_dir <- vintage_dir
    out_path_pc     <- fs::path(glbs$OUT_DIR_PC, vintage_dir)


    glbs$available_OUT_DIR_PC <- fs::dir_ls(path = glbs$OUT_DIR_PC,
                                            type = "directory")
    # Final survey data output dir
    glbs$OUT_SVY_DIR_PC   <- fs::path(out_path_pc, '/survey_data/')

    #  Estimations output dir
    glbs$OUT_EST_DIR_PC   <- fs::path(out_path_pc, '/estimations/')

    # aux data output dir
    glbs$OUT_AUX_DIR_PC   <- fs::path(out_path_pc, '/_aux/')


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Table Maker --------
    #  Main TB output folder
    glbs$OUT_DIR_TB   <- fs::path(out_dir, 'pip_ingestion_pipeline/tb_data/output')
    if (isTRUE(create_dir)) {
      create_dir(glbs)
    }

    # create vintage dir for PC
    # vintage_dir_TB <- check_and_create(dir        = glbs$OUT_DIR_TB,
    #                                    vintage    = vintage,
    #                                    DATE       = glbs$DATE,
    #                                    clean      = clean,
    #                                    verbose    = verbose,
    #                                    create_dir = create_dir)
    #
    #
    # out_path_tb     <- fs::path(glbs$OUT_DIR_TB, vintage_dir_TB)
    #
    # #  Estimations output dir of table baker
    # glbs$OUT_EST_DIR_TB   <- fs::path(out_path_tb, 'estimations')
    #
    #
    # # Table Maker paths
    # glbs$TB_DATA          <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data')
    #
    # glbs$TB_ARROW         <- fs::path(glbs$PIP_PIPE_DIR, 'tb_data/arrow')
    #
    # glbs$CACHE_SVY_DIR_TB <- fs::path(glbs$TB_DATA, 'cache/clean_survey_data')
    #
    # if (isTRUE(create_dir)) {
    #   create_dir(glbs)
    # }

  } # end of vintage not null

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
check_and_create <- function(dir,
                             vintage,
                             DATE = format(Sys.Date(), "%Y%m%d"),
                             clean,
                             verbose,
                             create_dir) {

  # on.exit ------------
  on.exit({

  })

  # pattern that identifies folders
  vintage_pattern <- "\\d{8}_\\d{4}_\\d{1,2}_\\d{1,2}_(PROD|TEST|INT)$"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if character --------

  if (is.character(vintage)) {

    if (length(vintage) == 1 && grepl(vintage_pattern, vintage)) {
      ## if vintage comes in name form ---------

      out_dir <- pip_create_vintage(vintage = vintage)

    } else {

      if ("latest" %in% vintage) {
        # if the latest is wanted

        ## if vintage is to find out ---------

        # computations
        # create vintage dir for PC
        available_paths <- fs::dir_ls(path = dir,
                                      type = "directory")


        # all available directories
        available_dirs  <- gsub("(.+)/([^/]+)", "\\2",  available_paths)

        # directories that meet the criteria
        vintages_av <- stringr::str_extract(available_dirs, vintage_pattern)
        vintages_av <- vintages_av[!is.na(vintages_av)]
        vintages_av <- sort(vintages_av, decreasing = TRUE)

        # Find out the latest vintage available
        if (length(vintages_av) == 0) {
          out_dir <- pip_create_vintage()

        } else {

          vintages_prod <- vintages_av[grepl("PROD$", vintages_av)]
          vintages_prod <- sort(vintages_prod, decreasing = TRUE)

          vintages_test <- vintages_av[grepl("TEST$", vintages_av)]
          vintages_test <- sort(vintages_test, decreasing = TRUE)

          vintages_int  <- vintages_av[grepl("INT$",  vintages_av)]
          vintages_int  <- sort(vintages_int, decreasing = TRUE)


          # find latest depending on selection
          if ("prod" %in% tolower(vintage)) {
            if (length(vintages_prod) == 0) {

              msg     <- c(
                "The combination {.field latest} and {.field prod} is
                not available in the output folder {.file {dir}}",
                "*" = "vintages availables are {.file {vintages_av}}"
                )
              cli::cli_abort(msg,
                            class = "pipload_error"
                            )

            }
            out_dir <- vintages_prod[[1]]

          } else if  ("int" %in% tolower(vintage)) {

            if (length(vintages_int) == 0) {

              msg     <- c(
                "The combination {.field latest} and {.field int} is
                not available in the output folder {.file {dir}}",
                "*" = "vintages availables are {.file {vintages_av}}"
              )
              cli::cli_abort(msg,
                             class = "pipload_error"
              )

            }

            out_dir <- vintages_int[[1]]

          } else if  ("test" %in% tolower(vintage)) {

            if (length(vintages_test) == 0) {

              msg     <- c(
                "The combination {.field latest} and {.field test} is
                not available in the output folder {.file {dir}}",
                "*" = "vintages availables are {.file {vintages_av}}"
              )
              cli::cli_abort(msg,
                             class = "pipload_error"
              )

            }

            out_dir <- vintages_test[[1]]

          } else {
            out_dir <- vintages_av[[1]]
          }

        } # end of finding latest


      } else if ("new" %in% vintage) {
        # If new vintage is wanted

        if ("prod" %in% tolower(vintage)) {
          identity <- "PROD"
        } else if  ("test" %in% tolower(vintage)) {
          identity <- "TEST"
        } else if  ("int" %in% tolower(vintage)) {
          identity <- "INT"
        } else {
          identity <- "PROD"
        }

        out_dir <- pip_create_vintage(vintage = list(release  = DATE,
                                                     identity = identity))

      } else {
        # this should never happen
        out_dir <- NULL
      }
    } # end of if is.character()

  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If list --------
    out_dir <- pip_create_vintage(vintage = vintage)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # folder creation   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  out_path <- fs::path(dir, out_dir)

  if (create_dir == TRUE & !fs::dir_exists(out_path)) {

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
  return(out_dir)

}




