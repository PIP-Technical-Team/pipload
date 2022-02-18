#' Load all auxiliary files in memory
#'
#' @param replace logical or NULL. Whether to replace objects in `envir`
#' @param aux character: auxiliary files to load. Default is c("cpi", "ppp",
#'   "pfw", "pop", "gdm", "gdp", "pce"). If "all", all auxiliary frame will be
#'   loaded in memory
#' @param aux_names character of the same length of `aux`. Names of objects to
#'   be loaded. default is `aux`
#' @param envir environment where the data frame will be allocated. Default is
#'   `globalenv()`
#' @inheritParams pip_load_aux
#'
#' @return invisible TRUE
#' @export
pip_load_all_aux <- function(replace           = NULL,
                             aux               = c("cpi", "ppp", "pfw", "pop", "gdm"),
                             aux_names         = aux,
                             envir             = globalenv(),
                             root_dir          = Sys.getenv("PIP_ROOT_DIR"),
                             maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR,
                             version           = NULL,
                             file_to_load      = NULL,
                             apply_label       = TRUE,
                             verbose           = FALSE,
                             preferred_format  = NULL
                             ) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check arguments --------
  if (length(aux) != length(aux_names)) {
    cli::cli_abort("{.code length(aux)} and {.code length(aux_names)} must
                   be the same",
                   wrap = TRUE)
  }

  auxdir <- fs::path(maindir, "_aux/")

  aux_files <- fs::dir_ls(auxdir,
                          recurse = FALSE,
                          type = "directory")

  aux_indicators <- stringr::str_extract(aux_files, "[^/]+$")
  aux_indicators   <-  tolower(unique(aux_indicators))

  if (tolower(aux) == "all") {
    aux <- aux_indicators
  }
  not_av <- !(aux  %in% aux_indicators)

  if (tolower(aux_names) == "all") {
    aux_names <- aux_indicators
  }

  if (any(not_av)) {
    cli::cli_abort("auxiliary file {.field {aux[not_av]}} is not available")
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check names in desired envir --------

  obj_env <- ls(envir = envir)
  names_in_env <- aux_names  %in% obj_env

  if (any(names_in_env)) {
    if (is.null(replace)) {
      cli::cli_alert_danger("{.field {aux_names[names_in_env]}} object{?s}
                            {?is/are} in used in {.field envir}.",
                            wrap = TRUE)
      replace <- usethis::ui_yeah("Do you want to replace them?")
    }


  }

  if (isFALSE(replace)) {
    cli::cli_alert_warning("No object will be replace in {.field envir}")
    return(invisible(FALSE))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Load and assign --------

  names(aux) <- aux_names

  purrr::iwalk(.x = aux,
               .f = ~{
                 tryCatch(
                   expr = {
                     # Your code...
                     a <- pip_load_aux(measure = .x,
                                       root_dir          = root_dir        ,
                                       maindir           = maindir         ,
                                       version           = version         ,
                                       file_to_load      = file_to_load    ,
                                       apply_label       = apply_label     ,
                                       verbose           = verbose         ,
                                       preferred_format  = preferred_format)
                     assign(.y, a, envir = envir)
                   }, # end of expr section

                   error = function(e) {
                     cli::cli_alert_danger("error loading {.x}")
                   }, # end of error section

                   warning = function(w) {
                     cli::cli_alert_warning("warning loading {.x}")
                   }

                 ) # End of trycatch

               })

  return(invisible(TRUE))
}

