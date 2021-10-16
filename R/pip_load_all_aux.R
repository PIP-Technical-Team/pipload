#' Load all auxiliary files in memory
#'
#' @param aux character: auxiliary files to load. Default is c("cpi", "ppp",
#'   "pfw", "pop", "gdm", "gdp", "pce")
#' @param aux_names character of the same length of `aux`. Names of objects to
#'   be loaded. default is `aux`
#' @param envir environment where the data frame will be allocated. Default is
#'   `globalenv()`
#' @inheritParams pip_load_aux
#'
#' @return invisible TRUE
#' @export
pip_load_all_aux <- function(aux               = c("cpi", "ppp", "pfw", "pop", "gdm"),
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

  names(aux) <- aux_names
  purrr::iwalk(.x = aux,
               .f = ~{
                 a <- pip_load_aux(measure = .x,
                                   root_dir          = root_dir        ,
                                   maindir           = maindir         ,
                                   version           = version         ,
                                   file_to_load      = file_to_load    ,
                                   apply_label       = apply_label     ,
                                   verbose           = verbose         ,
                                   preferred_format  = preferred_format)
                 assign(.y, a, envir = envir)
               })
  return(invisible(TRUE))
}

