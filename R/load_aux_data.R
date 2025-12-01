#' Load auxiliary data from the PIP aux_data folder
#'
#' Loads auxiliary datasets stored in the `aux_data` folder of the active PIP
#' working environment (as defined by setup_working_release()).
#'
#' @param measure Character. The name of the auxiliary dataset to load
#'   (e.g., "cpi", "ppp", "pop").
#' @param version Optional version string or negative index passed to pip_read().
#' @param hash Optional artifact hash passed to pip_read().
#' @param ppp_defaults Logical. If TRUE, keeps only PPP default years (PPP only).
#' @param verbose Logical. Whether to show informational messages.
#'
#' @return A data.table containing the auxiliary data.
#' @export
#' Load auxiliary data stored in aux_repository via {stamp}
#'
#' @param measure character: name of the auxiliary dataset, e.g. "cpi", "ppp"
#' @param version integer or character: version to load (see stamp::st_load)
#' @param ppp_defaults logical: If TRUE and measure == "ppp", return only default PPP years
#' @param hash ignored — backward compatibility
#' @param verbose logical
#'
#' @return data.table
#' @export
load_aux_data <- function(measure  = NULL,
                          version  = NULL,
                          ppp_defaults = TRUE,
                          verbose  = getOption("pipload.verbose")) {

  stopifnot(!is.null(measure))

  # Ensure working release is loaded
  pipfun::get_wrk_release(verbose = FALSE)

  # Get aux_data folder
  pip_folders <- pipfun::get_pip_folders()
  aux_path <- pip_folders$aux_data

  # Artifact ID corresponds to the measure name
  artifact_id <- measure

  # Load data via pip_read
  dt <- pip_read(
    id  = artifact_id,
    dir = aux_path,
    version = version,
    verbose = verbose
  )

  # Keep only default PPP years if needed
  if (measure == "ppp" && ppp_defaults) {
    dt <- dt[ppp_default_by_year == TRUE]
  }

  dt
}

