#' Load auxiliary data from PIP aux_data folder
#'
#' @param measure Character. Name of the measure to load, e.g., "cpi" or "ppp".
#' @param version Version selector. See [pip_read()] for semantics.
#' @param format Character. Format in which the artifact was saved (default "qs2").
#' @param verbose Logical. Whether to print loading messages.
#' @param ppp_defaults Logical. If TRUE and measure == "ppp", only default PPP years are returned.
#' @return data.table or object saved as artifact.
#' @export
load_aux_data <- function(
    measure,
    version       = NULL,
    format        = "qs2",
    verbose       = getOption("pipload.verbose", TRUE),
    ppp_defaults  = TRUE
) {

  # Defensive checks
  if (missing(measure)) {
    cli::cli_abort("You must provide a measure name, e.g., {.val 'cpi'} or {.val 'ppp'}.")
  }

  # Ensure working release is loaded
  pipfun::get_wrk_release(verbose = FALSE)

  # Get PIP folder paths
  pip_folders <- pipfun::get_pip_folders("aux_data")

  if (is.null(pip_folders$aux_data)) {
    cli::cli_abort("Auxiliary data folder not set in .pipenv. Run setup_working_release() first.")
  }

  # Construct full path to the measure artifact
  artifact_dir <- fs::path(pip_folders, measure)

  if (!fs::dir_exists(artifact_dir)) {
    cli::cli_abort("Artifact folder {.path {artifact_dir}} does not exist.")
  }

  if (verbose) {
    cli::cli_alert_info("Loading auxiliary data {.field {measure}} from {.path {artifact_dir}}")
  }

  # Read the artifact using pip_read
  dt <- pip_read(
    id      = measure,
    dir     = artifact_dir,
    version = version,
    format  = format,
    verbose = verbose
  )

  # Optionally filter PPP default years
  if (measure == "ppp" && isTRUE(ppp_defaults)) {
    dt <- dt[ppp_default_by_year == TRUE]
  }

  dt
}
