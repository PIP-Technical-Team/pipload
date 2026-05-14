#' Load and deflate a PIP survey in a single call
#'
#' Convenience wrapper that chains [load_pip_data()] and
#' `pipdata::pd_deflation()`. All filtering arguments are forwarded to
#' [load_pip_data()]; the resulting survey data.table is then passed to
#' `pipdata::pd_deflation()`.
#'
#' `pipdata` is a **soft dependency** (`Suggests`). The function will abort
#' with an informative error if `pipdata` is not installed. Do not add
#' `pipdata` to `Imports` — that would create a circular dependency since
#' `pipdata` imports `pipload`.
#'
#' @inheritParams load_pip_data
#' @param cpi data.table or NULL. CPI table to override the default resolved
#'   by `pd_deflation()`. Passed directly to `pipdata::pd_deflation(cpi = )`.
#' @param ppp data.table or NULL. PPP table to override the default. Passed
#'   directly to `pipdata::pd_deflation(ppp = )`.
#' @param pop data.table or NULL. Population table to override the default.
#'   Passed directly to `pipdata::pd_deflation(pop = )`.
#'
#' @return A data.table — the deflated survey returned by
#'   `pipdata::pd_deflation()`.
#'
#' @seealso [load_pip_data()]
#' @family load_pip_data
#' @export
#'
#' @examples
#' \dontrun{
#' lr <- pipfun::get_latest_pip_release()
#' pipfun::setup_working_release(
#'   release  = lr$release,
#'   identity = lr$identity,
#'   verbose  = FALSE
#' )
#'
#' # Using id_name
#' load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")
#'
#' # Using country + year filters
#' load_pip_deflated_data(country_code = "PRY", surveyid_year = 2018)
#' }
load_pip_deflated_data <- function(
  country_code = NULL,
  surveyid_year = NULL,
  survey_acronym = NULL,
  welfare_type = NULL,
  module = NULL,
  id_name = NULL,
  vermast = NULL,
  veralt = NULL,
  collection = NULL,
  latest_version = TRUE,
  latest_year = FALSE,
  where = c("release", "master"),
  version = NULL,
  verbose = getOption("pipload.verbose"),
  format = "qs2",
  cpi = NULL,
  ppp = NULL,
  pop = NULL
) {
  rlang::check_installed(
    "pipdata",
    reason = "to apply deflation via `pd_deflation()`"
  )

  # Resolve pip_id (no file extension) for pd_deflation.
  # pd_deflation() needs pip_id explicitly when dt is provided without
  # pip_id attributes set — which is the case for data loaded via load_pip_data().
  if (!is.null(id_name)) {
    pip_id <- fs::path_ext_remove(id_name) |> toupper()
  } else {
    inv <- find_pip_data(
      country_code = country_code,
      surveyid_year = surveyid_year,
      survey_acronym = survey_acronym,
      welfare_type = welfare_type,
      module = module,
      vermast = vermast,
      veralt = veralt,
      collection = collection,
      latest_version = latest_version,
      latest_year = latest_year,
      where = where,
      verbose = verbose
    )
    pip_id <- inv[, pip_id]
  }

  survey <- load_pip_data(
    id_name = pip_id,
    where = where,
    version = version,
    verbose = verbose,
    format = format,
    metadata = FALSE
  )

  # Assign the pip S3 class required for pd_deflation()'s S3 dispatch.
  # Mirrors the same logic pd_deflation() applies when loading data itself:
  # use assign_pipclass() when the module column exists (legacy pipeline),
  # otherwise derive the module from the pip_id suffix (new pipeline).
  if ("module" %in% names(survey)) {
    survey <- assign_pipclass(survey)
  } else {
    pip_module <- utils::tail(strsplit(pip_id, "_", fixed = TRUE)[[1L]], 1L)
    survey <- if (grepl("GROUP", pip_module, ignore.case = TRUE)) {
      as_pipgd(survey)
    } else {
      as_pipmd(survey)
    }
  }

  pipdata::pd_deflation(
    dt = survey,
    pip_id = pip_id,
    cpi = cpi,
    ppp = ppp,
    pop = pop
  )
}
