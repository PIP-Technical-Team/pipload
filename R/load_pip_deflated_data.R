#' Load and deflate a PIP survey in a single call
#'
#' Convenience wrapper that chains [load_pip_data()] and
#' `pipdata::pd_deflation()`. All filtering arguments are forwarded to
#' [load_pip_data()]; the resulting survey data.table is then passed to
#' `pipdata::pd_deflation()`.
#'
#' @inheritParams load_pip_data
#' @param cpi data.table or NULL. CPI table to override the default resolved
#'   by `pd_deflation()`. Passed directly to `pipdata::pd_deflation(cpi = )`.
#' @param ppp data.table or NULL. PPP table to override the default. Passed
#'   directly to `pipdata::pd_deflation(ppp = )`.
#' @param pop data.table or NULL. Population table to override the default.
#'   Passed directly to `pipdata::pd_deflation(pop = )`.
#'
#' @return A data.table with the same structure as [load_pip_data()] output,
#'   with welfare variables deflated to real values. The S3 class (`pipmd` or
#'   `pipgd`) is preserved.
#'
#' @seealso [load_pip_data()]
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
# NOTE: pipdata is a soft dependency (Suggests). Do not add it to Imports —
# circular dependency: pipdata imports pipload.
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

  # Validate aux override tables when supplied.
  if (!is.null(cpi) && !data.table::is.data.table(cpi)) {
    cli::cli_abort(
      "{.arg cpi} must be a data.table or NULL, not {.obj_type_friendly {cpi}}."
    )
  }
  if (!is.null(ppp) && !data.table::is.data.table(ppp)) {
    cli::cli_abort(
      "{.arg ppp} must be a data.table or NULL, not {.obj_type_friendly {ppp}}."
    )
  }
  if (!is.null(pop) && !data.table::is.data.table(pop)) {
    cli::cli_abort(
      "{.arg pop} must be a data.table or NULL, not {.obj_type_friendly {pop}}."
    )
  }

  # Warn when id_name is supplied alongside filter args — the filter args
  # are silently discarded and this can return unexpected data.
  filter_args <- list(
    country_code = country_code,
    surveyid_year = surveyid_year,
    survey_acronym = survey_acronym,
    welfare_type = welfare_type,
    module = module
  )
  if (!is.null(id_name) && !all(vapply(filter_args, is.null, logical(1L)))) {
    active_filters <- names(Filter(Negate(is.null), filter_args))
    cli::cli_warn(
      c(
        "{.arg id_name} was supplied alongside filter argument(s): {.field {active_filters}}.",
        "i" = "Filter argument(s) are ignored; {.arg id_name} = {.val {id_name}} takes precedence."
      )
    )
  }

  # Resolve pip_id (no file extension) for pd_deflation.
  # pd_deflation() needs pip_id explicitly when dt is provided without
  # pip_id attributes set — which is the case for data loaded via load_pip_data().
  if (!is.null(id_name)) {
    if (length(id_name) != 1L) {
      cli::cli_abort(
        "{.arg id_name} must be a single string, not a length-{length(id_name)} vector."
      )
    }
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
    if (length(pip_id) == 0L) {
      cli::cli_abort(
        c(
          "No matching survey found.",
          "i" = "Check {.arg country_code}, {.arg surveyid_year}, and other filter arguments."
        )
      )
    }
    if (length(pip_id) > 1L) {
      cli::cli_abort(
        c(
          "More than one survey matched the filter arguments ({length(pip_id)} found).",
          "i" = "Matching IDs: {.val {pip_id}}.",
          "i" = "Refine filters or supply {.arg id_name} directly."
        )
      )
    }
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
    survey <- assign_pipclass_from_id(survey, pip_id)
  }

  return(pipdata::pd_deflation(
    dt = survey,
    pip_id = pip_id,
    cpi = cpi,
    ppp = ppp,
    pop = pop
  ))
}
