pipuax_default_options <- list(
  pipload.verbose = TRUE,
  pipload.working_dir = "PIP_ingestion_pipeline_v2"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))
  if (any(toset)) options(pipuax_default_options[toset])

  invisible()
}


# set important values in piploadenv ----------

## DLW -------------
set_in_piploadenv(key = "dlw_name_pattern",
                  value = "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_[Mm]_[Vv][0-9]{2}_[Aa]_[^_]+_[^_]+\\.[A-Za-z0-9]+$")

set_in_piploadenv(key = "dlw_id_vars",
                  value = c(
                    "Country_code",
                    "Surveyid_year",
                    "Survey_acronym",
                    "Vermast",
                    "M",
                    "Veralt",
                    "A",
                    "Collection",
                    "Module",
                    "ext"
                  ))



## PIP ------------
set_in_piploadenv(key = "pip_name_pattern",
                  value = "^[A-Za-z]+_[0-9]{4}_[^_]+_(INC|CON)_[^_]+\\.?[A-Za-z]*$")

set_in_piploadenv(key = "pip_id_vars",
                  value = c(
                    "Country_code",
                    "Surveyid_year",
                    "Survey_acronym",
                    "Welfare_type",
                    "Module"
                  ))

