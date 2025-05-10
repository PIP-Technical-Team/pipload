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
