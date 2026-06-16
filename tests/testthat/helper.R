# Helpers available to all test files (sourced by testthat before any tests run).

#' Returns TRUE when PIP_ROOT_DIR is set and the auxiliary data directory exists.
has_pip_data <- function() {
  root_dir <- Sys.getenv("PIP_ROOT_DIR", "")
  nzchar(root_dir) && dir.exists(file.path(root_dir, "_aux"))
}
