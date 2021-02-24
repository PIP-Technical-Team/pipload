pipuax_default_options <- list(
  pip.dlwdir  = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
  pip.maindir = "//w1wbgencifs01/pip/PIP-Data/_testing/pipdp_testing/"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))
  if (any(toset)) options(pipuax_default_options[toset])

  invisible()
}
