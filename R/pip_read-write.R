#' Read and write objects in the PIP ecosystem using `pins`
#'
#' `pip_write()` and `pip_read()` are just wrappers of `pins_write()` and
#' `pins_read()` with predefined arguments
#'
#' @inheritParams pins::pin_write
#' @inheritDotParams pins::pin_write  title description metadata tags urls
#'
#' @returns  fully qualified name of the new pin, invisibly.
#' @export
#'
#' @examples
pip_write <- function(board,
                      x,
                      name = NULL,
                      force_identical_write = FALSE,
                      ...) {

  pins::pin_write(board                 = board,
                  x                     = x,
                  name                  = name,
                  force_identical_write = force_identical_write,
                  type                  = "qs",
                  versioned             = TRUE,
                  ...)


}
