#' Add pipmd class
#'
#' @param x data frame
#'
#' @return data frame with new class pipmd
#' @export
as_pipmd <- function(x) {
  class(x) <- pipmd_class
  x
}

#' Add pipgd class
#'
#' @param x data frame
#'
#' @return data frame with new class pipgd
#' @export
as_pipgd <- function(x) {
  class(x) <- pipmgd_class
  x
}

#' Add pipid class
#'
#' @param x data frame
#'
#' @return data frame with new class pipid
#' @export
as_pipid <- function(x) {

  class(x) <- pipid_class
  x

}

pipmd_class <- c("pipdm", "data.table", "data.frame")
pipgd_class <- c("pipgd", "data.table", "data.frame")
pipid_class <- c("pipid", "data.table", "data.frame")

