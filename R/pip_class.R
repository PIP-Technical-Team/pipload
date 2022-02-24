#' Add pipmd class for microdata
#'
#' @param x data frame
#'
#' @return data frame with new class pipmd
#' @export
as_pipmd <- function(x) {
  x <- as.data.table(x)
  class(x) <- pipmd_class
  x
}

#' Add pipgd class for group data
#'
#' @param x data frame
#'
#' @return data frame with new class pipgd
#' @export
as_pipgd <- function(x) {
  x <- as.data.table(x)
  class(x) <- pipgd_class
  x
}

#' Add pipid class for imputed data
#'
#' @param x data frame
#'
#' @return data frame with new class pipid
#' @export
as_pipid <- function(x) {
  x <- as.data.table(x)
  class(x) <- pipid_class
  x
}


pipmd_class <- c("pipmd", "data.table", "data.frame")
pipgd_class <- c("pipgd", "data.table", "data.frame")
pipid_class <- c("pipid", "pipmd", "data.table", "data.frame")

