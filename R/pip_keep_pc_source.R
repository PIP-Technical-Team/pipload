#' Make sure we keep only one module per survey ID when loading.
#'
#' @param df dataframe from `pip_load_inventory()`
#'
#' @return dataframe
pip_keep_pc_source <- function(df){

  source_order <- c("GPWG", "HIST", "BIN", "GROUP", "synth")
  source_avail <- df[, unique(source)]

  out         <- FALSE
  i           <- 0
  maxi        <- length(source_order)
  source_keep <- NULL
  while(out == FALSE && i <= maxi) {

    i <- i + 1
    if (source_order[i] %in% source_avail) {
      source_keep <- source_order[i]
      out         <- TRUE
    }

  }

  if (!is.null(source_keep)) {
    df <- df[source == (source_keep)]
  }
  return(df)
}

