#' Load any auxiliary data
#'
#' @param msrdir character: measure (CPI) directory. created on `pip_prices()`.
#' @param measure character: Measure to be used. e.g., "cpi" or "ppp".
#'
#' @return
#' @export
#'
#' @examples
#' # Load CPI
#' pip_load_aux("cpi")
#'
#' # load PPP
#' pip_load_aux("ppp")
#'
#' # Load GDP
#' pip_load_aux("gdp")
pip_load_aux <- function(measure = NULL,
                         msrdir = paste0(getOption("pip.maindir"),
                                         "_aux/",
                                         measure, "/")
                         ){

  if (is.null(measure)) {

    rlang::abort(c(
                  "`measure` must be defined, as it does not have default value",
                  i = "make sure `measure` is not NULL."
                  ),
                  class = "pipaux_error"
                  )


  }

  # check file exists
  if(file.exists(paste0(msrdir, measure ,".fst"))){

    df <- fst::read_fst(paste0(msrdir, measure ,".fst"))

  } else {
    msg <- paste("file `", measure,".fst` does not exist.")
    rlang::abort(c(
      msg,
      i = "check your connection or data availability"
    ),
    class = "pipaux_error"
    )

  }
  df <- pip_add_aux_labels(df,
                       measure = measure)
  return(df)
}
