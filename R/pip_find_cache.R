#' Find cache data based on patterns
#'
#' @inheritParams pip_load_cache
#'
#' @return
#' @export
#'
#' @examples
pip_find_cache <- function(country          = NULL,
                           year             = NULL,
                           survey_acronym   = NULL,
                           data_level       = NULL,
                           welfare_type     = NULL,
                           source           = NULL,
                           tool             = c("PC", "TM"),
                           pipedir          = getOption("pip.pipedir")
                           )  {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #                   Check parameters   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # right arguments
  tool <- match.arg(tool)
  if (!is.null(welfare_type)) {
    wt_ok <- any(toupper(welfare_type) %in% c("CON", "INC"))

    if (isFALSE(wt_ok)) {
      cli::cli_alert_danger("{.code welfare_type} must be either {.field CON} or
                            {.field INC}, not {.field {welfare_type}}", wrap = TRUE)
      msg     <- "wrong specification in `welfare_type`"
      rlang::abort(c(msg),class = "pipload_error")
    }

  }

  ri <- pip_load_cache_inventory(pipedir = pipedir,
                                 tool    = tool)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##           Create regex --------

  args <- c("country",
            "year",
            "survey_acronym",
            "data_level",
            "welfare_type",
            "source")


  Tcountry          = "[A-Z]{3}"
  Tyear             = "[0-9]{4}"
  Tsurvey_acronym   = "[0-9A-Z\\-]+"
  Tdata_level       = "D[123]"
  Twelfare_type     = "(CON|INC)"
  Tsource           = "[A-Z]+$"

  pattern <- vector(length = length(args))
  for (i in seq_along(args)) {
    x  <- args[i]
    tx <- paste0("T", x)

    if (is.null(get(x))) {

      pattern[i] <- get(tx)

    } else {

      y <-  paste0("(", paste(get(x), collapse = "|"), ")")
      pattern[i] <- y
    }

  } # end of loop

  pattern <- paste(pattern, collapse = "_")
  pattern <- paste0("^", pattern)

  ri <- ri[grepl(pattern, cache_id),
           cache_id]

  return(ri)

}
