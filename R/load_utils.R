#' Find data available in PIP boards.
#'
#' This is a lower-level function and should not be used with any interactively.
#'
#' @param board board from `pipfun::get_pins_boards(board = "dlw_data")`
#' @inheritParams load_dlw_data
#' @param ... Just the following: country_code, year, module, survey, vermast,
#'   veralt, collection, module. They work exactly the same as the ones in
#'   [load_dlw_data]
#'
#' @returns data from with filter data
#' @keywords internal
find_data <- function(board,
                      latest_version,
                      latest_year,
                      verbose,
                      ...) {
  # Capture ... arguments as a list
  dots <- list(...)
  # Combine country and ... into a single list of arguments
  args <- lapply(dots, \(.) {
    if (is.character(.)) {
      toupper(.)
    } else {
      .
    }
  })

  # get names of arguments that are not null
  args_info <- Filter(Negate(is.null), args) |>
    names()

  cnds <- lapply(args_info, \(.) {
    paste(simpleCap(.), ., sep = " %in% ")
  }) |>
    # append them together
    paste(collapse = " & ") |>
    # convert to expression
    rlang::parse_expr()

  bl <- pins::pin_list(board)

  # Build catalog
  ctl <- data.table(pin_name = bl)

  vars <- c(
    "Country_code",
    "Year",
    "survey",
    "Vermast",
    "M",
    "Veralt",
    "A",
    "Collection",
    "Module",
    "ext"
  )

  ctl[, (vars) := tstrsplit(pin_name, split = "_|[.]", fill = NA)
  ][,
    c("M", "A") := NULL]


  ctl <- ctl[rlang::eval_tidy(cnds, data = args)] |>
    unique()

  if (latest_year == TRUE && !("year" %in% args_info)) {
    ctl <- ctl[,
               #  for each collectiona and module, the row(s) with the maximum Year
               .SD[Year == max(Year, na.rm = TRUE)],
               by = .(Collection, Module)
    ]
  }

  if (!("vermast" %in% args_info) &&
      !("veralt" %in% args_info) &&
      latest_version == TRUE) {
    ctl <- ctl[ ,
                #  for each year, the row(s) with the maximum Vermast.
                .SD[Vermast == max(Vermast, na.rm = TRUE)],
                by = .(Year, Collection, Module)
    ][,
      #It should return only one row per year (even if there are ties)
      .SD[Veralt == max(Veralt, na.rm = TRUE)],
      by = .(Year, Collection, Module)]
  }

  return(ctl)
}



#' check that dlw pin_name is correct
#'
#' @param pin_name pin name of dlw data
#'
#' @returns character with pin_name
#' @keywords internal
#'
#' @examples
#' check_data_pin_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs")
#' check_data_pin_name("HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG")
check_data_pin_name <- \(pin_name) {
  pin_name <- pin_name |>
    fs::path_ext_remove() |>
    fs::path(ext = "qs")

  ptt <- "^[A-Za-z]+_[0-9]{4}_[^_]+_[Vv][0-9]{2}_M_[Vv][0-9]{2}_A_[^_]+_[^_]+\\.[A-Za-z]+$"

  if (!grepl(ptt, pin_name)) {
    cli::cli_abort(c(x = "Wrong {.arg pin_name} specification",
                     i = "it should follow the pattern {.field {ptt}}",
                     i = "like in {.file HRV_2011_EU-SILC_V01_M_V04_A_GMD_GPWG.qs}"))
  }
  invisible(pin_name)
}
