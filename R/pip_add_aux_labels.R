#' Title
#'
#' @param x Data frame to be labeled.
#' @param measure type of data frame, e.g., "cpi" or "PPP".
#'
#' @return
#' @export
pip_add_aux_labels <- function(x, measure) {

  if (measure == "cpi") {

    # Label variables
    #attr(x$survey_year, "label") <- "Proportion of first year of survey"
    attr(x$cpi_domain,     "label") <- "CPI domain to join with microdata"
    attr(x$cpi_data_level, "label") <- "Values to use as keys to join with cpi_domain_var"
    #attr(x$cpi_year,  "label") <- "Year of survey ID"
    attr(x$ccf,            "label") <- "Currency conversion factor"
    attr(x$cpi,            "label") <- "Consumer Price Index (Based on 2011)."

  }  else if (measure == "ppp") {

    ppp_year <- unique(x[x$ppp_default == TRUE, "ppp_year"])

    # Label variables
    attr(x$ppp_domain,     "label")     <- "PPP domain to join with microdata"
    attr(x$ppp_data_level, "label")     <- "Values to use as keys to join with ppp_domain_var"
    attr(x$ppp,            "label")     <- paste0("Purchasing Power Parity (",
                                             ppp_year,"2011 ICP round)")
    attr(x$ppp_year,           "label") <- "ICP round year "
    attr(x$release_version,    "label") <- "Release version of ICP round"
    attr(x$adaptation_version, "label") <- "Adaptation version of release"
    attr(x$ppp_default,        "label") <- "PPP version used by default"


  } else if (measure == "maddison") {

    # Label Variables
    #attr(x$country_name, "label")  <- "Country name"
    attr(x$country_code, "label")  <- "Country code"
    attr(x$year,         "label")  <- "Year"
    #attr(x$cgdppc,       "label")  <- "Real GDP per capita in 2011US$, multiple benchmarks"
    attr(x$mpd_gdp,      "label")  <- "GDP per capita in 2011US$, 2011 benchmark (Maddison)"
    # attr(x$pop,          "label")  <- "Population, mid-year (thousands)"
    #attr(x$i_cig,        "label")  <- "0/1/2: observation is extrapolated (0), benchmark (1), or interpolated (2)"
    #attr(x$i_bm,         "label")  <- "1-5: type of benchmark estimate, see note i_bm"

  } else if (measure == "gdp") {
    # Label Variables
    attr(x$country_code,   "label")  <- "Country code"
    attr(x$year,           "label")  <- "Year"
    attr(x$gdp_data_level, "label")  <- "Values to use as keys to join with gdp_domain_var"
    attr(x$gdp,            "label")  <- "GDP per capita (constant 2010 US$)"
    attr(x$gdp_domain,     "label")  <- "GDP domain to join with microdata"

  } else if (measure == "pce") {

    attr(x$country_code,   "label")  <- "Country code"
    attr(x$year,           "label")  <- "Year"
    attr(x$pce_data_level, "label")  <- "Values to use as keys to join with \n pce_domain_var in microdata"
    attr(x$pce,            "label")  <- "Households and NPISHs Final consumption expenditure per capita (constant 2010 US$)"
    attr(x$pce_domain,     "label")  <- "PCE domain to join with microdata"


  } else if (measure == "pop") {

    attr(x$country_code,   "label")  <- "Country code"
    attr(x$year,           "label")  <- "Year"
    attr(x$pop_data_level, "label")  <- "Values to use as keys to join with pop_domain_var"
    attr(x$pop,            "label")  <- "Population"
    attr(x$pop_domain,     "label")  <- "Population domain to join with microdata"

  } else {
    cli::cli_alert_info(paste0("no labels available for measure {.field {measure}}"))
  }

  return(x)
}


