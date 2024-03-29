% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pip_find_dlw.R
\name{pip_find_dlw}
\alias{pip_find_dlw}
\title{Find surveys available for DatalibWeb}
\usage{
pip_find_dlw(
  country = NULL,
  year = NULL,
  survey_acronym = NULL,
  vermast = NULL,
  veralt = NULL,
  module = NULL,
  tool = NULL,
  condition = NULL,
  root_dir = Sys.getenv("PIP_ROOT_DIR"),
  dlw_dir = pip_create_globals(root_dir)$DLW_RAW_DIR,
  filter_to_pc = FALSE,
  filter_to_tb = FALSE,
  verbose = getOption("pipload.verbose")
)
}
\arguments{
\item{country}{character: vector of ISO3 country codes.}

\item{year}{numeric: survey year}

\item{survey_acronym}{character: Survey acronym}

\item{vermast}{character: Master version in the form v## or ##}

\item{veralt}{character: Alternative version in the form v## or ##}

\item{module}{character: Source of data. It could be \code{GPWG}, \code{HIST}, \code{GROUP},
\code{synth}, \code{BIN}, and \code{ALL}. The latter is used only in Table Maker.}

\item{tool}{character: PIP tool in which data will be used. It could be
\code{PC} for Poverty Calculator or \code{TB} for Table Maker. Others will be added}

\item{condition}{character: logical condition that applies to all surveys.
For example, "year > 2012". Make sure the condition uses the names of the
variables in \code{pip_load_dlw_inventory()}: orig, filename, country_code, year,
survey_acronym, vermast, veralt, collection, module, tool, and source.
Can't be used with arguments \code{country}, \code{year},
\code{survey_acronym} , \code{vermast}, \code{veralt}, \code{module} or \code{tool}.}

\item{root_dir}{character: root directory of the PIP data}

\item{dlw_dir}{character: Main directory}

\item{filter_to_pc}{logical: If TRUE filter most recent data to be included
in the Poverty Calculator. Default if FALSE}

\item{filter_to_tb}{logical: If TRUE filter most recent data to be included
in the Table Maker. Default if FALSE}

\item{verbose}{logical: whether to display message. Default is TRUE}
}
\value{
data.frame: list of filenames to be loaded with pcn_load()
}
\description{
Find surveys available for DatalibWeb
}
\examples{
# all years for one country
pip_find_dlw(country = "ARG")

#' # all years for more than one country
pip_find_dlw(country = c("COL", "ARG"))

# specific years for one country
pip_find_dlw(
            country = "COL",
            year = c(2010, 2012)
)

# country FHF does not exist so it will be part of `fail` output (No error)
pip_find_dlw(
       country = c("ARG", "FHF"),
       year = 2010
)

# Load a different module (e.g., GPWG)
pip_find_dlw(country = "PRY",
             year = 2010,
             module = "GPWG",
             tool = "PC")

# Load different modules
pip_find_dlw(country = "COL",
             module = "HIST")
\dontrun{
# more than two years for more than one country (only firt year will be used)
pip_find_dlw(
       country = c("COL", "ARG"),
       year = c(2010, 2012)
)

# all countries and years
pip_find_dlw()
}
}
