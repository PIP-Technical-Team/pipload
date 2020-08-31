#' pip_datafind
#' Find surveys available for PIP
#'
#' @param country character: ISO3 country code
#' @param year    numeric: survey year
#' @param survey  character: Survey acronym
#' @param vermast character: Master version in the form v## or ##
#' @param veralt  character: Alternative version in the form v## or ##
#' @param type    character: Type of alternative version
#' @param maindir character: Main directory
#' @param drive   character: mapped drive
#'
#' @return data.frame: list of filenames to be loaded with pcn_load()
#' @import data.table
#' @export
#'
#' @examples
#' # all years for one country
#' pip_datafind(country = "ARG")
#'
#' #' # all years for more than one country
#' pip_datafind(country = c("COL", "ARG"))
#'
#' # specific years for one country
#' pip_datafind(
#'             country = "COL",
#'             year = c(2010, 2012)
#' )
#'
#' # country FHF does not exist so it will be part of `fail` output (No error)
#' pip_datafind(
#'        country = c("ARG", "FHF"),
#'        year = 2010
#' )
#'
#' # Load a different module (e.g., GPWG)
#' pip_datafind(country = "PRY",
#'              year = 2010,
#'              module = "GPWG")
#' \dontrun{
#' # more than two years for more than one country (only firt year will be used)
#' pip_datafind(
#'        country = c("COL", "ARG"),
#'        year = c(2010, 2012)
#' )
#'
#' # all countries and years
#' pip_datafind()
#' }

pip_datafind <- function(country          = NULL,
                         year             = NULL,
                         survey           = NA,
                         vermast          = NA,
                         veralt           = NA,
                         characteristics  = TRUE,
                         maindir          = ":/01.PovcalNet/01.Vintage_control",
                         drive            = "p") {
  #--------- Initial conditions

  # drive and main dir

  maindir <- paste0(drive, maindir)

  if (!dir.exists(maindir)) {
    maindir <- "//wbntpcifs/povcalnet/01.PovcalNet/01.Vintage_control"
  }
  if (!dir.exists(maindir)) {
    st_msg <- paste0("main directory `",
                     maindir,
                     "` not reachable. Check connection")
    stop(st_msg)
  }


  #----------------------------------------------------------
  #   Country condition
  #----------------------------------------------------------

  if (length(country) == 0) {
    argum <- c(year, survey, vermast, veralt)
    if (sum(is.na(argum)) != length(argum)) {
      warning(
        "if `country` is NA, arguments `year`, `survey`,
              `vermast`, and `veralt` should be NA as well"
      )
    }

    countries <- dir(maindir)
    countries <- countries[!grepl("^_", countries)]  # remove _aux folder

  } else { # country is selected
    lyear      <- length(year)
    lcountry   <-  length(country)

    if ( lyear != 0 ) {       # if year is selected along with country
      if (lcountry != 1) {  # if more than one country selected
        if ( lyear != 1) {
          warning(paste0("`length(country)` is greater than 1 (==",
                         lcountry, "), So the first value of `year` (",
                         year[[1]], ") wille be used"))
        }
        countries  <- country
        years <- rep(year[[1]], lcountry)
      } else {              # if only one country selected
        countries <- rep(country, lyear)
        years <- year
      }
    } else  { # if lyear == 0
      countries  <- country
    }
  }   # end of country no NA



  #----------------------------------------------------------
  #   Clean data
  #----------------------------------------------------------

  #--------- get file names

  if (length(year) > 0) {  # if year is selected
    DT <- purrr::map2_dfr(countries,
                          years,
                          find_filename,
                          maindir = maindir)
  } else {                 # if no year is selected
    DT <- purrr::map_dfr(countries,
                         find_filename,
                         maindir = maindir)
  }

  varnames <- c("countrycode",
                "year",
                "survey",
                "vermast",
                "M",
                "veralt",
                "A",
                "collection",
                "module"
  )
  # combinations not found
  not_found <- grep("NotFound$",
                    DT$filename,
                    value = TRUE)

  # filename <-grep("^((?!NotFound).)*$",
  #                           DT$filename,
  #                           perl = TRUE,
  #                           value = TRUE)

  DT <- DT[!(grep("NotFound$",  filename))]  # returns a vector, not a data.table

  if (nrow(DT) == 0) {
    r <- list(pcn   = DT,
              fail  = not_found )
    return(r)
  }


  DT <- DT[,
           (varnames) :=
             tstrsplit(filename, "_", fixed = TRUE) # Create columns separating by _
  ][ ,
     c("M", "A") := NULL
  ]

  vers <- c("vermast", "veralt")
  nvers <- c("vm", "va")

  DT[, (nvers) :=
       lapply(.SD, (function(x) {       # anonymous function
         x <- gsub("[Vv](.*)", "\\1", x)  # remove V
         x <- as.numeric(x)            # convert to number
       })),
     .SDcols = vers]                   # apply to vermast and veralt

  DT[,
     mxvm := max(vm),         # Get max vermast by country, year, and survey
     by = .(countrycode, year, survey)
  ]

  nvers <- c(nvers, "mxvm")  # add to variables to remove

  DT <- DT[DT[ mxvm ==vm,
               .I[which.max(va)],         # Get max veralt by country, year, and survey
               by = .(countrycode, year, survey, module) # module is added for group data
  ]$V1
  ][,
    (nvers) := NULL
  ]

  #--------- defaults
  # if it is Argentina get only EPHC-S2
  if (any(DT$countrycode == "ARG")) {
    DT <- DT[,
             n := .N,
             by = .(countrycode, year)
    ][
      n == 1 |survey == "EPHC-S2" | countrycode != "ARG"
    ][ ,
       n := NULL
    ]
  } # end of ARG condition

  # if it is Brazil get only PNADC-E1
  if (any(DT$countrycode == "BRA")) {
    DT <- DT[,
             n := .N,
             by = .(countrycode, year)
    ][
      n == 1 |survey == "PNADC-E1" | countrycode != "BRA"
    ][ ,
       n := NULL
    ]
  } # end of BRA condition

  # if it is Germany get only GSOEP-LIS
  if (any(DT$countrycode == "DEU")) {
    DT <- DT[,
             n := .N,
             by = .(countrycode, year)
    ][
      n == 1 |survey == "GSOEP-LIS" | countrycode != "DEU"
    ][ ,
       n := NULL
    ]
  } # end of DEU condition

  # Modify year for EU-SILC survey
  DT <- DT[,
           year := as.numeric(year)
  ][survey == "EU-SILC",
    year := year - 1
  ]

  #----------------------------------------------------------
  #   find characteristics  of Stata data
  #----------------------------------------------------------

  if (characteristics) {
    chr <- purrr::map_df(DT$filepath, find_characteristics)
    DT <- cbind(DT, chr)
  }

  #--------- Return

  r <- list(pcn   = tibble::as_tibble(DT),
            fail  = not_found )
  return(r)
} # end of function pip_datafind

#----------------------------------------------------------
#   Auxiliary functions
#----------------------------------------------------------

find_filename <- function(country = NULL,
                          year = NULL,
                          maindir) {


  modules <- c("PCN", "GROUP")
  nmods <- length(modules)
  filename <- NULL
  i <- 1
  while (length(filename) == 0 && i <= nmods) {

    pattern <- paste0(modules[i], "[\\-]?[URNA]?\\.dta$")

    if (length(country) > 0) {
      country <- toupper(country)
      cdir <- paste0(maindir, "/", country)

      if (length(year) > 0) {
        pattern <- paste0(".*", year, ".*", modules[i], "[\\-]?[URNA]?\\.dta$")
      }

    } else {
      cdir <- maindir
      if (length(year) > 0) {
        warning("argument `year` ignored when no country is selected")
      }
    }


    filepath <- list.files(path = cdir,
                           pattern = pattern,
                           recursive = TRUE,
                           full.names = TRUE)

    filename <- gsub(pattern = paste0("(.*[Dd]ata/)(.*)\\.dta$"),
                     replacement = "\\2",
                     x =  filepath)
    i <- i + 1
  } # end of while


  if (length(filename) == 0) {

    if (length(year) > 0) {
      filename <- filepath <- paste(country, year, "NotFound", sep = "_")
    } else {
      filename <- filepath <- paste(country, "NotFound", sep = "_")
    }
  } # if no file was found on any module

  return(data.table::data.table(filepath,
                                filename))
  # return(tibble::tibble(filename))
} # end of find_filename


find_characteristics <- function(filepath) {
  at <- attributes(readstata13::read.dta13(filepath))$expansion.fields

  r2get <-  c("welfaretype")  # rows to get


  df <- tibble::enframe(at)      %>%     #convert into data frame
    tidyr::unnest(cols = c(value)) %>%   # separate values in each vector of each row
    dplyr::mutate(index = rep(c("a", "note", "value"), dplyr::n()/3)) %>%  # names of future columns
    tidyr::pivot_wider(values_from = value,  # reshape to wide format
                       names_from = index) %>%
    dplyr::filter(note  %in% r2get) %>%
    dplyr::select(note, value)

  # transpose
  tdf <- tibble::as_tibble(t(df[,-1]),
                           .name_repair = "minimal")
  colnames(tdf) <- df$note # add names to columns
  return(tdf)
}



# mbm = microbenchmark(
#   map  = purrr::map_dfr(countries, find_filename, maindir = maindir),
#   base = find_filename(maindir = maindir),
#   times = 25
# )
# mbm


