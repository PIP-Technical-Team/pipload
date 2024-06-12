#' Update PIP microdata inventory
#'
#' @param  time time: time of execution to be saved in data signature. Default
#' is `format(Sys.time(), "%Y%m%d%H%M%S")`
#' @param  user character: Names or UPI of user. Default is `Sys.info()[8]`
#' @inheritParams pip_inventory
#' @inheritParams pip_create_globals
#'
#' @return invisible
#' @export
#' @import data.table
#'
#' @examples
#' \dontrun{
#' pip_update_inventory("COL")
#' }
pip_update_inventory <- function(country  = NULL,
                                 root_dir = Sys.getenv("PIP_ROOT_DIR"),
                                 maindir  = pipfun::pip_create_globals(root_dir)$PIP_DATA_DIR,
                                 force    = FALSE,
                                 time     = format(Sys.time(), "%Y%m%d%H%M%S"),
                                 user     = Sys.info()[8]
                                 ) {

  # inventory file to be used everywhere
  inv_file <- fs::path(maindir, "_inventory/inventory.fst")

  pst <- time |> as.POSIXct(format = "%Y%m%d%H%M%S")

  modi_date <- fs::file_info(inv_file)$modification_time


  if (maindir == pipfun::pip_create_globals(Sys.getenv("PIP_ROOT_DIR"))$PIP_DATA_DIR
      && is.null(country)) {

    # display menu if user wants to updated the whole thing

    ops <- c("Update the whole directory regardless of computational time",
             "No way, ABORT!",
             "Select one country to update",
             "Provide my own vector of countries to update")

    ans <- menu(ops,
                title=paste("Updating whole inventory may take up to 15min.\n",
                            "What do you want to do?")
    )

    # Action depending on answer
    if (ans == 1) {

      cli::cli_alert_info("Go and have a coffee. This may take a while!")

    } else if  (ans == 2) {

      return(invisible(TRUE))

    } else if (ans == 3) {
      message("this may take a few seconds to load.")

      country_list <- list_of_countries(maindir = maindir)

      ans_country  <- menu(country_list,
                           title = "Select the country you want to update")
      country     <- country_list[ans_country]

    } else if (ans == 4) {

      prompt <- paste("\nEnter the list of countries space-separated",
                      "\n (e.g., 'COL ARG', without the quotes) \n")
      country    <- as.character(strsplit(readline(prompt), " ")[[1]])

    } else {

      rlang::abort(c(
        "The number select is invalid.",
        i = paste0("It must one any integer from 1 to ", length(ops)),
        x = paste("you specified", ans)
      ),
      class = "pipload_error"
      )
    }

  } # end of condition when whole inventory to be updated


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- Look for data and organize    ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # search all data available for selected countries
  cli::cli_progress_step("reading PIP directory")

  if (is.null(country)) {
    cts_path <- fs::path(maindir)
  } else {
    cts_path <- fs::path(maindir, country)
  }

  inventory <- fs::dir_ls(path    = cts_path,
                          regexp  = "PIP.*[[:upper:]]\\.dta$",
                          recurse = TRUE)
  cli::cli_progress_done()

  inventory <- as.character(inventory) # necessary for the data signature
  # Remove _vintage folder from inventory
  inventory <- grep("_vintage", inventory, value = TRUE, invert = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- Data signature    ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--------- Get data signature of inventory just created ---------
  if (is.null(country)) {
    country <- list_of_countries(maindir = maindir)
  }

  ds_inventory <- lapply(country,
                         \(x) {
                           country_ds(x = x,
                                      inventory = inventory,
                                      time      = time,
                                      user      = user)
                           }) |>
    rbindlist(use.names = TRUE)


  #minimal database of new inventory
  dsi <- ds_inventory[, c("country_code", "data_signature")]

  # check signature of current fst file
  ds_inventory_path <- fs::path(maindir, "_inventory/inventory_datasignature.fst")

  if (file.exists(ds_inventory_path)) {

    # read data signature in production
    ds_inventory_production <- fst::read_fst(ds_inventory_path,
                                             as.data.table = TRUE)

  } else { # if ds files does not exist
    cli::cli_alert_info("Data signature of inventory was not found")

    # we try to create signature using the inventory file
    if (file.exists(inv_file)) {
      cli::cli_alert("Creating data signature from current
                     {.file inventory.fst} file", wrap = TRUE)

      df                   <- fst::read_fst(inv_file,
                                            as.data.table = TRUE)
      inventory_production <- df[, "orig"]
      avaiable_countries   <- unique(df$country_code)

      ds_inventory_production <- lapply(country,
                                        \(x) {
                                          country_ds(x = x,
                                                     inventory = inventory_production,
                                                     time      = time,
                                                     user      = user)
                                        }) |>
        rbindlist(use.names = TRUE)


    } else {
      # fake signature
      cli::cli_alert_info("file {.file inventory.fst} did not found")

      ds_inventory_production <-
        data.table::data.table(country_code   = list_of_countries(maindir = maindir),
                               data_signature = "0000",
                               time           = time,
                               user           = user)

      cli::cli_alert("Creating fake data signature to make comparison"
                     , wrap = TRUE)

    } # end of if inventory file does not exist
  }

  #minimal database of current inventory
  # get only the countries to compare
  dsip <- ds_inventory_production[country_code %chin% country,
                                  c("country_code", "data_signature")
  ]

  #--------- Compare data signatures ---------

  diff_inv <- fsetdiff(dsi, dsip)
  ldiff    <- dim(diff_inv)[1]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   Update data if Signature is different from
  #--------- the one in production   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (ldiff > 0 || force == TRUE) {


    if (ldiff > 0) {

      diff_cty  <- diff_inv[, unique(country_code)]

    } else {

      diff_cty <- country

    }

    inventory <- inventory[grepl(paste0("/(",
                                        paste(diff_cty, collapse = "|"),
                                        ")/"),
                                 inventory)
    ]
    if (length(diff_cty) > 0 && ldiff > 0) {
      cli::cli_alert_info("inventory has changed for {.file {diff_cty}}",
                          wrap = TRUE)
    }

    # make sure directory exists
    wholedir <- fs::path(maindir, "_inventory", "_vintage")
    if (!(fs::dir_exists(wholedir))) {

      fs::dir_create(wholedir,recurse =  TRUE)

    }

    #--------- create nice dataframe ---------
    cnames <-
      c(
        "country_code",
        "surveyid_year",
        "survey_acronym",
        "vermast",
        "M",
        "veralt",
        "A",
        "collection",
        "module"
      )

    dt <- data.table::data.table(orig = inventory)

    #--------- Format data ---------
    dt <-
      dt[,
         # Get filenane only
         filename := gsub("(.*[Dd]ata/)([^/]+)", "\\2", orig)
      ][,

        # Name sections of filename into variables
        (cnames) := tstrsplit(filename, "_", fixed=TRUE)
      ][,

        # Remove .dta in module
        module := gsub("\\.dta$", "", module)
      ][,

        # create tool and source
        c("tool", "source") := tstrsplit(module, "-", fixed = TRUE)
      ][,
        # change to lower case
        c("vermast", "veralt") := lapply(.SD, tolower),
        .SDcols = c("vermast", "veralt")
      ][
        ,
        # Remove unnecessary variables
        c("M", "A") := NULL
      ][
        # Remove unnecessary rows
        !(is.na(filename))
      ][,
        # remove root from file path
        orig := gsub((root_dir), "", orig)
      ]


    # Remove all data
    if (file.exists(inv_file)) {
      df <- fst::read_fst(inv_file,
                          as.data.table = TRUE)

      df <- df[!(country_code %chin% diff_cty)]
      dt <- rbindlist(list(dt, df),
                      use.names = TRUE,
                      fill = TRUE)
    }

    # re-write inventory in production if data signature is not found
    setorder(dt, country_code, surveyid_year, vermast, veralt)
    dt <- unique(dt) # Remove any duplicated row

    fst::write_fst(x = dt,
                   path = inv_file
    )

    haven::write_dta(data = dt,
                     path = fs::path(maindir, "_inventory", "inventory.dta")
    )

    # Vintage
    fst::write_fst(x = dt,
                   path = fs::path(wholedir, paste0("inventory_", time), ext = "fst")
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #---------   Save data signature   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (file.exists(ds_inventory_path) && length(diff_cty) > 0) {

      # read data signature in production, remove old signatures from
      # both dataframes and append

      dsp <- fst::read_fst(ds_inventory_path,
                           as.data.table = TRUE)

      dsp          <- dsp[!(country_code %chin% diff_cty)]
      ds_inventory <- ds_inventory[country_code %chin% diff_cty]
      ds_inventory <- rbindlist(list(ds_inventory, dsp), use.names = TRUE)

    }

    fst::write_fst(x = ds_inventory,
                   path = ds_inventory_path)

    if (ldiff == 0 && force == TRUE) {
      cli::cli_alert_warning("file {.file inventory.fst} has {cli::col_red('NOT')}
                             changed, but it was forcefully updated",
                             wrap = TRUE)
    } else {
      cli::cli_alert_success("file {.file inventory.fst} has been updated")
    }

  } else {

    cli::cli_alert_info("file {.file inventory.fst} is up to date.
                        No update performed")
  }
  return(invisible(inventory))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---------   Auxiliary functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


country_ds <- function(x, inventory, time, user) {
  y <- inventory[grepl(x, inventory)]
  ds <- digest::digest(y, algo = "xxhash64")

  df <- data.table::data.table(country_code   = x,
                               data_signature = ds,
                               time           = time,
                               user           = user
  )

  return(df)
}

