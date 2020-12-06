#' Update PIP microdata inventory
#'
#' @inheritParams pip_inventory
#' @param  time time: time of execution to be saved in data signature. Default
#' is `format(Sys.time(), "%Y%m%d%H%M%S")`
#' @param  user character: Names or UPI of user. Default is `Sys.info()[8]`
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_update_inventory <- function(country = NULL,
                                 maindir = getOption("pip.maindir"),
                                 force   = FALSE,
                                 time    = format(Sys.time(), "%Y%m%d%H%M%S"),
                                 user    = Sys.info()[8]
                                 ) {

  # inventory file to be used everywhere
  inv_file <- paste0(maindir, "_inventory/inventory.fst")

  if (maindir == getOption("pip.maindir") && is.null(country)) {

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

      message("Go and have a coffee.\nThis may take a while!")

    } else if  (ans == 2) {

      return(invisible(TRUE))

    } else if (ans == 3) {
      message("this may take a few seconds to load.")

      country_list <- list_of_countries(maindir = maindir)

      country      <- menu(country_list,
                           title = "Select the country you want to update")

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
  inventory <- fs::dir_ls(path    = paste0(maindir, country),
                          regexp  = "PIP.*dta$",
                          recurse = TRUE)

  inventory <- as.character(inventory) # necessary for the data signature
  # Remove _vintage folder from inventory
  inventory <- grep("_vintage", inventory, value = TRUE, invert = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #--------- Data signature    ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #--------- Get data signature of inventory just created ---------
  if (is.null(country)) {
    country <- list_of_countries(maindir)
  }

  ds_inventory <- purrr::map_df(.x        = country,
                                .f        = country_ds,
                                inventory = inventory,
                                time      = time,
                                user      = user)

  #minimal database of new inventory
  dsi <- ds_inventory[,
                      c("country_code", "data_signature")
  ]

  # check signature of current fst file
  ds_inventory_path <- paste0(maindir, "_inventory/inventory_datasignature.fst")

  if (file.exists(ds_inventory_path)) {

    # read data signature in production
    ds_inventory_production <- fst::read_fst(ds_inventory_path)
    setDT(ds_inventory_production)

  } else { # if ds files does not exist

    # we try to create signature using the inventory file
    if (file.exists(inv_file)) {

      df                   <- fst::read_fst(inv_file)
      inventory_production <- df[, "orig"]
      avaiable_countries   <- unique(df$country_code)

      ds_inventory_production <- purrr::map_df(.x        = country,
                                               .f        = country_ds,
                                               inventory = inventory_production,
                                               time      = time,
                                               user      = user)

    } else {
      # fake signature

      ds_inventory_production <-
        data.table::data.table(country_code   = list_of_countries(maindir),
                               data_signature = "0000",
                               time           = time,
                               user           = user)

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
      message(paste0("data inventory changed for ",
                     add_and(diff_cty),
                     ".\n"))
    }

    # make sure directory exists
    wholedir <- paste0(maindir, "_inventory/_vintage/")
    if (!(dir.exists(wholedir))) {
      dir.create(wholedir, recursive = TRUE)
    }

    #--------- create nice dataframe ---------
    dt <- data.table::data.table(orig = inventory)

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

    # check number of items Pick third one by random (it could be any other row)
    linv <- inventory[[3]]
    nobj <- length(strsplit(linv, "/")[[1]])

    dt[,
       # Get filename only
       filename := tstrsplit(orig, "/", fixed=TRUE, keep = (nobj))
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
    ]

    # Remove all data
    if (file.exists(inv_file)) {
      df <- fst::read_fst(inv_file)
      setDT(df)

      df <- df[!(country_code %chin% diff_cty)]
      dt <- rbindlist(list(dt, df), use.names = TRUE)
    }

    # re-write inventory in production if data signature is not found
    setorder(dt, country_code, surveyid_year, vermast, veralt)
    dt <- unique(dt) # Remove any duplicated row

    fst::write_fst(x = dt,
                   path = inv_file
    )

    haven::write_dta(data = dt,
                     path = paste0(maindir, "_inventory/", "inventory.dta")
    )

    # Vintage
    fst::write_fst(x = dt,
                   path = paste0(wholedir, "inventory", "_", time,".fst")
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #---------   Save data signature   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (file.exists(ds_inventory_path) && length(diff_cty) > 0) {

      # read data signature in production, remove old signatures from
      # both dataframes and append

      dsp <- fst::read_fst(ds_inventory_path)
      setDT(dsp)

      dsp          <- dsp[!(country_code %chin% diff_cty)]
      ds_inventory <- ds_inventory[country_code %chin% diff_cty]
      ds_inventory <- rbindlist(list(ds_inventory, dsp), use.names = TRUE)

    }

    fst::write_fst(x = ds_inventory,
                   path = ds_inventory_path)

    infmsg <- paste("Data signature has changed, it was not found,",
                    "or update was forced.\n",
                    paste0("`inventory.fst` has been updated")
    )
    rlang::inform(infmsg)

  } else {

    rlang::inform("Data signature is up to date.\nNo update performed")
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

