#' Title
#'
#' @param action
#' @param main
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_inventory <- function(action            = "load",
                          country           = NULL,
                          maindir           = getOption("pip.maindir"),
                          force             = FALSE,
                          inventory_version = NULL
                          ) {

  #----------------------------------------------------------
  #   Initial parameters
  #----------------------------------------------------------

  # inventory file to be used everywhere
  inv_file <- paste0(maindir, "_inventory/inventory.fst")

  # get processing time for all data signatures
  time <- format(Sys.time(), "%Y%m%d%H%M%S")

  # User
  user <- Sys.info()[8]

  if (action == "load" ) {

    #----------------------------------------------------------
    #   Load data
    #----------------------------------------------------------

    if (file.exists(inv_file)) {
      df       <- fst::read_fst(inv_file)
      return(df)
    } else {
      rlang::abort(c(
                    paste("file", inv_file, "does not exist"),
                    i = "Check your connection to the drives"
                    ),
                    class = "pipload_error"
                    )

    }


  } else if (action == "update") {
    #----------------------------------------------------------
    #   Update data
    #----------------------------------------------------------


    #--------- If user wants to update the whoe inventory ---------

    if (maindir == getOption("pip.maindir") && is.null(country)) {

      # display meny if user wants to updated the whole thing

      ops <- c("Update the whole directory regardless of computational time",
               "No way, ABORT!",
               "Select one country to update",
               "Provide my own vector of countries to update")

      ans <- menu(ops,
                  title="Updating whole inventory may take up to 15min.\n"
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


    #----------------------------------------------------------
    #   Look for data and organize
    #----------------------------------------------------------

    # searh all data avaiable for selected countries
    inventory <- fs::dir_ls(path    = paste0(maindir, country),
                            regexp  = "PIP.*dta$",
                            recurse = TRUE
                        )
    inventory <- as.character(inventory) # necessary for the data signature

    #----------------------------------------------------------
    #   Data signature
    #----------------------------------------------------------

    #--------- Get data signature of inventory just created ---------

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

      # we try to create it using the inventory file
      if (file.exists(inv_file)) {

        df                   <- fst::read_fst(inv_file)
        inventory_production <- df[, "orig"]

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


    #----------------------------------------------------------
    #   Update data if Signature is different from
    #   the one in production
    #----------------------------------------------------------

    if (ds_inventory_production != ds_inventory || force == TRUE) {

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
          "year",
          "survey_acronym",
          "vermast",
          "M",
          "veralt",
          "A",
          "collection",
          "module"
        )

      # check number of items Pick thirdone by random (it could be any other row)
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
          ][
            ,
            # Remove unnecessary variables
            c("M", "A") := NULL
          ]
      # re-write inventory in production if data signature is not found

      fst::write_fst(x = dt,
                     path = paste0(maindir, "_inventory/", "inventory.fst")
                    )

      haven::write_dta(data = dt,
                       path = paste0(maindir, "_inventory/", "inventory.dta")
      )
      # Vintage
      time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones
      fst::write_fst(x = dt,
                     path = paste0(wholedir, "inventory", "_", time,".fst")
                      )

      ds_text <- c(ds_inventory, time, user)

      readr::write_lines(x = ds_text,
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


  } else {
    rlang::abort(c(
                "The action selected is not a valid name",
                i = "you can use `load` or `update`", # update this message automatically
                x = paste("you specified", action)
              ),
              class = "pipload_error"
              )
  }
}


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


list_of_countries <- function(maindir) {

  countries <- fs::dir_ls(path    = maindir,
                          recurse = FALSE,
                          type    = "directory"
  )

  country_list <- gsub(maindir, "", countries)
  country_list <- country_list[!grepl("^_", country_list)]
  return(country_list)
}
