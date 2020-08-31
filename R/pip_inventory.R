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
pip_inventory <- function(action    = "load",
                          maindir   = getOption("pip.maindir"),
                          force     = FALSE
                          ) {
  if (action == "load" ) {

  } else if (action == "update") {

    if (maindir == getOption("pip.maindir")) {
      countries <- fs::dir_ls(path    = maindir,
                              recurse = FALSE,
                              type    = "directory"
                              )

    }

    inventory <- fs::dir_ls(path    = maindir,
                            regexp  = "PIP.*dta$",
                            recurse = TRUE
                        )

    ds_inventory <- digest::digest(inventory, algo = "xxhash64")

    # check signature of current fst file
    ds_inventory_path <- paste0(maindir, "_inventory/inventory_datasignature.txt")  # data signature in production

    if (file.exists(ds_inventory_path)) {

      # read data signature in production
      ds_inventory_production <- readr::read_lines(ds_inventory_path)[[1]]

    } else {

      # fake signature
      ds_inventory_production <- "0000"
    }

    #--------- if Signature is different from the one in production ---------

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

      ds_text <- c(ds_inventory, time, Sys.info()[8])

      readr::write_lines(x = ds_text,
                         path = ds_inventory_path)

      infmsg <- paste("Data signature has changed, it was not found,",
                      "or update was forced.\n",
                      paste0("`inventory.fst` has been updated")
      )
      rlang::inform(infmsg)
      return(invisible(inventory))

    } else {

        rlang::inform("Data signature is up to date.\nNo update performed")
        return(invisible(FALSE))
    }


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
