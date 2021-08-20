context("PIP inventory")

test_that("list of countries correct", {
  root_dir  <-  Sys.getenv("PIP_DATA_ROOT_FOLDER")
  maindir   <-  pip_create_globals(root_dir)$PIP_DATA_DIR

  cdf <- fs::dir_ls(path    = paste0(maindir, "COL"),
                    regexp  = "PIP.*dta$",
                    recurse = TRUE
  )
  cdf <- as.character(cdf)
  # Remove _vintage folder from inventory
  cdf <- grep("_vintage", cdf, value = TRUE, invert = TRUE)
  df  <- pip_inventory("update",
                      country = "COL")
  df  <- as.character(df)

  expect_equal(df, cdf)
})
