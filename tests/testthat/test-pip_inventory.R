test_that("list of countries correct", {
  cdf <- fs::dir_ls(path    = paste0(getOption("pip.maindir"), "COL"),
                    regexp  = "PIP.*dta$",
                    recurse = TRUE
  )
  cdf <- as.character(cdf)
  df  <- pip_inventory("update",
                      country = "COL")
  df  <- as.character(df)

  expect_equal(df, cdf)
})
