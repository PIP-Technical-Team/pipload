test_that("list of countries correct", {
  cdf <- fs::dir_ls(path    = paste0(getOption("pip.maindir"), "COL"),
                    regexp  = "PIP.*dta$",
                    recurse = TRUE
  )

  df <- pip_inventory("update",
                      country = "COL")

  expect_equal(df, cdf)
})
