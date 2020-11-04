context("PIP inventory")

test_that("list of countries correct", {
  cdf <- fs::dir_ls(path    = paste0(getOption("pip.maindir"), "COL"),
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
