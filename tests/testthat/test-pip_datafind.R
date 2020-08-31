context("Consistency conditions")

test_that("main directory is reachable", {
  expect_error(pip_datafind(maindir = "//nofolder"))
})

test_that("Country and Year arguments are ok", {
  expect_warning(pip_datafind(country = c("ARG", "COL"),
                              year    = c(2000, 2001))
                )
})
