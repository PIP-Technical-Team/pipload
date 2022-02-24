context("Consistency conditions")

test_that("main directory is reachable", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(pip_find_data(maindir = "//nofolder"))
})

test_that("Country and Year arguments are ok", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_warning(pip_find_data(country = c("ARG", "COL"),
                              year    = c(2000, 2001))
                )
})
