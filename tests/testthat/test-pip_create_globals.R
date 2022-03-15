test_that("wrong names in vintage", {
  skip_on_ci()
  expect_error(pip_create_globals(vintage = list(test = "hola", test2 = "chao")))

})


# test_that("only accept lists and characters of length 1", {
#
# })
#
# test_that("inputs within vintage lists are correct", {
#
#
#
#
# })
#
#
