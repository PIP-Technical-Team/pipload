test_that("Syntax is working", {

  # No arguments
  pip_load_aux() |>
    expect_error()


  # wrong use of file_to_load
  pip_load_aux(measure = 'cpi',
               file_to_load = "//w1wbgencifs01/pip/PIP-Data/_aux/ppp/ppp.fst") |>
  expect_warning()  |>
  expect_error()


  # wrong use of filename using old way with file_to_load
  pip_load_aux(measure = 'cpi',
               filename = "//w1wbgencifs01/pip/PIP-Data/_aux/ppp/ppp.fst") |>
  expect_warning()  |>
  expect_error()


  # deprecation works
  pip_load_aux(measure = "ppp",
               file_to_load = "ppp_vintage") |>
  expect_warning()

})
