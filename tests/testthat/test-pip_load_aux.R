test_that("Syntax is working", {
  expect_error(pip_load_aux())
  expect_error(pip_load_aux(measure = 'cpi',
                            file_to_load = "//w1wbgencifs01/pip/PIP-Data/_aux/ppp/ppp.fst")
               )

})
