test_that("return warning when surveys are not available", {
  survey_id <-
    c(
      "PRY_2017_EPH_V01_M_V01_A_PIP_PC-GPWG",
      "PRY_2018_EPH_V01_M_V02_A_PIP_PC-GPWG",
      "x",
      "y",
      "z"
    )
  expect_warning(pip_load_data(survey_id = survey_id))
})

test_that("return NULL when only one wrong survey id is provided", {
  expect_null(pip_load_data(survey_id = "x"))
})




