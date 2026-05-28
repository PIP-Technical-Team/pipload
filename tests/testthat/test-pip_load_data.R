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
  expect_null( suppressWarnings(pip_load_data(survey_id = "x")) )
})

# -------------------------------------------------------------------------
# load_pip_data() — pk warning suppression
# -------------------------------------------------------------------------

test_that("load_pip_data suppresses warn_missing_pk_on_load during pip_read call", {
  # Arrange: ensure the global option is TRUE so we can detect the suppression.
  old_warn <- stamp::st_opts("warn_missing_pk_on_load", .get = TRUE)
  on.exit(stamp::st_opts(warn_missing_pk_on_load = old_warn), add = TRUE)
  stamp::st_opts(warn_missing_pk_on_load = TRUE)

  warn_during_call <- NULL

  local_mocked_bindings(
    # Stub folder resolution
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    # Stub alias lookup so load_pip_data finds a match
    st_alias_list = function() {
      data.table::data.table(root = "/fake/dir", alias = "fake_alias")
    },
    .package = "stamp"
  )
  local_mocked_bindings(
    # Capture the stamp option value at the moment pip_read is invoked
    pip_read = function(...) {
      warn_during_call <<- stamp::st_opts(
        "warn_missing_pk_on_load",
        .get = TRUE
      )
      data.table::data.table() # dummy return
    },
    .package = "pipload"
  )

  load_pip_data(id_name = "BOL_2022_EH_INC_ALL")

  # During the pip_read call, warn_missing_pk_on_load must have been FALSE
  expect_false(isTRUE(warn_during_call))
  # After the call returns, the option must be restored to TRUE
  expect_true(isTRUE(stamp::st_opts("warn_missing_pk_on_load", .get = TRUE)))
})
