test_that("load_pip_deflated_data() aborts when id_name is length > 1", {
  expect_error(
    load_pip_deflated_data(id_name = c("BOL_2022_EH_INC_ALL", "PRY_2018_EPH_INC_ALL")),
    regexp = "single string",
    class = "rlang_error"
  )
})

test_that("load_pip_deflated_data() aborts with informative error when pipdata is not installed", {
  local_mocked_bindings(
    check_installed = function(pkg, reason = NULL, ...) {
      rlang::abort(
        paste0("Package '", pkg, "' is required ", reason),
        call = NULL
      )
    },
    .package = "rlang"
  )

  expect_error(
    load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL"),
    regexp = "pipdata",
    class = "rlang_error"
  )
})

test_that("load_pip_deflated_data() calls load_pip_data() then pd_deflation()", {
  # No "module" col — simulates new pipeline survey structure
  fake_survey <- data.table::data.table(welfare = 1:3, weight = c(1, 1, 1))
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)
  captured_dt <- NULL
  captured_pip_id <- NULL

  local_mocked_bindings(
    load_pip_data = function(...) fake_survey,
    .package = "pipload"
  )

  local_mocked_bindings(
    pd_deflation = function(
      dt,
      pip_id = NULL,
      cpi = NULL,
      ppp = NULL,
      pop = NULL,
      ...
    ) {
      captured_dt <<- dt
      captured_pip_id <<- pip_id
      fake_deflated
    },
    .package = "pipdata"
  )

  result <- load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")
  expect_s3_class(captured_dt, "pipmd")  # class assigned from pip_id suffix "ALL"
  expect_equal(captured_pip_id, "BOL_2022_EH_INC_ALL")
  expect_identical(result, fake_deflated)
})

test_that("load_pip_deflated_data() forwards cpi/ppp/pop to pd_deflation()", {
  fake_survey <- data.table::data.table(welfare = 1:3)
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)
  fake_cpi <- data.table::data.table(cpi = 1.1)

  local_mocked_bindings(
    load_pip_data = function(...) fake_survey,
    .package = "pipload"
  )

  local_mocked_bindings(
    pd_deflation = function(
      dt,
      pip_id = NULL,
      cpi = NULL,
      ppp = NULL,
      pop = NULL,
      ...
    ) {
      expect_identical(cpi, fake_cpi)
      fake_deflated
    },
    .package = "pipdata"
  )

  result <- load_pip_deflated_data(
    id_name = "BOL_2022_EH_INC_ALL",
    cpi = fake_cpi
  )
  expect_identical(result, fake_deflated)
})

test_that("load_pip_deflated_data() propagates load_pip_data() errors", {
  local_mocked_bindings(
    load_pip_data = function(...) {
      cli::cli_abort("Wrong number of data to load.")
    },
    .package = "pipload"
  )

  expect_error(
    load_pip_deflated_data(id_name = "INVALID"),
    regexp = "Wrong number"
  )
})

test_that("load_pip_deflated_data() assigns pipmd class when module column present (legacy pipeline)", {
  # Survey with a module column — legacy pipeline; assign_pipclass() dispatches.
  fake_survey <- data.table::data.table(welfare = 1:3, module = "PC")
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)

  local_mocked_bindings(
    load_pip_data = function(...) fake_survey,
    .package = "pipload"
  )

  local_mocked_bindings(
    pd_deflation = function(dt, pip_id = NULL, ...) {
      expect_s3_class(dt, "pipmd")
      fake_deflated
    },
    .package = "pipdata"
  )

  result <- load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")
  expect_identical(result, fake_deflated)
})

test_that("load_pip_deflated_data() strips file extension from id_name", {
  fake_survey <- data.table::data.table(welfare = 1:3)
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)
  captured_pip_id <- NULL

  local_mocked_bindings(
    load_pip_data = function(...) fake_survey,
    .package = "pipload"
  )

  local_mocked_bindings(
    pd_deflation = function(dt, pip_id = NULL, ...) {
      captured_pip_id <<- pip_id
      fake_deflated
    },
    .package = "pipdata"
  )

  load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL.qs2")
  expect_equal(captured_pip_id, "BOL_2022_EH_INC_ALL")
})

test_that("load_pip_deflated_data() resolves pip_id via find_pip_data() when no id_name", {
  fake_inv <- data.table::data.table(pip_id = "BOL_2022_EH_INC_ALL")
  fake_survey <- data.table::data.table(welfare = 1:3)
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)

  local_mocked_bindings(
    find_pip_data = function(...) fake_inv,
    load_pip_data = function(...) fake_survey,
    .package = "pipload"
  )

  local_mocked_bindings(
    pd_deflation = function(dt, pip_id = NULL, ...) {
      expect_equal(pip_id, "BOL_2022_EH_INC_ALL")
      fake_deflated
    },
    .package = "pipdata"
  )

  result <- load_pip_deflated_data(country_code = "BOL", surveyid_year = 2022)
  expect_identical(result, fake_deflated)
})

test_that("load_pip_deflated_data() assigns pipgd class for GROUP surveys", {
  fake_survey <- data.table::data.table(welfare = 1:3)
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)

  local_mocked_bindings(
    load_pip_data = function(...) fake_survey,
    .package = "pipload"
  )

  local_mocked_bindings(
    pd_deflation = function(dt, pip_id = NULL, ...) {
      expect_s3_class(dt, "pipgd")
      fake_deflated
    },
    .package = "pipdata"
  )

  result <- load_pip_deflated_data(id_name = "BOL_2022_EH_INC_GROUP")
  expect_identical(result, fake_deflated)
})

# Integration test — requires a working release + stamp setup
test_that("load_pip_deflated_data() integration: load → deflate round-trip", {
  skip_if_not_installed("pipdata")
  skip_if_not_installed("pipfun")
  skip_on_ci()

  lr <- pipfun::get_latest_pip_release()
  # Note: pipfun has no teardown_working_release() export; side-effects
  # (stamp aliases) persist for the session, which is acceptable in
  # integration tests that require a working release context anyway.
  pipfun::setup_working_release(
    release = lr$release,
    identity = lr$identity,
    verbose = FALSE
  )

  result <- suppressWarnings(
    load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
