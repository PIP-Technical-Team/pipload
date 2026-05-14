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
    class = "error"
  )
})

test_that("load_pip_deflated_data() calls load_pip_data() then pd_deflation()", {
  skip_if_not_installed("pipdata")

  # No "module" col — simulates new pipeline survey structure
  fake_survey <- data.table::data.table(welfare = 1:3, weight = c(1, 1, 1))
  fake_deflated <- data.table::data.table(welfare_ppp = 1:3)

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
      expect_s3_class(dt, "pipmd") # class assigned from pip_id suffix "ALL"
      expect_equal(pip_id, "BOL_2022_EH_INC_ALL")
      fake_deflated
    },
    .package = "pipdata"
  )

  result <- load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")
  expect_identical(result, fake_deflated)
})

test_that("load_pip_deflated_data() forwards cpi/ppp/pop to pd_deflation()", {
  skip_if_not_installed("pipdata")

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
  skip_if_not_installed("pipdata")

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

# Integration test — requires a working release + stamp setup
test_that("load_pip_deflated_data() integration: load → deflate round-trip", {
  skip_if_not_installed("pipdata")
  skip_if_not_installed("pipfun")
  skip_on_ci()

  lr <- pipfun::get_latest_pip_release()
  pipfun::setup_working_release(
    release = lr$release,
    identity = lr$identity,
    verbose = FALSE
  )

  result <- load_pip_deflated_data(id_name = "BOL_2022_EH_INC_ALL")

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
