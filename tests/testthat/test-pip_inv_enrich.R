test_that("pip_inv_enrich returns inv unchanged when fields is empty", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    version_id_metadata = "v1"
  )
  result <- pip_inv_enrich(inv, fields = character(0))
  expect_identical(result, inv)
})

test_that("pip_inv_enrich extracts field from metadata into inventory column", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    version_id_metadata = "vid_001"
  )
  fake_meta <- list(reporting_level = "1")

  testthat::local_mocked_bindings(
    pip_read = function(id, alias, version = NULL, ...) fake_meta,
    .package = "pipload"
  )

  result <- pip_inv_enrich(inv, fields = "reporting_level")
  expect_true("reporting_level" %in% names(result))
  expect_equal(result$reporting_level, "1")
})

test_that("pip_inv_enrich passes version_id_metadata to pip_read", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    version_id_metadata = "exact_version_abc"
  )
  captured_version <- NULL
  fake_meta <- list(reporting_level = "2")

  testthat::local_mocked_bindings(
    pip_read = function(id, alias, version = NULL, ...) {
      captured_version <<- version
      fake_meta
    },
    .package = "pipload"
  )

  pip_inv_enrich(inv, fields = "reporting_level")
  expect_equal(captured_version, "exact_version_abc")
})

test_that("pip_inv_enrich uses version = NULL when version_id_metadata is NA", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    version_id_metadata = NA_character_
  )
  captured_version <- "not_null"
  fake_meta <- list(reporting_level = "1")

  testthat::local_mocked_bindings(
    pip_read = function(id, alias, version = NULL, ...) {
      captured_version <<- version
      fake_meta
    },
    .package = "pipload"
  )

  pip_inv_enrich(inv, fields = "reporting_level")
  expect_null(captured_version)
})

test_that("pip_inv_enrich gives NA when metadata load fails", {
  inv <- data.table::data.table(
    pip_id = "XYZ_2000_TST_INC_ALL",
    version_id_metadata = "stale_version"
  )

  testthat::local_mocked_bindings(
    pip_read = function(...) stop("version not found"),
    .package = "pipload"
  )

  expect_warning(
    result <- pip_inv_enrich(inv, fields = "reporting_level"),
    class = "pip_inv_enrich_missing_meta"
  )
  expect_true("reporting_level" %in% names(result))
  expect_true(is.na(result$reporting_level))
})

test_that("pip_inv_enrich removes pre-existing field columns before join", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    version_id_metadata = "v1",
    reporting_level.x = NA_character_,
    reporting_level.y = NA_character_
  )
  fake_meta <- list(reporting_level = "1")

  testthat::local_mocked_bindings(
    pip_read = function(...) fake_meta,
    .package = "pipload"
  )

  result <- pip_inv_enrich(inv, fields = "reporting_level")
  expect_false("reporting_level.x" %in% names(result))
  expect_false("reporting_level.y" %in% names(result))
  expect_equal(result$reporting_level, "1")
})

test_that("pip_inv_enrich extracts multiple fields in one call", {
  inv <- data.table::data.table(
    pip_id = c("BOL_2022_EH_INC_ALL", "CHN_2015_NSS_INC_ALL"),
    version_id_metadata = c("v1", "v2")
  )
  fake_meta_bol <- list(reporting_level = "1", welfare_type = "INC")
  fake_meta_chn <- list(reporting_level = "2", welfare_type = "INC")

  call_count <- 0L
  testthat::local_mocked_bindings(
    pip_read = function(id, ...) {
      call_count <<- call_count + 1L
      if (id == "BOL_2022_EH_INC_ALL") fake_meta_bol else fake_meta_chn
    },
    .package = "pipload"
  )

  result <- pip_inv_enrich(inv, fields = c("reporting_level", "welfare_type"))
  expect_true("reporting_level" %in% names(result))
  expect_true("welfare_type" %in% names(result))
  expect_equal(result$reporting_level, c("1", "2"))
  expect_equal(result$welfare_type, c("INC", "INC"))
})
