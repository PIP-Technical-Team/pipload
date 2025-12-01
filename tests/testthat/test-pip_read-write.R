library(testthat)
library(fs)
library(data.table)
library(stamp)

# -------------------------------
# Helper: create temp directory and write multiple versions
# -------------------------------
create_test_dir <- function() {
  dir <- fs::path_temp()
  fs::dir_create(dir)

  # Version 1
  pip_write(1:5, id = "x", dir = dir)
  Sys.sleep(1)

  # Version 2
  pip_write(1:10, id = "x", dir = dir)
  Sys.sleep(1)

  # Version 3
  pip_write(4:9, id = "x", dir = dir)

  dir
}

# -------------------------------
# Suppress PK warnings
# -------------------------------
old_opts <- stamp::st_opts(.get = TRUE)
on.exit(do.call(stamp::st_opts, old_opts), add = TRUE)

stamp::st_opts(
  warn_missing_pk_on_load = FALSE,
  require_pk_on_load     = FALSE
)

# -------------------------------
# pip_write and pip_read basic functionality
# -------------------------------

test_that("pip_write and pip_read work for latest version", {
  dir <- create_test_dir()
  res <- pip_read("x", dir = dir)
  expect_equal(res, 4:9)
})

test_that("pip_read returns previous versions by negative index", {
  dir <- create_test_dir()
  expect_equal(pip_read("x", dir = dir, version = -1), 1:10)
  expect_equal(pip_read("x", dir = dir, version = -2), 1:5)
})

test_that("pip_read returns available versions as a data.table with vintage", {
  dir <- create_test_dir()
  vers <- pip_read("x", dir = dir, version = "available")
  expect_s3_class(vers, "data.table")
  expect_true(nrow(vers) >= 3)
  expect_true("vintage" %in% names(vers))
  expect_equal(vers$vintage[1], 0)
  expect_equal(vers$vintage[2], -1)
})

test_that("pip_read errors on positive version index", {
  dir <- create_test_dir()
  expect_error(pip_read("x", dir = dir, version = 1))
})

test_that("pip_read errors on missing artifact", {
  dir <- fs::path_temp()
  fs::dir_create(dir)
  expect_error(pip_read("not_a_file", dir = dir))
})

# -------------------------------
# pip_write edge cases
# -------------------------------

test_that("pip_write preserves metadata and code hash", {
  dir <- fs::path_temp()
  fs::dir_create(dir)

  my_code <- function() { 1 + 1 }
  out <- pip_write(1:3, id = "y", dir = dir, metadata = list(foo = "bar"), code = my_code)

  expect_true("metadata" %in% names(out))
  expect_true("foo" %in% names(out$metadata))
  expect_true("code_hash" %in% names(out$metadata))
})

test_that("pip_write always creates a version ID", {
  dir <- fs::path_temp()
  fs::dir_create(dir)

  out <- pip_write(letters, id = "z", dir = dir)
  expect_true(!is.null(out$version_id))
  expect_type(out$version_id, "character")
})

test_that("pip_read preserves content integrity and type", {
  dir <- create_test_dir()

  res <- pip_read("x", dir = dir)
  expect_true(is.integer(res))
  expect_equal(res, 4:9)
})

test_that("pip_read works with specific version ID", {
  dir <- create_test_dir()
  vers <- pip_read("x", dir = dir, version = "available")
  version_id <- vers$version_id[2]  # pick second version
  res <- pip_read("x", dir = dir, version = version_id)
  expect_equal(res, 1:10)
})

test_that("pip_read works with interactive 'select' in non-interactive session", {
  skip_if(interactive(), "Skip interactive selection test in non-interactive session")
  dir <- create_test_dir()
  expect_error(pip_read("x", dir = dir, version = "select"))
})

# -------------------------------
# Cleanup
# -------------------------------
# Optionally delete temp directories after tests
