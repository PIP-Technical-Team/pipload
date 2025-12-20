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

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read returns previous versions by negative index", {
  dir <- create_test_dir()
  expect_equal(pip_read("x", dir = dir, version = -1), 1:10)
  expect_equal(pip_read("x", dir = dir, version = -2), 1:5)

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read returns available versions as a data.table with vintage", {
  dir <- create_test_dir()
  vers <- pip_read("x", dir = dir, version = "available")
  expect_s3_class(vers, "data.table")
  expect_true(nrow(vers) >= 3)
  expect_true("vintage" %in% names(vers))
  expect_equal(vers$vintage[1], 0)
  expect_equal(vers$vintage[2], -1)

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read errors on positive version index", {
  dir <- create_test_dir()
  expect_error(pip_read("x", dir = dir, version = 1))

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read errors on missing artifact", {
  dir <- fs::path_temp()
  fs::dir_create(dir)
  expect_error(pip_read("not_a_file", dir = dir))

  # clean up
  fs::dir_delete(dir)
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

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_write always creates a version ID", {
  dir <- fs::path_temp()
  fs::dir_create(dir)

  out <- pip_write(letters, id = "z", dir = dir)
  expect_true(!is.null(out$version_id))
  expect_type(out$version_id, "character")

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read preserves content integrity and type", {
  dir <- create_test_dir()

  res <- pip_read("x", dir = dir)
  expect_true(is.integer(res))
  expect_equal(res, 4:9)

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read works with specific version ID", {
  dir <- create_test_dir()
  vers <- pip_read("x", dir = dir, version = "available")
  version_id <- vers$version_id[2]  # pick second version
  res <- pip_read("x", dir = dir, version = version_id)
  expect_equal(res, 1:10)

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read works with interactive 'select' in non-interactive session", {
  skip_if(interactive(), "Skip interactive selection test in non-interactive session")
  dir <- create_test_dir()
  expect_error(pip_read("x", dir = dir, version = "select"))
  # clean up
  fs::dir_delete(dir)
})


test_that("pip_read detects multiple formats and requires explicit format", {
  dir <- fs::path_temp("pip_fmt_test")
  fs::dir_create(dir)

  # Write the same artifact in two different formats
  pip_write(letters[1:3], id = "fmt_test", dir = dir, format = "qs2")
  Sys.sleep(1)
  pip_write(letters[4:6], id = "fmt_test", dir = dir, format = "rds")

  # Without specifying format, pip_read should error because multiple formats exist
  expect_error(pip_read("fmt_test", dir = dir))

  # Specifying format should load the correct object
  expect_equal(pip_read("fmt_test", dir = dir, format = "qs2"), letters[1:3])
  expect_equal(pip_read("fmt_test", dir = dir, format = "rds"), letters[4:6])

  # clean up
  fs::dir_delete(dir)
})


test_that("pip_read auto-detects single existing format when id has no extension", {
  dir <- fs::path_temp("pip_single_fmt_test")
  fs::dir_create(dir)

  # Write artifact in a single format (qs2)
  pip_write(mtcars[1:3, ], id = "single_fmt", dir = dir, format = "qs2")

  # Calling pip_read without format should auto-detect and load the qs2 file
  res <- pip_read("single_fmt", dir = dir)
  expect_true(is.data.frame(res) || is.data.table(res))
  expect_equal(nrow(res), 3)

  # clean up
  fs::dir_delete(dir)
})


test_that("pip_read handles id with extension and respects explicit format", {
  dir <- fs::path_temp("pip_id_ext_test")
  fs::dir_create(dir)

  # Write artifact with explicit extension
  pip_write(iris[1:5, ], id = "with_ext", dir = dir, format = "qs2")

  # When id includes the extension and format = NULL, it should load the file
  res1 <- pip_read("with_ext.qs2", dir = dir, format = NULL)
  expect_equal(nrow(res1), 5)

  # If the caller requests a different format, it should error (file not found)
  expect_error(pip_read("with_ext.qs2", dir = dir, format = "rds"))

  # clean up
  fs::dir_delete(dir)
})

# -------------------------------
# Cleanup
# -------------------------------
# Optionally delete temp directories after tests
