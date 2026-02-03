library(testthat)
library(fs)
library(data.table)
library(stamp)

# -------------------------------
# Helper: create temp directory and write multiple versions
# -------------------------------
create_test_alias <- function(alias_name = NULL) {
  if (is.null(alias_name)) {
    alias_name <- paste0("test_", as.integer(Sys.time() * 1000))
  }
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  # Version 1
  pip_write(1:5, id = "x", alias = alias_name)
  Sys.sleep(1)

  # Version 2
  pip_write(1:10, id = "x", alias = alias_name)
  Sys.sleep(1)

  # Version 3
  pip_write(4:9, id = "x", alias = alias_name)

  list(alias = alias_name, dir = dir)
}

# -------------------------------
# Suppress PK warnings
# -------------------------------
old_opts <- stamp::st_opts(.get = TRUE)
on.exit(do.call(stamp::st_opts, old_opts), add = TRUE)

stamp::st_opts(
  warn_missing_pk_on_load = FALSE,
  require_pk_on_load = FALSE
)

# -------------------------------
# pip_write and pip_read basic functionality
# -------------------------------

test_that("pip_write and pip_read work for latest version", {
  ctx <- create_test_alias("test_latest")
  res <- pip_read("x", alias = ctx$alias)
  expect_equal(res, 4:9)

  # clean up
  fs::dir_delete(ctx$dir)
})

test_that("pip_write and pip_read accept and forward alias", {
  alias_name <- "test_alias_forward"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  out <- pip_write(1:2, id = "alias_test", alias = alias_name)
  expect_true(!is.null(out$version_id))

  res <- pip_read("alias_test", alias = alias_name)
  expect_equal(res, 1:2)

  fs::dir_delete(dir)
})

test_that("pip_read returns previous versions by negative index", {
  ctx <- create_test_alias("test_versions")
  expect_equal(pip_read("x", alias = ctx$alias, version = -1), 1:10)
  expect_equal(pip_read("x", alias = ctx$alias, version = -2), 1:5)

  # clean up
  fs::dir_delete(ctx$dir)
})

test_that("pip_read returns available versions as a data.table with vintage", {
  ctx <- create_test_alias("test_available")
  vers <- pip_read("x", alias = ctx$alias, version = "available")
  expect_s3_class(vers, "data.table")
  expect_true(nrow(vers) >= 3)
  expect_true("vintage" %in% names(vers))
  expect_equal(vers$vintage[1], 0)
  expect_equal(vers$vintage[2], -1)

  # clean up
  fs::dir_delete(ctx$dir)
})

test_that("pip_read errors on positive version index", {
  ctx <- create_test_alias("test_positive_err")
  expect_error(pip_read("x", alias = ctx$alias, version = 1))

  # clean up
  fs::dir_delete(ctx$dir)
})

test_that("pip_read errors on missing artifact", {
  alias_name <- "test_missing"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)
  expect_error(pip_read("not_a_file", alias = alias_name))

  # clean up
  fs::dir_delete(dir)
})

# -------------------------------
# pip_write edge cases
# -------------------------------

test_that("pip_write preserves metadata and code hash", {
  alias_name <- "test_metadata"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  my_code <- function() {
    1 + 1
  }
  out <- pip_write(
    1:3,
    id = "y",
    alias = alias_name,
    metadata = list(foo = "bar"),
    code = my_code
  )

  expect_true("metadata" %in% names(out))
  expect_true("foo" %in% names(out$metadata))
  expect_true("code_hash" %in% names(out$metadata))

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_write always creates a version ID", {
  alias_name <- "test_version_id"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  out <- pip_write(letters, id = "z", alias = alias_name)
  expect_true(!is.null(out$version_id))
  expect_type(out$version_id, "character")

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read preserves content integrity and type", {
  ctx <- create_test_alias("test_integrity")

  res <- pip_read("x", alias = ctx$alias)
  expect_true(is.integer(res))
  expect_equal(res, 4:9)

  # clean up
  fs::dir_delete(ctx$dir)
})

test_that("pip_read works with specific version ID", {
  ctx <- create_test_alias("test_specific_ver")
  vers <- pip_read("x", alias = ctx$alias, version = "available")
  version_id <- vers$version_id[2] # pick second version
  res <- pip_read("x", alias = ctx$alias, version = version_id)
  expect_equal(res, 1:10)

  # clean up
  fs::dir_delete(ctx$dir)
})

test_that("pip_read can load artifacts when format is explicitly specified", {
  alias_name <- "test_multi_fmt"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  # Write the same artifact in two different formats
  pip_write(letters[1:3], id = "fmt_test", alias = alias_name, format = "qs2")
  Sys.sleep(1)
  pip_write(letters[4:6], id = "fmt_test", alias = alias_name, format = "rds")

  # Specifying format should load the correct object
  expect_equal(
    pip_read("fmt_test", alias = alias_name, format = "qs2"),
    letters[1:3]
  )
  expect_equal(
    pip_read("fmt_test", alias = alias_name, format = "rds"),
    letters[4:6]
  )

  # clean up
  fs::dir_delete(dir)
})


test_that("pip_read auto-detects single existing format when id has no extension", {
  alias_name <- "test_single_fmt"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  # Write artifact in a single format (qs2)
  pip_write(mtcars[1:3, ], id = "single_fmt", alias = alias_name)

  # Calling pip_read without format should auto-detect and load the qs2 file
  res <- pip_read("single_fmt", alias = alias_name)
  expect_true(is.data.frame(res) || is.data.table(res))
  expect_equal(nrow(res), 3)

  # clean up
  fs::dir_delete(dir)
})


test_that("pip_read handles id with extension and respects explicit format", {
  alias_name <- "test_id_ext"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  # Write artifact with explicit extension
  pip_write(iris[1:5, ], id = "with_ext.qs2", alias = alias_name)

  # When id includes the extension and format = NULL, it should load the file
  res1 <- pip_read("with_ext.qs2", alias = alias_name, format = NULL)
  expect_equal(nrow(res1), 5)

  # If the caller requests a different format, it should error (file not found)
  expect_error(pip_read("with_ext.qs2", alias = alias_name, format = "rds"))

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_write and pip_read work with directory structure in id", {
  alias_name <- "test_dir_structure"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  # Write with subdirectory in id
  pip_write(1:10, id = "data/nested.qs2", alias = alias_name)

  # Read back
  res <- pip_read("data/nested.qs2", alias = alias_name)
  expect_equal(res, 1:10)

  # clean up
  fs::dir_delete(dir)
})

test_that("pip_read and pip_write work with pk parameter", {
  alias_name <- "test_pk"
  dir <- fs::path_temp(alias_name)
  fs::dir_create(dir)
  stamp::st_init(dir, alias = alias_name)

  # Write with pk
  df <- data.frame(id = 1:3, value = c("a", "b", "c"))
  out <- pip_write(df, id = "with_pk", alias = alias_name, pk = "id")
  expect_true(!is.null(out$version_id))

  # Read back
  res <- pip_read("with_pk", alias = alias_name)
  expect_equal(nrow(res), 3)
  expect_true("id" %in% names(res))

  # clean up
  fs::dir_delete(dir)
})

# -------------------------------
# Cleanup
# -------------------------------
# Optionally delete temp directories after tests
