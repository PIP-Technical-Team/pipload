# -------------------------------
# Helper: create temp directory and write multiple versions
# -------------------------------
create_test_dir <- function() {
  dir <- fs::path_temp()
  fs::dir_create(dir)

  pip_write(1:5,  id = "x", dir = dir)
  Sys.sleep(1)
  pip_write(1:10, id = "x", dir = dir)
  Sys.sleep(1)
  pip_write(4:9,  id = "x", dir = dir)

  dir
}

# -------------------------------
# Run tests with PK warnings suppressed
# -------------------------------
withr::local_options(list())  # just placeholder; not strictly needed

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
  expect_equal(pip_read("x", dir = dir), 4:9)
})

test_that("pip_read returns previous versions by negative index", {
  dir <- create_test_dir()
  expect_equal(pip_read("x", dir = dir, version = -1), 1:10)
  expect_equal(pip_read("x", dir = dir, version = -2), 1:5)
})

test_that("pip_read returns available versions as a data.table", {
  dir <- create_test_dir()
  vers <- pip_read("x", dir = dir, version = "available")
  expect_s3_class(vers, "data.table")
  expect_true(nrow(vers) >= 3)
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
