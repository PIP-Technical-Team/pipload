library(pins)

# Helper: create a temp board and write multiple versions
create_test_board <- function() {
  board <- board_temp(versioned = TRUE)
  name <- "x"
  pip_write(board, x = 1:5, name = name)
  Sys.sleep(1)
  pip_write(board, x = 1:10, name = name)
  Sys.sleep(1)
  pip_write(board, x = 4:9, name = name)
  list(board = board, name = name)
}

# pip_write and pip_read basic functionality

test_that("pip_write and pip_read work for latest version", {
  env <- create_test_board()
  expect_equal(pip_read(env$board, env$name), 4:9)
})

test_that("pip_read returns previous versions by negative index", {
  env <- create_test_board()
  expect_equal(pip_read(env$board, env$name, version = -1), 1:10)
  expect_equal(pip_read(env$board, env$name, version = -2), 1:5)
})

test_that("pip_read returns available versions as a data.table", {
  env <- create_test_board()
  vers <- pip_read(env$board, env$name, version = "available")
  expect_s3_class(vers, "data.table")
  expect_true(nrow(vers) >= 3)
})

test_that("pip_read errors on positive version index", {
  env <- create_test_board()
  expect_error(pip_read(env$board, env$name, version = 1),
               "can't be a positive number")
})

test_that("pip_read errors on missing pin", {
  board <- board_temp(versioned = TRUE)
  expect_error(pip_read(board, "not_a_pin"),
               "does not exist")
})

# get_pin_versions

test_that("get_pin_versions returns correct structure and order", {
  env <- create_test_board()
  vers <- get_pin_versions(env$board, env$name)
  expect_s3_class(vers, "data.table")
  expect_true(all(c("vintage", "ver", "created") %in% names(vers)))
  expect_true(vers$vintage[1] == 0)
  expect_true(vers$vintage[2] == -1)
})

test_that("get_pin_versions errors on missing pin", {
  board <- board_temp(versioned = TRUE)
  expect_error(get_pin_versions(board, "not_a_pin"))
})

# filter_version

test_that("filter_version works for character and numeric input", {
  env <- create_test_board()
  vr <- get_pin_versions(env$board, env$name)
  # Use version string
  expect_equal(filter_version(vr, vr$ver[1]), vr$ver[1])
  # Use negative index
  expect_equal(filter_version(vr, -1), vr$ver[2])
})

test_that("filter_version errors for unavailable version", {
  env <- create_test_board()
  vr <- get_pin_versions(env$board, env$name)
  expect_error(filter_version(vr, "not_a_version"))
  expect_error(filter_version(vr, -99))
})

test_that("filter_version errors for ambiguous version", {
  env <- create_test_board()
  vr <- get_pin_versions(env$board, env$name)
  # Duplicate a version row to force ambiguity
  vr2 <- rbind(vr, vr[1,])
  expect_error(filter_version(vr2, vr2$ver[1]), "returns more than one version")
})