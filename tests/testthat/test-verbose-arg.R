library(testthat)
library(data.table)

# -------------------------------------------------------------------------
# pip_find_cache()
# -------------------------------------------------------------------------

test_that("pip_find_cache accepts verbose parameter", {
  expect_true("verbose" %in% names(formals(pip_find_cache)))
})

test_that("pip_find_cache default verbose is getOption('pipload.verbose')", {
  default_expr <- deparse(formals(pip_find_cache)$verbose)
  expect_match(default_expr, "getOption")
  expect_match(default_expr, "pipload.verbose")
})

test_that("pip_find_cache emits cli message when verbose = TRUE", {
  local_mocked_bindings(
    pip_load_cache_inventory = function(...) {
      data.table(cache_id = c("ARG_2020_EH_INC_D1_PC", "BOL_2019_EH_CON_D1_PC"))
    },
    .package = "pipload"
  )
  expect_message(pip_find_cache(verbose = TRUE), regexp = "Found")
})

test_that("pip_find_cache is silent when verbose = FALSE", {
  local_mocked_bindings(
    pip_load_cache_inventory = function(...) {
      data.table(cache_id = c("ARG_2020_EH_INC_D1_PC", "BOL_2019_EH_CON_D1_PC"))
    },
    .package = "pipload"
  )
  expect_no_message(pip_find_cache(verbose = FALSE))
})

# -------------------------------------------------------------------------
# pip_load_dlw_inventory()
# -------------------------------------------------------------------------

test_that("pip_load_dlw_inventory accepts verbose parameter", {
  expect_true("verbose" %in% names(formals(pip_load_dlw_inventory)))
})

test_that("pip_load_dlw_inventory default verbose is getOption('pipload.verbose')", {
  default_expr <- deparse(formals(pip_load_dlw_inventory)$verbose)
  expect_match(default_expr, "getOption")
  expect_match(default_expr, "pipload.verbose")
})

test_that("pip_load_dlw_inventory emits message when verbose = TRUE", {
  tmp <- withr::local_tempdir()
  inv_dir <- file.path(tmp, "_Inventory")
  dir.create(inv_dir, recursive = TRUE)
  inv_file <- file.path(inv_dir, "dlw_inventory.fst")
  fst::write_fst(data.frame(fullname = "ARG/data.qs2"), inv_file)

  expect_message(
    pip_load_dlw_inventory(root_dir = tmp, dlw_dir = tmp, verbose = TRUE),
    regexp = "Loading"
  )
})

test_that("pip_load_dlw_inventory is silent when verbose = FALSE", {
  tmp <- withr::local_tempdir()
  inv_dir <- file.path(tmp, "_Inventory")
  dir.create(inv_dir, recursive = TRUE)
  inv_file <- file.path(inv_dir, "dlw_inventory.fst")
  fst::write_fst(data.frame(fullname = "ARG/data.qs2"), inv_file)

  expect_no_message(
    pip_load_dlw_inventory(root_dir = tmp, dlw_dir = tmp, verbose = FALSE)
  )
})

# -------------------------------------------------------------------------
# Five zero-arg DLW loaders
# -------------------------------------------------------------------------

test_that("load_dlw_gmd_inventory accepts verbose and emits message when TRUE", {
  expect_true("verbose" %in% names(formals(load_dlw_gmd_inventory)))
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_message(load_dlw_gmd_inventory(verbose = TRUE), regexp = "dlw_gmd_inv")
})

test_that("load_dlw_gmd_inventory is silent when verbose = FALSE", {
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_no_message(load_dlw_gmd_inventory(verbose = FALSE))
})

test_that("load_dlw_gmd_log accepts verbose and emits message when TRUE", {
  expect_true("verbose" %in% names(formals(load_dlw_gmd_log)))
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_message(load_dlw_gmd_log(verbose = TRUE), regexp = "dlw_gmd_log")
})

test_that("load_dlw_gmd_log is silent when verbose = FALSE", {
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_no_message(load_dlw_gmd_log(verbose = FALSE))
})

test_that("load_gmd_valid_inv accepts verbose and emits message when TRUE", {
  expect_true("verbose" %in% names(formals(load_gmd_valid_inv)))
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_message(load_gmd_valid_inv(verbose = TRUE), regexp = "gmd_valid_inv")
})

test_that("load_gmd_valid_inv is silent when verbose = FALSE", {
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_no_message(load_gmd_valid_inv(verbose = FALSE))
})

test_that("load_gmd_valid_log accepts verbose and emits message when TRUE", {
  expect_true("verbose" %in% names(formals(load_gmd_valid_log)))
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_message(
    load_gmd_valid_log(verbose = TRUE),
    regexp = "dlw_validation_log"
  )
})

test_that("load_gmd_valid_log is silent when verbose = FALSE", {
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_no_message(load_gmd_valid_log(verbose = FALSE))
})

test_that("load_gmd_valid_report accepts verbose and emits message when TRUE", {
  expect_true("verbose" %in% names(formals(load_gmd_valid_report)))
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_message(
    load_gmd_valid_report(verbose = TRUE),
    regexp = "validation_report"
  )
})

test_that("load_gmd_valid_report is silent when verbose = FALSE", {
  local_mocked_bindings(
    get_pip_folders = function(...) "/fake/dir",
    .package = "pipfun"
  )
  local_mocked_bindings(
    st_alias_list = function() data.table(root = "/fake/dir", alias = "fake"),
    .package = "stamp"
  )
  local_mocked_bindings(
    pip_read = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  expect_no_message(load_gmd_valid_report(verbose = FALSE))
})

# -------------------------------------------------------------------------
# pip_load_all_aux() — default alignment and cascade
# -------------------------------------------------------------------------

test_that("pip_load_all_aux default verbose is getOption('pipload.verbose')", {
  default_expr <- deparse(formals(pip_load_all_aux)$verbose)
  expect_match(default_expr, "getOption")
  expect_match(default_expr, "pipload.verbose")
})

test_that("pip_load_all_aux verbose = FALSE silences the full loop (cascade check)", {
  local_mocked_bindings(
    pip_load_aux = function(...) data.table(x = 1L),
    .package = "pipload"
  )
  local_mocked_bindings(
    pip_add_aux_labels = function(dt, ...) dt,
    .package = "pipload"
  )
  expect_no_message(
    pip_load_all_aux(aux = c("cpi", "ppp"), verbose = FALSE, envir = new.env())
  )
})

test_that("pip_load_all_aux verbose = TRUE fires messages per aux item (cascade check)", {
  local_mocked_bindings(
    pip_load_aux = function(measure, verbose, ...) {
      if (isTRUE(verbose)) {
        cli::cli_alert_info("loading {measure}")
      }
      data.table(x = 1L)
    },
    .package = "pipload"
  )
  local_mocked_bindings(
    pip_add_aux_labels = function(dt, ...) dt,
    .package = "pipload"
  )
  expect_message(
    pip_load_all_aux(aux = c("cpi", "ppp"), verbose = TRUE, envir = new.env())
  )
})

# -------------------------------------------------------------------------
# pip_find_cache()
# -------------------------------------------------------------------------

test_that("pip_find_cache accepts verbose parameter", {
  expect_true("verbose" %in% names(formals(pip_find_cache)))
})

test_that("pip_find_cache default verbose is getOption('pipload.verbose')", {
  default_expr <- deparse(formals(pip_find_cache)$verbose)
  expect_match(default_expr, "getOption")
  expect_match(default_expr, "pipload.verbose")
})

test_that("pip_find_cache emits cli message when verbose = TRUE", {
  local_mocked_bindings(
    pip_load_cache_inventory = function(...) {
      data.table(cache_id = c("ARG_2020_EH_INC_D1_PC", "BOL_2019_EH_CON_D1_PC"))
    },
    .package = "pipload"
  )
  expect_message(
    pip_find_cache(verbose = TRUE),
    regexp = "Found"
  )
})

test_that("pip_find_cache is silent when verbose = FALSE", {
  local_mocked_bindings(
    pip_load_cache_inventory = function(...) {
      data.table(cache_id = c("ARG_2020_EH_INC_D1_PC", "BOL_2019_EH_CON_D1_PC"))
    },
    .package = "pipload"
  )
  expect_no_message(pip_find_cache(verbose = FALSE))
})

# -------------------------------------------------------------------------
# pip_load_dlw_inventory()
# -------------------------------------------------------------------------

test_that("pip_load_dlw_inventory accepts verbose parameter", {
  expect_true("verbose" %in% names(formals(pip_load_dlw_inventory)))
})

test_that("pip_load_dlw_inventory default verbose is getOption('pipload.verbose')", {
  default_expr <- deparse(formals(pip_load_dlw_inventory)$verbose)
  expect_match(default_expr, "getOption")
  expect_match(default_expr, "pipload.verbose")
})

test_that("pip_load_dlw_inventory emits message when verbose = TRUE", {
  tmp <- withr::local_tempdir()
  inv_dir <- file.path(tmp, "_Inventory")
  dir.create(inv_dir, recursive = TRUE)
  inv_file <- file.path(inv_dir, "dlw_inventory.fst")
  fst::write_fst(data.frame(fullname = "ARG/data.qs2"), inv_file)

  expect_message(
    pip_load_dlw_inventory(root_dir = tmp, dlw_dir = tmp, verbose = TRUE),
    regexp = "Loading"
  )
})

test_that("pip_load_dlw_inventory is silent when verbose = FALSE", {
  tmp <- withr::local_tempdir()
  inv_dir <- file.path(tmp, "_Inventory")
  dir.create(inv_dir, recursive = TRUE)
  inv_file <- file.path(inv_dir, "dlw_inventory.fst")
  fst::write_fst(data.frame(fullname = "ARG/data.qs2"), inv_file)

  expect_no_message(
    pip_load_dlw_inventory(root_dir = tmp, dlw_dir = tmp, verbose = FALSE)
  )
})
