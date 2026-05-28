# Helper: write a metadata list to a stamp-layout temp path and return an inv
# data.table row pointing at it.
make_inv_with_meta <- function(pip_id, meta, dir) {
  vid <- "abc123"
  artifact_dir <- fs::path(dir, "versions", vid)
  fs::dir_create(artifact_dir)
  artifact_path <- fs::path(artifact_dir, "artifact")
  qs2::qs_save(meta, artifact_path)

  data.table::data.table(
    pip_id = pip_id,
    path_metadata = dir,
    version_id_metadata = vid
  )
}

# ---------------------------------------------------------------------------
# Early-exit and validation
# ---------------------------------------------------------------------------

test_that("pip_inv_enrich returns inv unchanged when fields is empty", {
  inv <- data.table::data.table(pip_id = "BOL_2022_EH_INC_ALL")
  result <- pip_inv_enrich(inv, fields = character(0))
  expect_identical(result, inv)
})

test_that("pip_inv_enrich aborts on invalid field name", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    path_metadata = "x",
    version_id_metadata = "v1"
  )
  expect_error(
    pip_inv_enrich(inv, fields = "not_a_real_field"),
    class = "pip_inv_enrich_invalid_field"
  )
})

test_that("pip_inv_enrich aborts when path_metadata is missing", {
  inv <- data.table::data.table(pip_id = "BOL_2022_EH_INC_ALL")
  expect_error(
    pip_inv_enrich(inv, fields = "reporting_level"),
    class = "pip_inv_enrich_no_path_metadata"
  )
})

test_that("pip_inv_enrich skips fields already in inv with inform", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    path_metadata = "x",
    version_id_metadata = NA_character_,
    reporting_level = "existing"
  )
  expect_message(
    result <- pip_inv_enrich(inv, fields = "reporting_level"),
    class = "pip_inv_enrich_skip_existing"
  )
  # inv unchanged — field was skipped
  expect_equal(result$reporting_level, "existing")
})

# ---------------------------------------------------------------------------
# Scalar field extraction
# ---------------------------------------------------------------------------

test_that("pip_inv_enrich extracts scalar field from temp metadata", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    reporting_level = "2",
    cpi = c(`2022_national` = 1.0)
  )
  inv <- make_inv_with_meta("BOL_2022_EH_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "reporting_level")
  expect_contains(names(result), "reporting_level")
  expect_equal(result$reporting_level, "2")
})

test_that("pip_inv_enrich extracts multiple scalar fields at once", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  meta1 <- list(
    surveyid_year = 2022,
    reporting_level = "1",
    distribution_type = "micro"
  )
  meta2 <- list(
    surveyid_year = 2015,
    reporting_level = "2",
    distribution_type = "group"
  )
  inv <- data.table::rbindlist(list(
    make_inv_with_meta("BOL_2022_EH_INC_ALL", meta1, dir1),
    make_inv_with_meta("CHN_2015_NSS_INC_ALL", meta2, dir2)
  ))

  result <- pip_inv_enrich(
    inv,
    fields = c("reporting_level", "distribution_type")
  )
  expect_equal(result$reporting_level, c("1", "2"))
  expect_equal(result$distribution_type, c("micro", "group"))
})

# ---------------------------------------------------------------------------
# NA handling
# ---------------------------------------------------------------------------

test_that("pip_inv_enrich gives NA when version_id_metadata is NA", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    path_metadata = "irrelevant",
    version_id_metadata = NA_character_
  )
  expect_warning(
    result <- pip_inv_enrich(inv, fields = "reporting_level"),
    class = "pip_inv_enrich_missing_meta"
  )
  expect_contains(names(result), "reporting_level")
  expect_true(is.na(result$reporting_level))
})

test_that("pip_inv_enrich gives NA when artifact file is missing", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    path_metadata = withr::local_tempdir(),
    version_id_metadata = "no_such_version"
  )
  # artifact path does not exist → qs_read fails → per-file read_error warning,
  # then aggregate missing_meta warning.
  expect_warning(
    expect_warning(
      result <- pip_inv_enrich(inv, fields = "reporting_level"),
      class = "pip_inv_enrich_read_error"
    ),
    class = "pip_inv_enrich_missing_meta"
  )
  expect_true(is.na(result$reporting_level))
})

# ---------------------------------------------------------------------------
# cpi vector expansion
# ---------------------------------------------------------------------------

test_that("cpi field expands to cpi_YYYY_area columns", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    cpi = c(`2005_rural` = 1.1, `2011_urban` = 0.9)
  )
  inv <- make_inv_with_meta("CHN_2022_X_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "cpi")
  expect_contains(names(result), "cpi_2005_rural")
  expect_contains(names(result), "cpi_2011_urban")
  expect_equal(result$cpi_2005_rural, 1.1)
  expect_equal(result$cpi_2011_urban, 0.9)
})

# ---------------------------------------------------------------------------
# ppp vector expansion (no prefix doubling)
# ---------------------------------------------------------------------------

test_that("ppp field uses existing names without doubling prefix", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    ppp = c(`ppp_2011_02_02_national` = 3.697, `ppp_2017_01_02_rural` = 3.495)
  )
  inv <- make_inv_with_meta("CHN_2022_X_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "ppp")
  expect_contains(names(result), "ppp_2011_02_02_national")
  expect_contains(names(result), "ppp_2017_01_02_rural")
  # No ppp_ppp_ prefix doubling
  expect_false(any(grepl("^ppp_ppp_", names(result))))
  expect_equal(result$ppp_2011_02_02_national, 3.697)
})

# ---------------------------------------------------------------------------
# pop / gdp / pce year-stripping
# ---------------------------------------------------------------------------

test_that("pop strips year when it matches surveyid_year", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    pop = c(`2022_rural` = 514e6, `2022_national` = 1412e6)
  )
  inv <- make_inv_with_meta("CHN_2022_X_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "pop")
  expect_contains(names(result), "pop_rural")
  expect_contains(names(result), "pop_national")
  expect_false("pop_2022_rural" %in% names(result))
  expect_false("pop_year" %in% names(result))
})

test_that("pop keeps full name and adds pop_year when year mismatches", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    pop = c(`2019_national` = 1000e6)
  )
  inv <- make_inv_with_meta("CHN_2022_X_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "pop")
  expect_contains(names(result), "pop_2019_national")
  expect_contains(names(result), "pop_year")
  expect_equal(result$pop_year, "2019")
})

# ---------------------------------------------------------------------------
# union of columns across surveys with different areas
# ---------------------------------------------------------------------------

test_that("two surveys with different pop areas produce union of columns with NAs", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()
  meta1 <- list(surveyid_year = 2022,
                pop = c(`2022_rural` = 500e6, `2022_urban` = 900e6))
  meta2 <- list(surveyid_year = 2015,
                pop = c(`2015_national` = 1200e6))
  inv <- data.table::rbindlist(list(
    make_inv_with_meta("CHN_2022_X_INC_ALL", meta1, dir1),
    make_inv_with_meta("IND_2015_X_INC_ALL", meta2, dir2)
  ))

  result <- pip_inv_enrich(inv, fields = "pop")
  # CHN row: pop_rural and pop_urban present, pop_national NA
  expect_contains(names(result), "pop_rural")
  expect_contains(names(result), "pop_urban")
  expect_contains(names(result), "pop_national")
  expect_true(is.na(result[pip_id == "CHN_2022_X_INC_ALL", pop_national]))
  expect_true(is.na(result[pip_id == "IND_2015_X_INC_ALL", pop_rural]))
})

# ---------------------------------------------------------------------------
# Mixed scalar + vector in one call
# ---------------------------------------------------------------------------

test_that("mixed scalar and vector fields work together", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    reporting_level = "national",
    cpi = c(`2005_national` = 1.2, `2011_national` = 1.1)
  )
  inv <- make_inv_with_meta("BOL_2022_EH_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = c("reporting_level", "cpi"))
  expect_contains(names(result), "reporting_level")
  expect_contains(names(result), "cpi_2005_national")
  expect_contains(names(result), "cpi_2011_national")
  expect_equal(result$reporting_level, "national")
})

# ---------------------------------------------------------------------------
# version_id_metadata column absent (P2.16)
# ---------------------------------------------------------------------------

test_that("pip_inv_enrich warns with pip_inv_enrich_no_version_col when column absent", {
  inv <- data.table::data.table(
    pip_id = "BOL_2022_EH_INC_ALL",
    path_metadata = "some/path"
    # No version_id_metadata column
  )
  # Emits no_version_col first, then aggregate missing_meta.
  expect_warning(
    expect_warning(
      result <- pip_inv_enrich(inv, fields = "reporting_level"),
      class = "pip_inv_enrich_no_version_col"
    ),
    class = "pip_inv_enrich_missing_meta"
  )
  expect_contains(names(result), "reporting_level")
  expect_true(is.na(result$reporting_level))
})

# ---------------------------------------------------------------------------
# gdp / pce vector field coverage (P2.17)
# ---------------------------------------------------------------------------

test_that("gdp strips year when it matches surveyid_year", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    gdp = c(`2022_national` = 2100)
  )
  inv <- make_inv_with_meta("CHN_2022_X_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "gdp")
  expect_contains(names(result), "gdp_national")
  expect_false("gdp_2022_national" %in% names(result))
  expect_false("gdp_year" %in% names(result))
})

test_that("pce keeps full name and adds pce_year when year mismatches", {
  dir <- withr::local_tempdir()
  meta <- list(
    surveyid_year = 2022,
    pce = c(`2018_national` = 500)
  )
  inv <- make_inv_with_meta("CHN_2022_X_INC_ALL", meta, dir)

  result <- pip_inv_enrich(inv, fields = "pce")
  expect_contains(names(result), "pce_2018_national")
  expect_contains(names(result), "pce_year")
  expect_equal(result$pce_year, "2018")
})

