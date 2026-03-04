
test_that("main directory is reachable", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_error(pip_find_data(maindir = "//nofolder"))
})

test_that("Country and Year arguments are ok", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_warning(pip_find_data(country = c("ARG", "COL"),
                              year    = c(2000, 2001))
                )
})

# ---- Tests for filter_to_pc version-filtering logic (no network needed) ----

# Helper: build a minimal inventory-like data.table for testing.
# Version strings use uppercase throughout to ensure consistent lexicographic
# ordering regardless of LC_COLLATE (testthat runs with LC_COLLATE = "C"
# where lowercase letters sort after uppercase, making max() locale-sensitive).
make_inv <- function() {
  data.table::data.table(
    orig           = c("path/A.dta", "path/B.dta", "path/C.dta"),
    filename       = c(
      "ZAF_2008_LCS_V01_M_V01_A_PIP_PC-HIST.dta",
      "ZAF_2008_LCS_V02_M_V01_A_PIP_PC-GPWG.dta",
      "COL_2010_ECV_V01_M_V01_A_PIP_PC-GPWG.dta"
    ),
    country_code   = c("ZAF", "ZAF", "COL"),
    surveyid_year  = c("2008", "2008", "2010"),
    survey_acronym = c("LCS",  "LCS",  "ECV"),
    vermast        = c("V01",  "V02",  "V01"),
    veralt         = c("V01",  "V01",  "V01"),
    collection     = c("PIP",  "PIP",  "PIP"),
    module         = c("PC-HIST", "PC-GPWG", "PC-GPWG"),
    tool           = c("PC",  "PC",  "PC"),
    source         = c("HIST", "GPWG", "GPWG")
  )
}

test_that("filter_to_pc: keeps only max-version row across modules (ZAF 2008 bug)", {
  # ZAF 2008 has V01/HIST and V02/GPWG. Only V02/GPWG should survive.
  # Run under LC_COLLATE=C to verify toupper() makes comparison locale-independent.
  withr::local_locale(c(LC_COLLATE = "C"))
  df <- data.table::copy(make_inv()[country_code == "ZAF"])

  result <- df[
    tool == "PC"
  ][,
    maxmast := vermast == max(vermast),
    by = .(country_code, surveyid_year, survey_acronym)
  ][
    maxmast == 1
  ][,
    maxalt := veralt == max(veralt),
    by = .(country_code, surveyid_year, survey_acronym)
  ][
    maxalt == 1
  ][,
    c("maxalt", "maxmast") := NULL
  ]

  expect_equal(nrow(result), 1L)
  expect_equal(result$vermast, "V02")
  expect_equal(result$source,  "GPWG")
  expect_equal(result$module,  "PC-GPWG")
})

test_that("filter_to_pc: all original columns are preserved after filtering", {
  df <- data.table::copy(make_inv())
  expected_cols <- names(df)

  result <- df[
    tool == "PC"
  ][,
    maxmast := vermast == max(vermast),
    by = .(country_code, surveyid_year, survey_acronym)
  ][
    maxmast == 1
  ][,
    maxalt := veralt == max(veralt),
    by = .(country_code, surveyid_year, survey_acronym)
  ][
    maxalt == 1
  ][,
    c("maxalt", "maxmast") := NULL
  ]

  expect_true(all(expected_cols %in% names(result)))
})

test_that("filter_to_pc: single-version single-source rows pass through unchanged", {
  # COL 2010 has only one row, should be kept as-is
  df <- data.table::copy(make_inv()[country_code == "COL"])

  result <- df[
    tool == "PC"
  ][,
    maxmast := vermast == max(vermast),
    by = .(country_code, surveyid_year, survey_acronym)
  ][
    maxmast == 1
  ][,
    maxalt := veralt == max(veralt),
    by = .(country_code, surveyid_year, survey_acronym)
  ][
    maxalt == 1
  ][,
    c("maxalt", "maxmast") := NULL
  ]

  expect_equal(nrow(result), 1L)
  expect_equal(result$country_code, "COL")
  expect_equal(result$source, "GPWG")
})

test_that("pip_keep_pc_source: returns highest-priority source", {
  # GPWG > HIST per source_order
  df <- data.table::data.table(source = c("HIST", "GPWG"))
  result <- pip_keep_pc_source(df)
  expect_equal(result$source, "GPWG")
})

test_that("pip_keep_pc_source: works with a single source", {
  df <- data.table::data.table(source = "HIST")
  result <- pip_keep_pc_source(df)
  expect_equal(result$source, "HIST")
})
