# init parameters
rqr_sect          <- c("release", "ppp_year", "ppp_rv", "ppp_av", "identity")
vintage_ch        <- "20220315_2017_01_01_TEST"
vintage_lt        <- data.table::tstrsplit(vintage_ch, "_")
names(vintage_lt) <- rqr_sect


ppp_v <- pip_load_aux("ppp",
                      suffix = "vintage",
                      verbose = FALSE)

# remove Vs in case they are available and add zeros
ver_vars <- c("ppp_rv", "ppp_av")
ppp_v[,
      (ver_vars) := lapply(.SD, function(x) {
        x <-  gsub("[Vv]", "", x)

        x <- fifelse(nchar(x) == 1, paste0("0",x), x)
      }),
      .SDcols = ver_vars]


test_that("basic convertion works", {
  # convert list
  expect_equal(pip_create_vintage(vintage = vintage_lt), vintage_ch)

  # convert character vector
  expect_equal(pip_create_vintage(vintage = vintage_ch), vintage_ch)

  # works with data frames
  vintage_DT <- data.table::as.data.table(vintage_lt)
  vintage_DF <- as.data.frame(vintage_lt)

  expect_equal(pip_create_vintage(vintage = vintage_DT), vintage_ch)
  expect_equal(pip_create_vintage(vintage = vintage_DF), vintage_ch)


})



test_that("wrong inputs triggers error", {

  # bad names in list
  expect_error(pip_create_vintage(vintage = list(test = "hola", identity = "chao")))

  # numbers\
  expect_error(pip_create_vintage(vintage = 12314))

  # if name provided, then other names should not work
  expect_error(pip_create_vintage(vintage = list(name = "hola", identity = "chao")))


  # Wrong length in character vectors sections
  wrong_names <-
  c("2220315_2017_01_01_TEST",  # wrong  release
    "20220315_2017d_01_01_TEST", # wrong year
    "20220315_20174_01_01_TEST", # wrong year
    "20220315_2017d_014_01_TEST", # wrong
    "20220315_2017d__01_TEST", # wrong
    "20220315_2017d_0f_01_TEST", # wrong
    "20220315_2017d_01f_01_TEST", # wrong
    "20220315_2017d_01_0_TEST", # wrong
    "20220315_2017d_01_144_TEST", # wrong
    "20220315_2017d_01__TEST", # wrong
    "20220315_2017d_01_01_43fjk", # wrong
    "20220315_2017d_01_01_ds434"
    )


    purrr::walk(wrong_names,
                .f = ~{
                  expect_error(pip_create_vintage(vintage = .x),
                               label = .x)
                  })

})



test_that("Release is working fine", {


  vt2         <- vintage_lt
  vt2$release <-  "2022035" # short
  expect_error(pip_create_vintage(vintage = vt2),
               label = vt2$release)



  vt2$release <-  "2022035r" # letters
  expect_error(pip_create_vintage(vintage = vt2),
               label = vt2$release)



  vt2$release <-  "" # empty
  expect_error(pip_create_vintage(vintage = vt2),
               label = vt2$release)


  # when NULL and sorting
  vt2$release <-  NULL # short
  expect_equal(pip_create_vintage(vintage = vt2),
               paste0(format(Sys.Date(), "%Y%m%d"), "_2017_01_01_TEST"))

})


test_that("PPP year is working fine", {

  vt3          <- vintage_lt
  vt3$ppp_year <-  "215" # short
  expect_error(pip_create_vintage(vintage = vt3),
               label = vt3$ppp_year)



  vt3$ppp_year <-  "20r4" # letters
  expect_error(pip_create_vintage(vintage = vt3),
               label = vt3$ppp_year)


  # Empty
  vt3$ppp_year <-  "" # empty
  expect_error(pip_create_vintage(vintage = vt3),
               label = vt3$ppp_year)


  # when NULL and sorting
  vt3$ppp_year <-  NULL # short
  m_py <- as.character(ppp_v[, max(ppp_year)])

  expect_equal(pip_create_vintage(vintage = vt3),
               gsub(pattern     = "(\\d{8})_(\\d+)_(.+)",
                    replacement = paste("\\1", m_py, "\\3", sep = "_"),
                    x           = vintage_ch))


  # works with numeric values
  vt3$ppp_year <-  as.numeric(vintage$ppp_year) # short
  expect_equal(pip_create_vintage(vintage = vt3), vintage_ch)

  # Wrong year
  vt3$ppp_year <-  "2016"
  expect_error(pip_create_vintage(vintage = vt3),
               label = vt3$ppp_year)

})



test_that("Release version of PPP works fine", {


  vt4          <- vintage_lt
  vt4$ppp_rv <-  "" # short
  expect_error(pip_create_vintage(vintage = vt4),
               label = vt4$ppp_rv)



  vt4$ppp_rv <-  "r4" # letters
  expect_error(pip_create_vintage(vintage = vt4),
               label = vt4$ppp_rv)

  # Empty
  vt4$ppp_rv <-  "398" # long
  expect_error(pip_create_vintage(vintage = vt4),
               label = vt4$ppp_rv)


  # when NULL and sorting
  vt4$ppp_rv <-  NULL # short
  m_rv <- ppp_v[ppp_year == vt4$ppp_year, max(ppp_rv)]

  expect_equal(pip_create_vintage(vintage = vt4),
               gsub(pattern     = "(.+)_(\\d{1,2})",
                    replacement = paste("\\1", m_rv, sep = "_"),
                    x           = vintage_ch)
               )

  # works with numeric values
  vt4$ppp_rv <-  as.numeric(vintage$ppp_rv) # short
  expect_equal(pip_create_vintage(vintage = vt4), vintage_ch)

  vt4$ppp_rv <-  "1" # just one value
  expect_equal(pip_create_vintage(vintage = vt4), vintage_ch)

  # Wrong year
  vt4$ppp_rv <-  "5"
  expect_error(pip_create_vintage(vintage = vt4),
               label = vt4$ppp_rv)

})


test_that("Adaptation version of PPP works fine", {


  vt5          <- vintage_lt
  vt5$ppp_av <-  "" # short
  expect_error(pip_create_vintage(vintage = vt5),
               label = vt5$ppp_av)



  vt5$ppp_av <-  "r4" # letters
  expect_error(pip_create_vintage(vintage = vt5),
               label = vt5$ppp_av)

  # Empty
  vt5$ppp_av <-  "398" # long
  expect_error(pip_create_vintage(vintage = vt5),
               label = vt5$ppp_av)


  # when NULL and sorting
  vt5$ppp_av <-  NULL # short
  m_av <- ppp_v[ppp_year == vt5$ppp_year & ppp_rv == vt5$ppp_rv,
                max(ppp_av)]

  expect_equal(pip_create_vintage(vintage = vt5),
               gsub(pattern     = "(.+)_(\\d{1,2})_([[:alpha:]]+)",
                    replacement = paste("\\1", m_av, "\\3", sep = "_"),
                    x           = vintage_ch)
               )

  # works with numeric values
  vt5$ppp_av <-  as.numeric(vintage$ppp_av) # short
  expect_equal(pip_create_vintage(vintage = vt5), vintage_ch)

  vt5$ppp_av <-  "1" # just one value
  expect_equal(pip_create_vintage(vintage = vt5), vintage_ch)

  # Wrong value
  vt5$ppp_av <-  "5"
  expect_error(pip_create_vintage(vintage = vt5),
               label = vt5$ppp_av)

})


test_that("default is working", {

  m_py <- ppp_v[, max(ppp_year)]
  m_rv <- ppp_v[ppp_year == m_py, max(ppp_rv)]
  m_av <- ppp_v[ppp_year == m_py & ppp_rv == m_rv,
                max(ppp_av)]

  expect_equal(pip_create_vintage(), paste(format(Sys.Date(), "%Y%m%d"),
                                           m_py, m_rv, m_av, "PROD",
                                           sep = "_")
               )

})
