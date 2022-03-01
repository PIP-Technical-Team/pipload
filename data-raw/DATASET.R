## code to prepare `DATASET` dataset goes here



md <- pipload::pip_load_data("PRY", 2015)
md_ex <- md[1:10]

readr::write_rds(x    = md_ex,
                 file = "tests/testdata/md_ex.rds")

gd <- pipload::pip_load_data("CHN", 2015)
gd_ex <- gd[1:10]

readr::write_rds(x    = gd_ex,
                 file = "tests/testdata/gd_ex.rds")




