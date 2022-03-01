## code to prepare `DATASET` dataset goes here



md <- pipload::pip_load_data("PRY", 2015)
md_ex <- md[1:10]
usethis::use_data(md_ex, overwrite = TRUE, internal = TRUE)

gd <- pipload::pip_load_data("CHN", 2015)
gd_ex <- gd[1:10]
usethis::use_data(gd_ex, overwrite = TRUE, internal = TRUE)




