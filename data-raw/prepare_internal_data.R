## code to prepare `DATASET` dataset goes here

met_var_dic <- read.csv("data-raw/met_var_dic.csv")
lake_var_dic <- read.csv("data-raw/lake_var_dic.csv")
met_input <- read.csv("data-raw/met_input.csv")
pars_dic <- read.csv("data-raw/pars_dic.csv")
usethis::use_data(met_var_dic, overwrite = TRUE)
usethis::use_data(lake_var_dic, overwrite = TRUE)
usethis::use_data(met_input, overwrite = TRUE)
usethis::use_data(pars_dic, overwrite = TRUE)
