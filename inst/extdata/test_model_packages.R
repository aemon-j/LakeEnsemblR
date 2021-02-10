# test_model_packages.R

###################################
# This script will check if all of your model packages are installed correctly and
# can be run on your operating system.
#
# Before running this script, make sure you have installed all packages
# according to the instructions on the LakeEnsemblR github main page.
# 
# 2020-10-23 LakeEnsemblR team
###################################

## set working directory to location the R script is stored - only in RStudio
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Set time zone to UTC
Sys.setenv(TZ = "UTC")

## copy example files from the package to current working directory
template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
dir.create("test")
file.copy(from = template_folder, to = "test", recursive = TRUE)

## change working diorectory to the feeagh folder
setwd("test/feeagh")

## Load LakeEnsemblR
library(LakeEnsemblR)

# Set config file & models
config_file <- "LakeEnsemblR.yaml"

## Test FLake ----
# 1. Example - creates directories with all model setup
export_config(config_file = config_file, model = "FLake")

setwd("FLake")

FLakeR::run_flake(verbose = TRUE)
out <- read.table("output/output.dat", skip = 1, header = TRUE)
plot(out$time, out$Ts, type = "l")

setwd("../")

## Test GLM ----
# 1. Example - creates directories with all model setup
export_config(config_file = config_file, model = "GLM")

setwd("GLM")

GLM3r::run_glm(verbose = TRUE)
out <- glmtools::get_var("output/output.nc", "temp", reference = "surface")
plot(out$DateTime, out[, 2], type = "l")

setwd("../")

## Test GOTM ----
# 1. Example - creates directories with all model setup
export_config(config_file = config_file, model = "GOTM")

setwd("GOTM")

GOTMr::run_gotm(verbose = TRUE)
out <- gotmtools::get_vari("output/output.nc", "temp")
plot(out$Datetime, out[, 2], type = "l")

setwd("../")

## Test Simstrat ----
# 1. Example - creates directories with all model setup
export_config(config_file = config_file, model = "Simstrat")

setwd("Simstrat")

SimstratR::run_simstrat(par_file = "simstrat.par", verbose = TRUE)
out <- read.table(file.path("output", "T_out.dat"), header = TRUE, 
                  sep = ",", check.names = FALSE)
plot(1:nrow(out), out[, ncol(out)], type = "l")

setwd("../")


## Test MyLake ----
# 1. Example - creates directories with all model setup
export_config(config_file = config_file, model = "MyLake")

# setwd("MyLake")
MyLakeR::run_mylake(sim_folder = ".", config_dat = "mylake.Rdata")
load("MyLake/output/output.RData")
plot(res$tt, res$Tzt[1, ], type = "l")

setwd("../")

# END