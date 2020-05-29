# this is a junk test to just create a placeholder. It doesn't really test anything:
test_that("test data can be created", {
  testthat::skip_on_cran()
  library(LakeEnsemblR)
  
  
  testthat::expect_error(run_ensemble(model = c("GRE")), 
                         'Unknown model: "GRE" in input argument "model"')
})

test_that("create model meteo & config files", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  
  # Set config file
  masterConfigFile <- "LakeEnsemblR.yaml"
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = masterConfigFile, model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                inflow_file = "LakeEnsemblR_inflow_standard.csv", folder = ".")
  
  # 2. Create meteo driver files
  export_meteo(masterConfigFile, model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
               meteo_file = "LakeEnsemblR_meteo_standard.csv")
  
  # 3. Create initial conditions
  export_init_cond(config_file = masterConfigFile,
                   model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                   print = TRUE)
  
  
  testthat::expect_true((file.exists("FLake/flake.nml") & file.exists("GLM/glm3.nml") &
                          file.exists("GOTM/gotm.yaml") & file.exists("Simstrat/simstrat.par") &
                          file.exists("MyLake/mylake.Rdata")))
})


test_that("can run models", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  
  # Set config file & models
  config_file <- 'LakeEnsemblR.yaml'
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run models & generate csv files", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  
  # Set config file & models
  config_file <- 'LakeEnsemblR.yaml'
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  
  # Change to text output
  input_yaml(config_file, label = "output", key = "format", value = "text")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  
  
  testthat::expect_true((length(list.files("output", pattern = "csv")) > 1))
})

test_that("can calibrate models", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  
  # Set config file
  masterConfigFile <- "LakeEnsemblR.yaml"
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = masterConfigFile, model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                inflow_file = "LakeEnsemblR_inflow_standard.csv", folder = ".")
  
  # 2. Create meteo driver files
  export_meteo(masterConfigFile, model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
               meteo_file = "LakeEnsemblR_meteo_standard.csv")
  
  # 3. Create initial conditions
  export_init_cond(config_file = masterConfigFile,
                   model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                   print = TRUE)
  
  # 4 calibrate models
  cali_ensemble(config_file = masterConfigFile, cmethod = "LHC", num = 5,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"))
  
  testthat::expect_true(length(list.files("cali")) == 10 )
})

test_that("check plots", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  library(ggplot2)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  
  # Set config file & models
  config_file <- 'LakeEnsemblR.yaml'
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  ncdf <- "output/ensemble_output.nc"
  
  # Change to netcdf output
  input_yaml(config_file, label = "output", key = "format", value = "netcdf")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model,
                inflow_file = "LakeEnsemblR_inflow_standard.csv")
  
  # 2. Create meteo driver files
  export_meteo(config_file = config_file, model = model,
               meteo_file = "LakeEnsemblR_meteo_standard.csv")
  
  # 3. Create initial conditions
  export_init_cond(config_file = config_file,
                   model = model,
                   print = TRUE)
  
  # 4 run models
  run_ensemble(config_file = config_file,
               model = model)
  
  plist <- plot_ensemble(ncdf = ncdf, model = model, var = "watertemp", depth = 0.9)
  
  
  testthat::expect_true(ggplot2::is.ggplot(plist[[1]]))
})
