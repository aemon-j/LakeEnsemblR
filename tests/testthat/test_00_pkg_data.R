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
  
  # 1. Example - export configuration settings
  export_config(config_file = masterConfigFile,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".")
  
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


test_that("can add members to netCDF models", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  ncdf <- "output/ensemble_output.nc"
  
  # Set config file & models
  config_file <- 'LakeEnsemblR.yaml'
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  test1 <- tryCatch({
    load_var(ncdf, "watertemp", return = "array")
  }, error = function(e) return(FALSE))
  
  test2 <- tryCatch({
    load_var(ncdf, "watertemp", return = "list")
  }, error = function(e) return(FALSE))
  
  testthat::expect_true(is.array(test1))
  testthat::expect_true(is.list(test2))
  
  test3 <- tryCatch({
    run_ensemble(config_file = config_file,
                        model = model, add = TRUE)
    }, error = function(e) return(FALSE))
  
  testthat::expect_null(test3)
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
  export_config(config_file = masterConfigFile,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".")
  
  # 2. Calibrate models
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
  
  # 2. Run models
  run_ensemble(config_file = config_file,
               model = model)
  
  pl1 <- plot_ensemble(ncdf = ncdf, model = model, var = "watertemp", depth = 0.9)
  pl2 <- plot_resid(ncdf = ncdf, model = model, var = "watertemp")
  pl3 <- plot_heatmap(ncdf = ncdf, model = model)
  
  
  testthat::expect_true(ggplot2::is.ggplot(pl1[[1]]))
  testthat::expect_true(ggplot2::is.ggplot(pl2[[1]]))
  testthat::expect_true(ggplot2::is.ggplot(pl3))
})


test_that("can load from netCDF", {
  
  library(LakeEnsemblR)
  library(gotmtools)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  dir.create("example") # Create example folder
  file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd("example/feeagh") # Change working directory to example folder
  ncdf <- "output/ensemble_output.nc"
  
  # Set config file & models
  config_file <- 'LakeEnsemblR.yaml'
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  
})

