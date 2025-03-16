# this is a junk test to just create a placeholder. It doesn't really test anything:
test_that("test data can be created", {
  testthat::skip_on_cran()
  testthat::expect_error(run_ensemble(model = c("GRE")),
                         'Unknown model: "GRE" in input argument "model"')
})

test_that("create model meteo & config files", {
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  setwd(template_folder) # Change working directory to example folder

  # Set config file
  masterConfigFile <- "LakeEnsemblR.yaml"
  config_file <- "LakeEnsemblR_copy.yaml"
  file.copy(masterConfigFile, config_file, overwrite = TRUE)

  # 1. Example - export configuration settings
  export_config(config_file = config_file,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".")

  testthat::expect_true((file.exists("FLake/flake.nml") & file.exists("GLM/glm3.nml") &
                          file.exists("GOTM/gotm.yaml") & file.exists("Simstrat/simstrat.par") &
                          file.exists("MyLake/mylake.Rdata")))
})


test_that("can run FLake", {
  
  # Skip test on MacOS
  testthat::skip_on_os("mac")

  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run GLM", {
  
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("GLM")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run GOTM", {
  
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("GOTM")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run Simstrat", {
  
  # Skip test on MacOS
  testthat::skip_on_os("mac")
  
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("Simstrat")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run MyLake", {
  
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("MyLake")
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run all models in parallel", {
  
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")  
  
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model, parallel = TRUE, ncores = 2)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})


test_that("can add members to netCDF models", {

  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  ncdf <- "output/ensemble_output.nc"

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  test1 <- tryCatch({
    load_var(ncdf, "temp", return = "array")
  }, error = function(e) return(FALSE))

  test2 <- tryCatch({
    load_var(ncdf, "temp", return = "list")
  }, error = function(e) return(FALSE))
  test3 <- tryCatch({
    load_var(ncdf, "ice_height", return = "array")
  }, error = function(e) return(FALSE))

  test4 <- tryCatch({
    load_var(ncdf, "ice_height", return = "list")
  }, error = function(e) return(FALSE))

  met <- read.csv('LakeEnsemblR_meteo_standard.csv')
  met$Air_Temperature_celsius <- met$Air_Temperature_celsius + 2
  met$Ten_Meter_Elevation_Wind_Speed_meterPerSecond <- met$Ten_Meter_Elevation_Wind_Speed_meterPerSecond*0.75
  write.csv(met, "LakeEnsemblR_meteo_standard.csv", row.names = F, quote = F)
  export_config(config_file, model = model)

  test_add <- tryCatch({
    run_ensemble(config_file = config_file,
                 model = model, add = TRUE)
    TRUE
  }, error = function(e) return(FALSE))
  testthat::expect_true(test_add)



  test5 <- tryCatch({
    load_var(ncdf, "temp", return = "array", dim = "member")
  }, error = function(e) return(FALSE))

  test6 <- tryCatch({
    load_var(ncdf, "temp", return = "list", dim = "member")
  }, error = function(e) return(FALSE))
  test7 <- tryCatch({
    load_var(ncdf, "ice_height", return = "array", dim = "member")
  }, error = function(e) return(FALSE))

  test8 <- tryCatch({
    load_var(ncdf, "ice_height", return = "list", dim = "member")
  }, error = function(e) return(FALSE))

  testthat::expect_true(is.array(test1))
  testthat::expect_true(is.list(test2))
  testthat::expect_true(is.array(test3))
  testthat::expect_true(is.list(test4))
  testthat::expect_true(is.array(test5))
  testthat::expect_true(is.list(test6))
  testthat::expect_true(is.array(test7))
  testthat::expect_true(is.list(test8))


  test9 <- tryCatch({
    analyse_ncdf(ncdf, model)
  }, error = function(e) return(FALSE))

  test10 <- tryCatch({
    analyse_ncdf(ncdf, model, dim = "member")
  }, error = function(e) return(FALSE))

  #testthat::expect_true(is.list(test9))
  testthat::expect_true(is.list(test10))

  test11 <- tryCatch({
    plot_resid(ncdf, var = "temp")
  }, error = function(e) return(FALSE))

  test12 <- tryCatch({
    plot_resid(ncdf, var = "temp", dim = "member")
  }, error = function(e) return(FALSE))


  testthat::expect_true(is.list(test11))
  testthat::expect_true(is.list(test12))

  test13 <- tryCatch({
    plot_ensemble(ncdf, model, var = "temp", depth = 0.9)
  }, error = function(e) return(FALSE))
  testthat::expect_true(ggplot2::is.ggplot(test13))

  test14 <- tryCatch({
    plot_ensemble(ncdf, model, var = "temp", depth = 0.9, dim = "member")
  }, error = function(e) return(FALSE))
  testthat::expect_true(ggplot2::is.ggplot(test14))

  test15 <- tryCatch({
    plot_ensemble(ncdf, model, var = "temp", depth = 0.9, dim = "member",
                  residuals = TRUE)
  }, error = function(e) return(FALSE))
  testthat::expect_true(ggplot2::is.ggplot((test15[[1]])) &
                          ggplot2::is.ggplot((test15[[2]])))

  test16 <- tryCatch({
    plot_ensemble(ncdf, model, var = "temp", depth = 0.9, dim = "member",
                  residuals = TRUE, boxwhisker = TRUE)
  }, error = function(e) return(FALSE))
  testthat::expect_true(ggplot2::is.ggplot((test16[[1]])) &
                          ggplot2::is.ggplot((test16[[2]])) &
                          ggplot2::is.ggplot((test16[[3]])))

})

test_that("can run models & generate csv files", {

  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }

  # Change to text output
  input_yaml(config_file, label = "output", key = "format", value = "text")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true(length(list.files("output", pattern = "csv")) == 16L)
})

test_that("can calibrate models", {

  # 1. Example - creates directories with all model setup
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  
  export_config(config_file = config_file,
                model = model,
                folder = ".")

  # 2. Calibrate models
  cali_ensemble(config_file = config_file, cmethod = "LHC", num = 5,
                model = model)

  testthat::expect_true(length(list.files("cali")) == 10)
})

test_that("can calibrate models in parallel using old LHC", {
  
  # 1. Example - creates directories with all model setup
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  export_config(config_file = config_file,
                model = model,
                folder = ".")
  
  # 2. Calibrate models
  cal_out <- cali_ensemble(config_file = config_file, cmethod = "LHC_old", 
                           num = 10, model = model, parallel = TRUE, ncores = 2)
  chk <- all(!is.na(unlist(cal_out)))
  
  testthat::expect_true(chk)
})

test_that("can calibrate models in parallel using new LHC", {
  
  # 1. Example - creates directories with all model setup
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  export_config(config_file = config_file,
                model = model,
                folder = ".")
  
  # 2. Calibrate models
  cal_out <- cali_ensemble(config_file = config_file, cmethod = "LHC",
                           out_f =  "LHC",
                           num = 10, model = model, parallel = TRUE, ncores = 2)
  chk <- all(!is.na(unlist(cal_out)))
  
  testthat::expect_true(chk)
})

test_that("can generate all output vars", {
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  input_yaml(config_file, label = "output", key = "format", value = "netcdf")
  input_yaml_multiple(config_file, key1 = "output", key2 = "variables",
                      value = c("temp", "salt", "dens", "ice_height", "w_level", "q_sens", "q_lat"))
  
  export_config(config_file = config_file, model = model)
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("check plots", {

  # Change to netcdf output
  file.remove("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
  }
  ncdf <- "output/ensemble_output.nc"
  input_yaml(config_file, label = "output", key = "format", value = "netcdf")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. Run models
  run_ensemble(config_file = config_file,
               model = model)

  pl1 <- plot_ensemble(ncdf = ncdf, model = model, var = "temp", depth = 0.9)
  pl2 <- plot_resid(ncdf = ncdf, model = model, var = "temp")
  pl3 <- plot_heatmap(ncdf = ncdf, model = model)


  testthat::expect_true(ggplot2::is.ggplot(pl1))
  testthat::expect_true(ggplot2::is.ggplot(pl2[[1]]))
  testthat::expect_true(ggplot2::is.ggplot(pl3))
  
  unlink("output", recursive = TRUE)
  file.remove(config_file)
})


test_that("export and run_ensemble old yaml file (version 1.0) still works", {
  
  # Set config file
  masterConfigFile <- "LakeEnsemblR_v1.yaml"
  config_file <- "LakeEnsemblR_v1_copy.yaml"
  file.copy(masterConfigFile, config_file, overwrite = TRUE)
  
  unlink("FLake", recursive = TRUE)
  unlink("GLM", recursive = TRUE)
  unlink("GOTM", recursive = TRUE)
  unlink("Simstrat", recursive = TRUE)
  unlink("MyLake", recursive = TRUE)
  
  # 1. Example - export configuration settings
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  model_cfg_files <- c("FLake" = "FLake/flake.nml", "GLM" = "GLM/glm3.nml",
                       "GOTM" = "GOTM/gotm.yaml", "Simstrat" = "Simstrat/simstrat.par",
                       "MyLake" = "MyLake/mylake.Rdata")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
    model_cfg_files <- c("GLM" = "GLM/glm3.nml",
                         "GOTM" = "GOTM/gotm.yaml",
                         "MyLake" = "MyLake/mylake.Rdata")
  }
  export_config(config_file = config_file,
                model = model,
                folder = ".")
  
  # 2. Run models
  run_ensemble(config_file = config_file,
               model = model)
  
  testthat::expect_true(all(file.exists(model_cfg_files) &
                              file.exists("output/ensemble_output.nc")))
  
  unlink("output", recursive = TRUE)
  file.remove(config_file)
})

test_that("calibration with old yaml file (version 1.0) still works", {
  
  # Set config file
  masterConfigFile <- "LakeEnsemblR_v1.yaml"
  config_file <- "LakeEnsemblR_v1_copy.yaml"
  file.copy(masterConfigFile, config_file, overwrite = TRUE)
  
  unlink("FLake", recursive = TRUE)
  unlink("GLM", recursive = TRUE)
  unlink("GOTM", recursive = TRUE)
  unlink("Simstrat", recursive = TRUE)
  unlink("MyLake", recursive = TRUE)
  unlink("cali", recursive = TRUE)
  
  # 1. Example - export configuration settings
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  model_cfg_files <- c("FLake" = "FLake/flake.nml", "GLM" = "GLM/glm3.nml",
                       "GOTM" = "GOTM/gotm.yaml", "Simstrat" = "Simstrat/simstrat.par",
                       "MyLake" = "MyLake/mylake.Rdata")
  os <- tolower(Sys.info()[["sysname"]])
  if (os == "mac") {
    model <- c("GLM", "GOTM", "MyLake")  
    model_cfg_files <- c("GLM" = "GLM/glm3.nml",
                         "GOTM" = "GOTM/gotm.yaml",
                         "MyLake" = "MyLake/mylake.Rdata")
  }
  export_config(config_file = config_file,
                model = model,
                folder = ".")
  # 2. Calibrate models
  cali_ensemble(config_file = config_file, cmethod = "LHC", num = 5,
                model = model)
  
  testthat::expect_true(length(list.files("cali")) == (length(model) * 2))
  
  unlink("output", recursive = TRUE)
  file.remove(config_file)
})

# end
