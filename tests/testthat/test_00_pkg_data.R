# this is a junk test to just create a placeholder. It doesn't really test anything:
test_that("test data can be created", {
  testthat::skip_on_cran()
  library(LakeEnsemblR)

  testthat::expect_error(run_ensemble(model = c("GRE")),
                         'Unknown model: "GRE" in input argument "model"')
})

test_that("create model meteo & config files", {

  # library(gotmtools)
  library(LakeEnsemblR)
  template_folder <- system.file("extdata/feeagh", package= "LakeEnsemblR")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)
  # file.copy(from = template_folder, to = "example", recursive = TRUE)
  setwd(file.path(temp_dir, "feeagh")) # Change working directory to example folder
  # setwd(file.path("example", "feeagh")) # Change working directory to example folder

  # Set config file
  masterConfigFile <- "LakeEnsemblR.yaml"
  config_file <- "LakeEnsemblR_copy.yaml"
  file.copy(masterConfigFile, config_file, overwrite = TRUE)

  # 1. Example - export configuration settings
  export_config(config_file = config_file,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".", dirs = T, time = T, location = T, output_settings = T,
                meteo = F, init_cond = F, extinction = T, inflow = F, model_parameters = F)

  testthat::expect_true((file.exists("FLake/flake.nml") & file.exists("GLM/glm3.nml") &
                          file.exists("GOTM/gotm.yaml") & file.exists("Simstrat/simstrat.par") &
                          file.exists("MyLake/mylake.Rdata")))

  # 1. met file
  export_config(config_file = config_file,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".", dirs = F, time = F, location = F, output_settings = F,
                meteo = T, init_cond = F, extinction = F, inflow = F, model_parameters = F)

  testthat::expect_true((file.exists("FLake/all_meteo_file.dat") & file.exists("GLM/meteo_file.csv") &
                           file.exists("GOTM/meteo_file.dat") & file.exists("Simstrat/meteo_file.dat") &
                           file.exists("MyLake/meteo_file.dat")))

  # 1. Inflow file
  export_config(config_file = config_file,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".", dirs = F, time = F, location = F, output_settings = F,
                meteo = F, init_cond = F, extinction = F, inflow = T, model_parameters = F)

  testthat::expect_true((file.exists("FLake/Tinflow") & file.exists("GLM/inflow_file.csv") &
                           file.exists("GOTM/inflow_file.dat") & file.exists("Simstrat/Qin.dat")))

  # Model parameters
  export_config(config_file = config_file,
                  model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                  folder = ".", dirs = F, time = F, location = F, output_settings = F,
                  meteo = F, init_cond = F, extinction = F, inflow = F, model_parameters = T)

  # Initial conditions
  export_config(config_file = config_file,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".", dirs = F, time = F, location = F, output_settings = F,
                meteo = F, init_cond = T, extinction = F, inflow = F, model_parameters = F)




})

test_that("can add members to netCDF models", {

  unlink("output/ensemble_output.nc", recursive = TRUE, force = TRUE)
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
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
  }, error = function(e) return(FALSE))
  testthat::expect_true(is.list(test_add))



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

  # testthat::expect_true(is.list(test9))
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

test_that("can run FLake", {

  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model, verbose = T)

  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("FLake", "output", "output.dat"))))
})

test_that("can run FLake with errors", {

  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  nml <- glmtools::read_nml(nml_file = file.path("FLake", "flake.nml"))
  nml <- glmtools::set_nml(nml, "meteofile", "NA.dat")
  glmtools::write_nml(nml, file.path("FLake", "flake.nml"))

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model, verbose = T)

  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run GLM", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("GLM")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("GLM", "output", "output.nc"))))
})

test_that("can run GLM with errors", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("GLM")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  nml <- glmtools::read_nml(nml_file = file.path("GLM", "glm3.nml"))
  nml <- glmtools::set_nml(nml, "meteo_fl", "NA.dat")
  glmtools::write_nml(nml, file.path("GLM", "glm3.nml"))

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run GOTM", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR.yaml"
  model <- c("GOTM")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("GOTM", "output", "output.nc"))))
})

test_that("can run GOTM with errors", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR.yaml"
  model <- c("GOTM")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  yaml <- gotmtools::read_yaml(file.path("GOTM", "gotm.yaml"))
  yaml <- gotmtools::set_yaml(yaml, value = "NA.dat", "location", "hypsograph")
  gotmtools::write_yaml(yaml, file.path("GOTM", "gotm.yaml"))

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run Simstrat", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("Simstrat")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("Simstrat", "output", "T_out.dat"))))
})

test_that("can run Simstrat with errors", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("Simstrat")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)
  val <- "\"NA.dat\""
  input_json(file = file.path("Simstrat", "simstrat.par"), label = "Input",
                           key = "Forcing", value = val)
  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc")))
})

test_that("can run MyLake", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("MyLake")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("MyLake", "output", "output.RData"))))
})

test_that("can run MyLake with errors", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("MyLake")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  load(file.path("MyLake", "mylake.Rdata"))
  mylake_config$Phys.par <- NULL
  save("mylake_config", file = file.path("MyLake", "mylake.Rdata"))

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)

  testthat::expect_true((file.exists("output/ensemble_output.nc")))
  unlink(file.path("MyLake", "mylake.Rdata"))
})

test_that("can run models & generate csv files", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")

  # Change to text output
  yaml <- read_yaml(config_file)
  yaml <- set_yaml(yaml, label = "output", key = "format", value = "text")
  write_yaml(yaml, config_file)
  # input_yaml(config_file, label = "output", key = "format", value = "text")

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)



  testthat::expect_true((length(list.files("output", pattern = "csv")) == 11))
})

test_that("can calibrate models", {

  # 1. Example - creates directories with all model setup
  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR.yaml"
  export_config(config_file = config_file,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                folder = ".")

  # 2. Calibrate models
  cali_ensemble(config_file = config_file, cmethod = "LHC", num = 5,
                model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"))

  testthat::expect_true(length(list.files("cali")) == 10 )
})

test_that("check plots", {

  # Change to netcdf output
  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("FLake", "GLM", "GOTM", "Simstrat", "MyLake")
  ncdf <- "output/ensemble_output.nc"
  yaml <- read_yaml(config_file)
  yaml <- set_yaml(yaml, label = "output", key = "format", value = "netcdf")
  write_yaml(yaml, config_file)

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

})

test_that("can restart GLM", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("GLM")

  yaml <- gotmtools::read_yaml(config_file)
  yaml$restart$use <- FALSE
  yaml$time$start <- "2010-06-01 00:00:00"
  yaml$time$stop <- "2010-06-03 00:00:00"
  yaml$output$time_step <- 1
  gotmtools::write_yaml(yaml, config_file)

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  restart_list <- read_restart(model = model)
  # plot_heatmap(ncdf)+
  #   scale_colour_gradientn(limits = c(9, 17),
  #                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))


  unlink("output/ensemble_output.nc")

  yaml <- gotmtools::read_yaml(config_file)
  # yaml$restart$use <- TRUE
  yaml$time$start <- "2010-06-03 00:00:00"
  yaml$time$stop <- "2010-06-05 00:00:00"
  gotmtools::write_yaml(yaml, config_file)

  export_config(config_file = config_file, model = model, dirs = FALSE, time = TRUE,
                location = FALSE, output_settings = FALSE, meteo = TRUE, init_cond = FALSE,
                extinction = FALSE, inflow = TRUE, model_parameters = TRUE)

  write_restart(model = model, restart_list = restart_list)

  run_ensemble(config_file = config_file,
               model = model)
  # plot_heatmap(ncdf)+
  #   scale_colour_gradientn(limits = c(9, 17),
  #                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))


  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("GLM", "output", "output.nc"))))
})

test_that("can restart GOTM", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("GOTM")

  yaml <- gotmtools::read_yaml(config_file)
  yaml$restart$use <- FALSE
  yaml$time$start <- "2010-06-01 00:00:00"
  yaml$time$stop <- "2010-06-03 00:00:00"
  yaml$output$time_step <- 1
  gotmtools::write_yaml(yaml, config_file)

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)

  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  # ncdf <- "output/ensemble_output.nc"
  # plot_heatmap(ncdf)+
  #   scale_colour_gradientn(limits = c(9, 17),
  #                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))


  restart_list <- read_restart(model = model)

  unlink("output/ensemble_output.nc")

  yaml <- gotmtools::read_yaml(config_file)
  # yaml$restart$use <- TRUE
  yaml$time$start <- "2010-06-03 00:00:00"
  yaml$time$stop <- "2010-06-05 00:00:00"
  gotmtools::write_yaml(yaml, config_file)

  export_config(config_file = config_file, model = model, dirs = FALSE, time = TRUE,
                location = FALSE, output_settings = FALSE, meteo = TRUE, init_cond = FALSE,
                extinction = FALSE, inflow = TRUE, model_parameters = TRUE)
  write_restart(model = model, restart_list = restart_list)


  run_ensemble(config_file = config_file,
               model = model)
  # plot_heatmap(ncdf)+
  #   scale_colour_gradientn(limits = c(9, 17),
  #                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))


  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("GOTM", "output", "output.nc"))))
})

test_that("can restart Simstrat", {

  unlink("output/ensemble_output.nc")
  config_file <- "LakeEnsemblR_copy.yaml"
  model <- c("Simstrat")

  yaml <- gotmtools::read_yaml(config_file)
  yaml$restart$use <- FALSE
  yaml$time$start <- "2010-06-01 00:00:00"
  yaml$time$stop <- "2010-06-03 00:00:00"
  yaml$output$time_step <- 1
  yaml$model_parameters$Simstrat$`ModelConfig/InitializeSeicheEnergy` <- FALSE
  gotmtools::write_yaml(yaml, config_file)

  # 1. Example - creates directories with all model setup
  export_config(config_file = config_file, model = model)


  # 2. run models
  run_ensemble(config_file = config_file,
               model = model)
  # ncdf <- "output/ensemble_output.nc"
  # plot_heatmap(ncdf)+
  #   scale_colour_gradientn(limits = c(9, 17),
  #                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))


  restart_list <- read_restart(model = model)
  unlink("output/ensemble_output.nc")

  yaml <- gotmtools::read_yaml(config_file)
  yaml$time$start <- "2010-06-03 00:00:00"
  yaml$time$stop <- "2010-06-05 00:00:00"
  gotmtools::write_yaml(yaml, config_file)

  export_config(config_file = config_file, model = model, dirs = FALSE, time = TRUE,
                location = FALSE, output_settings = FALSE, meteo = TRUE, init_cond = FALSE,
                extinction = FALSE, inflow = TRUE, model_parameters = TRUE)
  write_restart(model = model, restart_list = restart_list)

  run_ensemble(config_file = config_file,
               model = model)
  # plot_heatmap(ncdf)+
  #   scale_colour_gradientn(limits = c(9, 17),
  #                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))
  #
  testthat::expect_true((file.exists("output/ensemble_output.nc") &
                           file.exists(file.path("Simstrat", "output", "T_out.dat"))))
})

# end
