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
  template_folder <- system.file("extdata/feeagh", package= 'LakeEnsemblR')
  dir.create('example') # Create example folder
  file.copy(from = template_folder, to = 'example', recursive = TRUE)
  setwd('example/feeagh') # Change working directory to example folder
  
  # Set config file
  masterConfigFile <- 'Feeagh_master_config.yaml'
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat', 'MyLake'),
                folder = '.')
  
  # 2. Create meteo driver files
  export_meteo(masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat', 'MyLake'),
               meteo_file = 'LakeEnsemblR_meteo_standard.csv')
  
  # 3. Create initial conditions
  start_date <- get_yaml_value(file = masterConfigFile, label =  "time", key = "start")
  
  export_init_cond(masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat', 'MyLake'),
                   wtemp_file = 'LakeEnsemblR_wtemp_profile_standard.csv',
                   date = start_date, 
                   month = 1, ndeps = 2, print = TRUE)
  
  
  testthat::expect_true((file.exists("FLake/feeagh.nml") & file.exists("GLM/glm3.nml") &
                          file.exists("GOTM/gotm.yaml") & file.exists("Simstrat/feeagh.par") &
                          file.exists("MyLake/mylake_config_template.Rdata")))
})

test_that("all models can be run", {

  # Set config file
  masterConfigFile <- 'Feeagh_master_config.yaml'
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat', 'MyLake'),
                folder = '.')
  
  # 2. Create meteo driver files
  export_meteo(masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat', 'MyLake'),
               meteo_file = 'LakeEnsemblR_meteo_standard.csv')
  
  # 3. Create initial conditions
  start_date <- get_yaml_value(file = masterConfigFile, label =  "time", key = "start")
  
  export_init_cond(masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat', 'MyLake'),
                   wtemp_file = 'LakeEnsemblR_wtemp_profile_standard.csv',
                   date = start_date, 
                   month = 1, ndeps = 2, print = TRUE)

# 4. Run ensemble lake models
wtemp_list <- run_ensemble(config_file = masterConfigFile,
                           model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
                           return_list = TRUE, create_netcdf = TRUE)

testthat::expect_true(file.exists("output/ensemble_output.nc"))
})
