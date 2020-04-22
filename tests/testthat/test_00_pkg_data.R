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
  masterConfigFile <- "Feeagh_master_config.yaml"
  
  # 1. Example - creates directories with all model setup
  export_config(config_file = masterConfigFile, model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                inflow_file = "LakeEnsemblR_inflow_standard.csv", folder = ".")
  
  # 2. Create meteo driver files
  export_meteo(masterConfigFile, model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
               meteo_file = "LakeEnsemblR_meteo_standard.csv")
  
  # 3. Create initial conditions
  export_init_cond(masterConfigFile,
                   model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
                   print = TRUE)
  
  
  testthat::expect_true((file.exists("FLake/feeagh.nml") & file.exists("GLM/glm3.nml") &
                          file.exists("GOTM/gotm.yaml") & file.exists("Simstrat/feeagh.par") &
                          file.exists("MyLake/mylake_config_template.Rdata")))
})
