#'Export extinction coefficients
#'
#'Exports extinction coefficients for each model based on a master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file. Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'
#'@importFrom stats approx
#'@import lubridate
#'@importFrom gotmtools get_yaml_value input_yaml input_nml
#'@importFrom glmtools read_nml set_nml write_nml
#'
#'@export

export_extinction <- function(config_file, model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.'){
  
  # Set working directory
  oldwd <- getwd()
  setwd(folder)
  
  # Fix time zone
  original_tz = Sys.getenv("TZ")
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
    Sys.setenv(TZ=original_tz)
  })
  
  Sys.setenv(TZ="GMT")
  
  # Check if the value in the config file is a fixed value, or a file (time series)
  Kw <- get_yaml_value(config_file, "model_settings", "extinction_coefficient")
  if(is.numeric(Kw)){
    constantValue = T
  }else{
    constantValue = F
    Kw_file <- read.csv(file.path(folder,Kw))
    Kw_file$datetime <- as.POSIXct(Kw_file$datetime)
    
    startTimeSeries <- as.POSIXct(get_yaml_value(config_file, "time", "start"))
    endTimeSeries <- as.POSIXct(get_yaml_value(config_file, "time", "stop"))
    
    # Calculate time-averaged extinction coefficient, for models that can only
    # use a constant value
    Kw <- time_average(Kw_file,
                      start=startTimeSeries,
                      end=endTimeSeries,
                      n=1000)
    
  }
  
  if("FLake" %in% model){
    
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('FLake')){
      dir.create('FLake')
    }
    if(!dir.exists('FLake/output')){
      dir.create('FLake/output')
    }
    
    if(!constantValue){
      message("FLake does not accept varying extinction coefficient over time. Average is used instead.")
    }
    
    # Read the FLake config file from config_file, and write it to the FLake directory
    temp_fil <- get_yaml_value(config_file, "config_files", "flake_config")
    if(file.exists(temp_fil)){
      fla_fil <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file('extdata/flake_template.nml', package = packageName())
      file.copy(from = template_file, to = file.path(folder, 'FLake', basename(temp_fil)))
      fla_fil <- file.path(folder, 'FLake', basename(temp_fil))
    }
    
    input_nml(fla_fil, label = 'TRANSPARENCY', key = 'extincoef_optic', Kw)
  }
  
  if("GLM" %in% model){
    
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('GLM/output')){
      dir.create('GLM/output', recursive = TRUE)
    }
    
    # Read the GLM config file from config_file, and write it to the GLM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "glm_config")
    
    if(file.exists(temp_fil)){
      glm_nml <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/glm3_template.nml", package = packageName()) #
      file.copy(from = template_file, to = file.path(folder, 'GLM', basename(temp_fil)))
      glm_nml <- file.path(folder, 'GLM', basename(temp_fil))
    }
    
    if(constantValue){
      input_nml(glm_nml, "light", "Kw", Kw)
    }else{
      # Make GLM-version of Kw_file and write to GLM nml
      message("Warning: variable water level in GLM not yet implemented")
    }
  }
  
  if("GOTM" %in% model){
    
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('GOTM/output')){
      dir.create('GOTM/output', recursive = TRUE)
    }
    
    # Read the GOTM config file from config_file, and write it to the GOTM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "gotm_config")
    if(file.exists(temp_fil)){
      got_yaml <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/gotm_template.yaml", package = packageName())
      file.copy(from = template_file, to = file.path(folder, 'GOTM', basename(temp_fil)))
      got_yaml <- file.path(folder, 'GOTM', basename(temp_fil))
    }
    
    if(constantValue){
      gotmtools::input_yaml(got_yaml, 'g2', 'method',0)
      gotmtools::input_yaml(got_yaml, 'g2', 'constant_value',1/Kw)
    }else{
      # Make GOTM-version of Kw_file and write to GOTM yaml (method = 2, etc.)
      # Then I would need to set method = 2 
      message("Warning: variable water level in GOTM not yet implemented")
    }
    
  }
  
  if("Simstrat" %in% model){
    
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('Simstrat/output')){
      dir.create('Simstrat/output', recursive = TRUE)
    }
    
    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    temp_fil <- get_yaml_value(config_file, "config_files", "simstrat_config")
    if(file.exists(temp_fil)){
      sim_par <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/simstrat_template.par", package = packageName())
      file.copy(from = template_file, to = file.path(folder, 'Simstrat', basename(temp_fil)))
      sim_par <- file.path(folder, 'Simstrat', basename(temp_fil))
    }
    
    light_fil <- system.file('extdata/absorption_langtjern.dat', package= 'SimstratR')
    file.copy(from = light_fil, to = file.path(folder, 'Simstrat','light_absorption.dat'))
    
    # Write absorption file
    absorption_line_1 <- "Time [d] (1.col)    z [m] (1.row)    Absorption [m-1] (rest)"
    absorption_line_2 <- "1"
    absorption_line_3 <- "-1 -1.00"
    
    if(constantValue){
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      absorption_line_4 <- paste(start_sim,Kw)
      absorption_line_5 <- paste(end_sim,Kw)
      
      fileConnection <- file("Simstrat/light_absorption.dat")
      writeLines(c(absorption_line_1,absorption_line_2,absorption_line_3,absorption_line_4,absorption_line_5), fileConnection)
      close(fileConnection)
    }else{
      # Make Simstrat-version of Kw_file and write to Simstrat dat file
      message("Warning: variable water level in Simstrat not yet implemented")
    }
    
    
    
  }
}
