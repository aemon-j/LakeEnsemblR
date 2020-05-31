#'Export time settings for each model
#'
#'Exports settings like start and end time and time step,
#'  based on the master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'@importFrom gotmtools get_yaml_value input_yaml
#'@importFrom glmtools get_nml_value read_nml set_nml write_nml
#'@importFrom lubridate year floor_date ceiling_date
#'
#'@export

export_time <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                        folder = "."){
  # Set working directory
  oldwd <- getwd()
  setwd(folder)
  
  # Fix time zone
  original_tz <- Sys.getenv("TZ")
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
    Sys.setenv(TZ = original_tz)
  })
  
  Sys.setenv(TZ = "GMT")
  
  # check model input
  model <- check_models(model)
  
##-------------Read settings---------------
  # Start date
  start_date <- get_yaml_value(config_file, "time", "start")
  # Stop date
  stop_date <- get_yaml_value(config_file, "time", "stop")
  # Time step
  timestep <- get_yaml_value(config_file, "time", "time_step")
  # Met time step (uses get_meteo_time_step function, in helpers.R)
  met_timestep <- get_meteo_time_step(file.path(folder,
                                                get_yaml_value(config_file, "meteo", "file")))
  
##---------------FLake-------------
  if("FLake" %in% model){
    fla_fil <- file.path(folder, get_yaml_value(config_file, "config_files", "FLake"))
    
    # check if meteo file is in accordence with start and stop date
    metf_flake <- gsub(",", "", glmtools::get_nml_value(nml_file = fla_fil, arg_name = "meteofile"))
    if(file.exists(metf_flake)) {
      met_flake <- read.table(file.path("FLake", metf_flake))
      met_flake$V6 <- as.POSIXct(met_flake$V6)
      # check if start date fits
      if(min(met_flake$V6) != as.POSIXct(start_date)) {
        warning(paste0("FLake start date (", as.character(min(met_flake$V6)),
                       ') does not fit start date specified in master control file "',
                       config_file, '"( ', as.character(start_date),
                       "). Please re-run export_meteo."))
      }
      # check if stop date fits
      if(max(met_flake$V6) != as.POSIXct(stop_date)) {
        warning(paste0("FLake stop date (", as.character(max(met_flake$V6)),
                       ') does not fit stop date specified in master control file "',
                       config_file, '"( ', as.character(stop_date),
                       "). Please re-run export_meteo."))
      }
      # set timesteps
      input_nml(fla_fil, "SIMULATION_PARAMS", "time_step_number", nrow(met_flake))
      
      # Input parameters
      input_nml(fla_fil, label = "SIMULATION_PARAMS", key = "del_time_lk", met_timestep)
      #meteo needs to be in same time step as model
    }
  }
  
##---------------GLM-------------
  
  if("GLM" %in% model){
    glm_nml <- file.path(folder, get_yaml_value(config_file, "config_files", "GLM"))
    
    # Read in nml and input parameters
    nml <- read_nml(glm_nml)
    
    inp_list <- list("start" = start_date,
                     "stop" = stop_date,
                     "dt" = timestep,
                     "timefmt" = 2,
                     "timezone" = 0)
    
    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    write_nml(nml, glm_nml)
  }
  
##---------------GOTM-------------
  if("GOTM" %in% model){
    got_yaml <- file.path(folder, get_yaml_value(config_file, "config_files", "GOTM"))
    
    # Set time settings
    input_yaml(got_yaml, "time", "start", start_date)
    input_yaml(got_yaml, "time", "stop", stop_date)
    input_yaml(got_yaml, "time", "dt", timestep)
  }
  
##---------------Simstrat-------------
  if("Simstrat" %in% model){
    sim_par <- file.path(folder, get_yaml_value(config_file, "config_files", "Simstrat"))
    
    # Set times
    reference_year <- lubridate::year(as.POSIXct(start_date))
    input_json(sim_par, "Simulation", "Start year", reference_year)
    start_date_simulation <- lubridate::floor_date(as.POSIXct(start_date), unit = "days")
    end_date_simulation <- lubridate::ceiling_date(as.POSIXct(stop_date), unit = "days")
    input_json(sim_par, "Simulation", "Start d",
               round(as.numeric(difftime(start_date_simulation,
                                         as.POSIXct(paste0(reference_year, "-01-01")),
                                         units = "days"))))
    input_json(sim_par, "Simulation", "End d",
               round(as.numeric(difftime(end_date_simulation,
                                         as.POSIXct(paste0(reference_year, "-01-01")),
                                         units = "days"))))
    input_json(sim_par, "Simulation", "Timestep s", timestep)
  }
  
##---------------MyLake-------------
  if("MyLake" %in% model){
    # Load config file MyLake
    load(get_yaml_value(config_file, "config_files", "MyLake"))
    
    # update MyLakeR config file
    mylake_config[["M_start"]] <- start_date
    mylake_config[["M_stop"]] <- stop_date
    
    # save lake-specific config file for MyLake
    temp_fil <- gsub(".*/", "", get_yaml_value(config_file, "config_files", "MyLake"))
    save(mylake_config, file = file.path(folder, "MyLake", temp_fil))
  }
  
  message("export_time complete!")
}