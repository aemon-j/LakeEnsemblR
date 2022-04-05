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
#'@importFrom glmtools get_nml_value read_nml set_nml write_nml
#'@importFrom lubridate year floor_date ceiling_date
#' @importFrom gotmtools read_yaml set_yaml write_yaml get_yaml_value
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

  if(!file.exists(config_file)) {
    stop(config_file, " does not exist. Make sure your file path is correct")
  } else {
    yaml <- gotmtools::read_yaml(config_file)
  }

  Sys.setenv(TZ = "GMT")

  # check model input
  model <- check_models(model)

##-------------Read settings---------------
  # Start date
  start_date <- get_yaml_value(yaml, "time", "start")
  # Stop date
  stop_date <- get_yaml_value(yaml, "time", "stop")
  # Time step
  timestep <- get_yaml_value(yaml, "time", "time_step")
  # Met time step (uses get_meteo_time_step function, in helpers.R)
  met_timestep <- get_meteo_time_step(file.path(folder,
                                                get_yaml_value(yaml, "input", "meteo", "file")))

##---------------FLake-------------
  if("FLake" %in% model){
    fla_fil <- file.path(folder, get_yaml_value(yaml, "config_files", "FLake"))

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
      nml <- glmtools::read_nml(fla_fil)

      nml <- glmtools::set_nml(nml, "SIMULATION_PARAMS::time_step_number", nrow(met_flake))
      nml <- glmtools::set_nml(nml, "SIMULATION_PARAMS::del_time_lk", met_timestep)

      glmtools::write_nml(nml, fla_fil)
    }
  }

##---------------GLM-------------

  if("GLM" %in% model){
    glm_nml <- file.path(folder, get_yaml_value(yaml, "config_files", "GLM"))

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
    got_file <- file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "GOTM"))
    got_yaml <- gotmtools::read_yaml(got_file)

    # Set time settings
    got_yaml <- gotmtools::set_yaml(got_yaml, "time", "start", value = start_date)
    got_yaml <- gotmtools::set_yaml(got_yaml, "time", "stop", value = stop_date)
    got_yaml <- gotmtools::set_yaml(got_yaml, "time", "dt", value = as.integer(timestep))

    write_yaml(got_yaml, got_file)
  }

##---------------Simstrat-------------
  if("Simstrat" %in% model){
    sim_par <- file.path(folder, get_yaml_value(yaml, "config_files", "Simstrat"))

    # Set times
    reference_year <- lubridate::year(as.POSIXct(start_date))
    input_json(sim_par, "Simulation", "Reference year", reference_year)
    if(!yaml$restart$use) {
      start_date_simulation <- as.POSIXct(start_date)
      input_json(sim_par, "Simulation", "Start d",
                 as.numeric(difftime(start_date_simulation,
                                     as.POSIXct(paste0(reference_year, "-01-01")),
                                     units = "days")))
    }
    end_date_simulation <- as.POSIXct(stop_date)
    input_json(sim_par, "Simulation", "End d",
               as.numeric(difftime(end_date_simulation,
                                         as.POSIXct(paste0(reference_year, "-01-01")),
                                         units = "days")))
    message("Simstrat: Model developers recommend using a timestep of 300s to prevent numerical instabilities. This will be used as the default value.\n`Simulation/Timestep s: 300`")
    input_json(sim_par, "Simulation", "Timestep s", 300)
  }

##---------------MyLake-------------
  if("MyLake" %in% model){
    # Load config file MyLake
    load(get_yaml_value(yaml, "config_files", "MyLake"))

    # update MyLakeR config file
    mylake_config[["M_start"]] <- start_date
    mylake_config[["M_stop"]] <- stop_date

    # save lake-specific config file for MyLake
    temp_fil <- gsub(".*/", "", get_yaml_value(yaml, "config_files", "MyLake"))
    save(mylake_config, file = file.path(folder, "MyLake", temp_fil))
  }

  message("export_time complete!")
}
