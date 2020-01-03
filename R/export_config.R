#'Export model configuration setups
#'
#'Create directory with file setups for each model based on a master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file. Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#'@param folder folder
#'@keywords methods
#'@author
#'Tadhg Moore, Jorrit Mesman
#'@examples
#'
#'
#'@importFrom stats approx
#'@import lubridate
#'@importFrom gotmtools get_yaml_value
#'@importFrom gotmtools streams_switch
#'@importFrom gotmtools input_yaml
#'@import glmtools
#'
#'@export

export_config <- function(config_file, model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.'){

  # Set working directory
  oldwd <- getwd()
  setwd(folder)

  # Fix time zone
  original_tz = Sys.getenv("TZ")
  Sys.setenv(TZ="GMT")


  # Read in all information from config_file that needs to be written to the model-specific config files

  # Latitude
  lat <- gotmtools::get_yaml_value(config_file, "location", "latitude")
  # Longitude
  lon <- gotmtools::get_yaml_value(config_file, "location", "longitude")
  # Maximum Depth
  max_depth = gotmtools::get_yaml_value(config_file, "location", "depth")
  # Read in hypsograph data
  hyp <- read.csv(gotmtools::get_yaml_value(config_file, "location", "hypsograph"))
  # Start date
  start_date <- gotmtools::get_yaml_value(config_file, "time", "start")
  # Stop date
  stop_date <- gotmtools::get_yaml_value(config_file, "time", "stop")
  # Time step
  timestep <- gotmtools::get_yaml_value(config_file, "time", "timestep")
  # Met time step
  met_timestep <- gotmtools::get_yaml_value(config_file, "meteo", "timestep")
  # Output depths
  output_depths <- gotmtools::get_yaml_value(config_file, "model_settings", "output_depths")
  # Extinction coefficient
  Kw <- gotmtools::get_yaml_value(config_file, "model_settings", "extinction_coefficient")
  # Use ice
  use_ice <- gotmtools::get_yaml_value(config_file, "ice", "use")
  # Use inflows
  use_inflows <- gotmtools::get_yaml_value(config_file, "inflows", "use")
  # Output
  out_tstep <- gotmtools::get_yaml_value(config_file, "output", "time_step")


  if("FLake" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('FLake')){
      dir.create('FLake')
    }
    if(!dir.exists('FLake/output')){
      dir.create('FLake/output')
    }

    # Read the FLake config file from config_file, and write it to the FLake directory
    temp_fil <- gotmtools::get_yaml_value(config_file, "config_files", "flake_config")
    if(file.exists(temp_fil)){
      fla_fil <- temp_fil
    }else{
      # This will work once we build the package
      # template_file <- system.file("extdata/flake_template.nml", package = 'LakeEnsemblR') # packageName()
      template_file <- '../../inst/extdata/flake_template.nml'
      file.copy(from = template_file, to = file.path(folder, 'FLake', basename(temp_fil)))
      fla_fil <- file.path(folder, 'FLake', basename(temp_fil))
    }


    # Calculate mean depth from hypsograph (mdepth = V / SA)
    # Calculate volume from hypsograph - converted to function?
    ## Needs to be double checked!
    bthA = hyp$Area_meterSquared
    bthD = hyp$Depth_meter
    top = min(bthD)
    bottom = max(bthD)
    layerD <- seq(top, bottom, 0.1)
    layerA <- stats::approx(bthD, bthA, layerD)$y
    vols <- c()
    for(i in 2:length(layerD)){
      h = layerD[i] - layerD[i-1]
      cal_v <- (h/3)*(layerA[i] + layerA[i-1] + sqrt(layerA[i] * layerA[i-1]))
      vols <- c(vols, cal_v)
    }
    vol = sum(vols)
    mean_depth = signif((vol / bthA[1]),4)
    ##

    # Input parameters
    input_nml(fla_fil, label = 'SIMULATION_PARAMS', key = 'del_time_lk', met_timestep) # Hard-code: meteo needs to be in same time step as model. Not yet supported (Jorrit, 2019-12-15)
    input_nml(fla_fil, label = 'SIMULATION_PARAMS', key = 'h_ML_in', mean_depth)
    input_nml(fla_fil, label = 'LAKE_PARAMS', key = 'depth_w_lk', mean_depth)
    input_nml(fla_fil, label = 'LAKE_PARAMS', key = 'latitude_lk', lat)
    input_nml(fla_fil, label = 'TRANSPARENCY', key = 'extincoef_optic', Kw)
    input_nml(fla_fil, label = 'METEO', key = 'outputfile', paste0("'output/output.dat'"))

    message('FLake configuration complete!')

  }

  if("GLM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('GLM')){
      dir.create('GLM')
    }
    if(!dir.exists('GLM/output')){
      dir.create('GLM/output')
    }

    # Read the GLM config file from config_file, and write it to the GLM directory
    temp_fil <- gotmtools::get_yaml_value(config_file, "config_files", "glm_config")

    if(file.exists(temp_fil)){
      glm_nml <- temp_fil
    }else{
      # This will work once we build the package
      # template_file <- system.file("extdata/glm3_template.nml", package = 'LakeEnsemblR') # packageName()
      template_file <- '../../inst/extdata/glm3_template.nml'
      file.copy(from = template_file, to = file.path(folder, 'GLM', basename(temp_fil)))
      glm_nml <- file.path(folder, 'GLM', basename(temp_fil))
    }

    # Format hypsograph
    glm_hyp <- hyp
    glm_hyp[,1] <- glm_hyp[nrow(glm_hyp),1] - glm_hyp[,1]

    # Read in nml and input parameters
    nml <- glmtools::read_nml(glm_nml)
    inp_list <- list('lake_name' = gotmtools::get_yaml_value(config_file, "location", "name"),
                     'latitude' = lat,
                     'longitude' = lon,
                     'lake_depth' = max_depth,
                     'Kw' = Kw,
                     'crest_elev' = max(-(glm_hyp[,1])),
                     'bsn_vals'=length(glm_hyp[,1]) ,
                     'H' = -(glm_hyp[,1]),
                     'A' = rev(glm_hyp[,2] ),
                     'start' = start_date,
                     'stop' = stop_date,
                     'dt' = timestep, # Hard-code: When choosing dt = 86400, GLM only gives daily output when dt = 3600? (Jorrit, 2019-12-15)
                     'nsave' = out_tstep,
                     'out_dir' = 'output',
                     'out_fn' = 'output',
                     'timefmt' = 2)
    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    glmtools::write_nml(nml, glm_nml)


    message('GLM configuration complete!')

  }

  if("GOTM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('GOTM')){
      dir.create('GOTM')
    }
    if(!dir.exists('GOTM/output')){
      dir.create('GOTM/output')
    }

    # Read the GOTM config file from config_file, and write it to the GOTM directory
    temp_fil <- gotmtools::get_yaml_value(config_file, "config_files", "gotm_config")
    if(file.exists(temp_fil)){
      got_yaml <- temp_fil
    }else{
      # This will work once we build the package
      # template_file <- system.file("extdata/gotm_template.yaml", package = 'LakeEnsemblR') # packageName()
      template_file <- '../../inst/extdata/gotm_template.yaml'
      file.copy(from = template_file, to = file.path(folder, 'GOTM', basename(temp_fil)))
      got_yaml <- file.path(folder, 'GOTM', basename(temp_fil))
    }

    # Get output.yaml from the GOTMr package and copy to the GOTM folder
    out_fil <- system.file('extdata/output.yaml', package= 'GOTMr')
    file.copy(from = out_fil, to = 'GOTM')

    # Write input parameters to got_yaml
    gotmtools::input_yaml(got_yaml, 'location', 'name', gotmtools::get_yaml_value(config_file, "location", "name"))
    gotmtools::input_yaml(got_yaml, 'location', 'latitude', lat)
    gotmtools::input_yaml(got_yaml, 'location', 'longitude', lon)

    # Set max depth
    gotmtools::input_yaml(got_yaml, 'location', 'depth', max_depth)
    gotmtools::input_yaml(got_yaml, 'grid', 'nlev', (max_depth/0.5))

    # Switch on ice model - MyLake
    gotmtools::input_yaml(got_yaml, 'ice', 'model', 2)


    # Create GOTM hypsograph file
    ndeps <- nrow(hyp)
    got_hyp <- hyp
    got_hyp[,1] <- -got_hyp[,1]
    colnames(got_hyp) <- c(as.character(ndeps), '2')
    write.table(got_hyp, 'GOTM/hypsograph.dat', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
    gotmtools::input_yaml(got_yaml, 'location', 'hypsograph', "hypsograph.dat")

    # Set time settings
    gotmtools::input_yaml(got_yaml, 'time', 'start', start_date)
    gotmtools::input_yaml(got_yaml, 'time', 'stop', stop_date)
    gotmtools::input_yaml(got_yaml, 'time', 'dt', timestep)

    # Set GOTM output
    out_yaml <-file.path(folder, 'GOTM', 'output.yaml')
    gotmtools::input_yaml(out_yaml, 'output', 'time_step', out_tstep)
    gotmtools::input_yaml(out_yaml, 'output', 'time_unit', 'hour')

    ## Input light extinction data [To be continued...]


    ## Switch off streams
    if(!use_inflows){
      gotmtools::streams_switch(file = got_yaml, method = 'off')
    }

    message('GOTM configuration complete!')
  }

  if("Simstrat" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('Simstrat')){
      dir.create('Simstrat')
    }
    if(!dir.exists('Simstrat/output')){
      dir.create('Simstrat/output')
    }

    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    temp_fil <- gotmtools::get_yaml_value(config_file, "config_files", "simstrat_config")
    if(file.exists(temp_fil)){
      sim_par <- temp_fil
    }else{
      # This will work once we build the package
      # template_file <- system.file("extdata/simstrat_template.par", package = 'LakeEnsemblR') # packageName()
      template_file <- '../../inst/extdata/simstrat_template.par'
      file.copy(from = template_file, to = file.path(folder, 'Simstrat', basename(temp_fil)))
      sim_par <- file.path(folder, 'Simstrat', basename(temp_fil))
    }

    # Copy in template files from examples folder in the package
    light_fil <- system.file('extdata/absorption_langtjern.dat', package= 'SimstratR')
    qin_fil <- system.file('extdata/Qin.dat', package= 'SimstratR')
    qout_fil <- system.file('extdata/Qout.dat', package= 'SimstratR')
    tin_fil <- system.file('extdata/Tin.dat', package= 'SimstratR')
    sin_fil <- system.file('extdata/Sin.dat', package= 'SimstratR')
    file.copy(from = light_fil, to = file.path(folder, 'Simstrat','light_absorption.dat'))
    file.copy(from = qin_fil, to = file.path(folder, 'Simstrat','Qin.dat'))
    file.copy(from = qout_fil, to = file.path(folder, 'Simstrat','Qout.dat'))
    file.copy(from = tin_fil, to = file.path(folder, 'Simstrat','Tin.dat'))
    file.copy(from = sin_fil, to = file.path(folder, 'Simstrat','Sin.dat'))

    # Create Simstrat bathymetry
    sim_hyp <- hyp
    sim_hyp[,1] <- -sim_hyp[,1]
    colnames(sim_hyp) <- c('Depth [m]',	'Area [m^2]')
    write.table(sim_hyp, 'Simstrat/hypsograph.dat', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)

    # Input parameters
    input_json(sim_par, 'Input', 'Morphology', '"hypsograph.dat"')
    input_json(sim_par, 'Input', 'Absorption', '"light_absorption.dat"')
    input_json(sim_par, 'Output', 'Path', '"output"')
    input_json(sim_par, 'ModelParameters', 'lat', lat)

    # Set output depths
    input_json(sim_par, "Input", "Grid", round(max_depth/output_depths))
    input_json(sim_par, "Output", "Depths", output_depths)

    # Set times
    reference_year <- lubridate::year(as.POSIXct(start_date))
    input_json(sim_par, "Simulation", "Start year", reference_year)
    start_date_simulation <- lubridate::floor_date(as.POSIXct(start_date), unit = "days")
    end_date_simulation <- lubridate::ceiling_date(as.POSIXct(stop_date), unit = "days")
    input_json(sim_par, "Simulation", "Start d", round(as.numeric(difftime(start_date_simulation, as.POSIXct(paste0(reference_year,"-01-01")), units = "days"))))
    input_json(sim_par, "Simulation", "End d", round(as.numeric(difftime(end_date_simulation, as.POSIXct(paste0(reference_year,"-01-01")), units = "days"))))
    input_json(sim_par, "Simulation", "Timestep s", timestep)
    input_json(sim_par, "Output", "Times", out_tstep)


    # Turn off ice and snow
    if(use_ice){
      input_json(sim_par, "ModelConfig", "IceModel", 1)
    }else{
      input_json(sim_par, "ModelConfig", "IceModel", 0)
      input_json(sim_par, "ModelConfig", "SnowModel", 0)
    }

    ## Input light absorption data
    absorption_line_1 <- "Time [d] (1.col)    z [m] (1.row)    Absorption [m-1] (rest)"
    # In case Kw is a single value for the whole simulation:
    absorption_line_2 <- "1"
    absorption_line_3 <- "-1 -1.00"
    start_sim <- get_json_value(sim_par, "Simulation", "Start d")
    end_sim <- get_json_value(sim_par, "Simulation", "End d")
    absorption_line_4 <- paste(start_sim,Kw)
    absorption_line_5 <- paste(end_sim,Kw)

    fileConnection <- file("Simstrat/light_absorption.dat")
    writeLines(c(absorption_line_1,absorption_line_2,absorption_line_3,absorption_line_4,absorption_line_5), fileConnection)
    close(fileConnection)

    # Turn off inflow
    if(!use_inflows){
      ## Set Qin and Qout to 0 inflow
      inflow_line_1 <- "Time [d]\tQ_in [m3/s]"
      # In case Kw is a single value for the whole simulation:
      inflow_line_2 <- "1"
      inflow_line_3 <- "-1 0.00"
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      inflow_line_4 <- paste(start_sim,0.000)
      inflow_line_5 <- paste(end_sim,0.000)

      fileConnection <- file("Simstrat/Qin.dat")
      writeLines(c(inflow_line_1,inflow_line_2,inflow_line_3,inflow_line_4,inflow_line_5), fileConnection)
      close(fileConnection)
      fileConnection <- file("Simstrat/Qout.dat")
      writeLines(c(inflow_line_1,inflow_line_2,inflow_line_3,inflow_line_4,inflow_line_5), fileConnection)
      close(fileConnection)
    }

    message('Simstrat configuration complete!')

  }

  setwd(oldwd)
  Sys.setenv(TZ=original_tz)
}
