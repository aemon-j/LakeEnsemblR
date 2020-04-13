#'Export model configuration setups
#'
#'Create directory with file setups for each model based on a master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#'@param folder folder
#'@param inflow_file filepath; to inflow file which is in the standardised LakeEnsemblR format (if
#' a different file than the one provided in the configuration file is needed); default is NULL
#'@keywords methods
#'@author
#'Tadhg Moore, Jorrit Mesman, Johannes Feldbauer, Robert Ladwig
#'@examples
#'
#'
#'@importFrom stats approx
#'@import lubridate
#'@importFrom gotmtools get_yaml_value streams_switch input_yaml input_nml
#'@importFrom glmtools read_nml set_nml write_nml
#'
#'@export

export_config <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                          folder = ".", inflow_file = NULL) {

  # Check if config file exists
  if(!file.exists(config_file)){
    stop(config_file, " does not exist.")
  }
  
  # check the master config file
  check_master_config(config_file, exp_cnf = TRUE)
  # check model input
  model <- check_models(model)
  
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


  # Read in all information from config_file that needs to be
  # written to the model-specific config files

  # Latitude
  lat <- get_yaml_value(config_file, "location", "latitude")
  # Longitude
  lon <- get_yaml_value(config_file, "location", "longitude")
  # Elevation
  elev <- get_yaml_value(config_file, "location", "elevation")
  # Maximum Depth
  max_depth <- get_yaml_value(config_file, "location", "depth")
  # Read in hypsograph data
  hyp_file <- get_yaml_value(config_file, "location", "hypsograph")
  if(!file.exists(hyp_file)){
    stop(hyp_file, " does not exist. Check filepath in ", config_file)
  }
  hyp <- read.csv(hyp_file)
  # Start date
  start_date <- get_yaml_value(config_file, "time", "start")
  # Stop date
  stop_date <- get_yaml_value(config_file, "time", "stop")
  # Time step
  timestep <- get_yaml_value(config_file, "time", "time_step")
  # Met time step (uses get_meteo_time_step function, in helpers.R)
  met_timestep <- get_meteo_time_step(file.path(folder,
                                                get_yaml_value(config_file, "meteo", "file")))
  # Output depths
  output_depths <- get_yaml_value(config_file, "output", "depths")
  # Use ice
  use_ice <- get_yaml_value(config_file, "ice", "use")
  # Use inflows
  use_inflows <- get_yaml_value(config_file, "inflows", "use")
  # Use counter outflows
  use_outflows <- get_yaml_value(config_file, "inflows", "mass-balance")
  # Output timestep
  out_tstep <- get_yaml_value(config_file, "output", "time_step")
  # Output unit
  out_unit <- get_yaml_value(config_file, "output", "time_unit")
  # Output timestep in seconds
  conv_l <- list(second = 1, hour = 3600, day = 86400)
  out_tstep_s <- out_tstep * conv_l[[out_unit]]
  # Output timestep in 1/d
  out_tstep_d <- out_tstep_s/86400
  
##--------------------- FLake --------------------------------------------------------------------

  if("FLake" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("FLake")){
      dir.create("FLake")
    }
    if(!dir.exists("FLake/output")){
      dir.create("FLake/output")
    }

    # Read the FLake config file from config_file, and write it to the FLake directory
    temp_fil <- get_yaml_value(config_file, "config_files", "FLake")
    if(file.exists(temp_fil)){
      fla_fil <- temp_fil
    }else{
      template_file <- system.file("extdata/flake_template.nml", package = packageName())
      file.copy(from = template_file,
                to = file.path(folder, get_yaml_value(config_file, "config_files", "FLake")))
      fla_fil <- file.path(folder, get_yaml_value(config_file, "config_files", "FLake"))
    }
    
    
    
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
                       "). Pleas re-run export_meteo."))
      }
      # check if stop date fits
      if(max(met_flake$V6) != as.POSIXct(stop_date)) {
        warning(paste0("FLake stop date (", as.character(max(met_flake$V6)),
                       ') does not fit stop date specified in master control file "',
                       config_file, '"( ', as.character(stop_date),
                       "). Pleas re-run export_meteo."))
      }
      # set timesteps
      input_nml(fla_fil, "SIMULATION_PARAMS", "time_step_number", nrow(met_flake))
    }
    # Calculate mean depth from hypsograph (mdepth = V / SA)
    # Calculate volume from hypsograph - converted to function?
    ## Needs to be double checked!
    bth_area <- hyp$Area_meterSquared
    bth_depth <- hyp$Depth_meter
    top <- min(bth_depth)
    bottom <- max(bth_depth)
    layer_d <- seq(top, bottom, 0.1)
    layer_a <- stats::approx(bth_depth, bth_area, layer_d)$y
    vols <- c()
    for(i in 2:length(layer_d)){
      h <- layer_d[i] - layer_d[i - 1]
      cal_v <- (h / 3) * (layer_a[i] + layer_a[i - 1] + sqrt(layer_a[i] * layer_a[i - 1]))
      vols <- c(vols, cal_v)
    }
    vol <- sum(vols)
    mean_depth <- signif((vol / bth_area[1]), 4)
    ##

    # Input parameters
    input_nml(fla_fil, label = "SIMULATION_PARAMS", key = "del_time_lk", met_timestep)
    #meteo needs to be in same time step as model
    input_nml(fla_fil, label = "SIMULATION_PARAMS", key = "h_ML_in", mean_depth)
    input_nml(fla_fil, label = "LAKE_PARAMS", key = "depth_w_lk", mean_depth)
    input_nml(fla_fil, label = "LAKE_PARAMS", key = "latitude_lk", lat)
    input_nml(fla_fil, label = "METEO", key = "outputfile", paste0("'output/output.dat'"))
    
    if(!use_inflows){
      input_nml(fla_fil, label = "inflow", key = "Qfromfile",  '.false.')
    }

    message("FLake configuration complete!")

  }

  ##----------------------- GLM --------------------------------------------------------------------
  
  if("GLM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("GLM/output")){
      dir.create("GLM/output", recursive = TRUE)
    }

    # Read the GLM config file from config_file, and write it to the GLM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "GLM")

    if(file.exists(temp_fil)){
      glm_nml <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/glm3_template.nml", package = packageName()) #
      file.copy(from = template_file,
                to = file.path(folder, get_yaml_value(config_file, "config_files", "GLM")))
      glm_nml <- file.path(folder, get_yaml_value(config_file, "config_files", "GLM"))
    }

    # Format hypsograph
    glm_hyp <- hyp
    glm_hyp[, 1] <- elev - glm_hyp[, 1] # this doesn't take into account GLM's lake elevation

    # Calculate bsn_len & bsn_wid:
    # Calculate basin dims assume ellipse with width is twice the length
    Ao <- max(glm_hyp[, 2])
    bsn_wid <- sqrt((2 * Ao) / pi)
    bsn_len <- 2 * bsn_wid
    # Can be overwritten by providing values in the model_parameters section of config_file

    # Read in nml and input parameters
    nml <- read_nml(glm_nml)

    # Calculate max number of layers
    min_layer_thick <- get_nml_value(nml, "min_layer_thick")
    max_layers <- round(max_depth / min_layer_thick)


    inp_list <- list("lake_name" = get_yaml_value(config_file, "location", "name"),
                     "latitude" = lat,
                     "longitude" = lon,
                     "lake_depth" = max_depth,
                     "crest_elev" = max((glm_hyp[, 1])),
                     "bsn_vals" = length(glm_hyp[, 1]),
                     "H" = rev(glm_hyp[, 1]),
                     "A" = rev(glm_hyp[, 2]),
                     "start" = start_date,
                     "stop" = stop_date,
                     "dt" = timestep,
                     "bsn_len" = bsn_len,
                     "bsn_wid" = bsn_wid,
                     "max_layers" = max_layers,
                     "max_layer_thick" = 1.0,
                     "nsave" = round(out_tstep_s / timestep),
                     "out_dir" = "output",
                     "out_fn" = "output",
                     "timefmt" = 2,
                     "timezone" = 0)
    if(!use_inflows){
      inp_list$num_inflows <- 0
      inp_list$num_outlet <- 0
    }
    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    write_nml(nml, glm_nml)


    message("GLM configuration complete!")

  }

  ##--------------------- GOTM ---------------------------------------------------------------------
  
  if("GOTM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("GOTM/output")){
      dir.create("GOTM/output", recursive = TRUE)
    }

    # Read the GOTM config file from config_file, and write it to the GOTM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "GOTM")
    if(file.exists(temp_fil)){
      got_yaml <- file.path(folder, temp_fil)
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/gotm_template.yaml", package = packageName())
      file.copy(from = template_file,
                to = file.path(folder, get_yaml_value(config_file, "config_files", "GOTM")))
      got_yaml <- file.path(folder, get_yaml_value(config_file, "config_files", "GOTM"))
    }

    # Get output.yaml from the GOTMr package and copy to the GOTM folder
    out_fil <- system.file("extdata/output.yaml", package = "GOTMr")
    file.copy(from = out_fil, to = "GOTM/output.yaml") # was just "GOTM", does this overwrite the GOTM dir?

    # Write input parameters to got_yaml
    input_yaml(got_yaml, "location", "name", get_yaml_value(config_file, "location", "name"))
    input_yaml(got_yaml, "location", "latitude", lat)
    input_yaml(got_yaml, "location", "longitude", lon)

    # Set max depth
    input_yaml(got_yaml, "location", "depth", max_depth)
    input_yaml(got_yaml, "grid", "nlev", round(max_depth / 0.5))

    # Switch on ice model - MyLake
    # input_yaml(got_yaml, "ice", "model", 2)


    # Create GOTM hypsograph file
    ndeps <- nrow(hyp)
    got_hyp <- hyp
    got_hyp[, 1] <- -got_hyp[, 1]
    colnames(got_hyp) <- c(as.character(ndeps), "2")
    write.table(got_hyp, "GOTM/hypsograph.dat", quote = FALSE,
                sep = "\t", row.names = FALSE, col.names = TRUE)
    input_yaml(got_yaml, "location", "hypsograph", "hypsograph.dat")

    
    # Set time settings
    input_yaml(got_yaml, "time", "start", start_date)
    input_yaml(got_yaml, "time", "stop", stop_date)
    input_yaml(got_yaml, "time", "dt", timestep)

    # Set GOTM output
    out_yaml <- file.path(folder, "GOTM", "output.yaml")
    input_yaml(out_yaml, "output", "time_step", out_tstep)
    input_yaml(out_yaml, "output", "time_unit", out_unit)
    # Need to input start and stop into yaml file
    time_method <- get_yaml_value(config_file, "output", "time_method")
    input_yaml(got_yaml, label = "output", key = "time_method", value = time_method)
    input_yaml(got_yaml, label = "output", key = "format", value = "netcdf")


    ## Switch off streams
    if(!use_inflows){
      streams_switch(file = got_yaml, method = "off")
    }

    message("GOTM configuration complete!")
  }

  ##--------------------- Simstrat -----------------------------------------------------------------
  
  if("Simstrat" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("Simstrat/output")){
      dir.create("Simstrat/output", recursive = TRUE)
    }

    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    temp_fil <- get_yaml_value(config_file, "config_files", "Simstrat")
    if(file.exists(temp_fil)){
      sim_par <- temp_fil
    }else{
      template_file <- system.file("extdata/simstrat_template.par", package = packageName())
      file.copy(from = template_file,
                to = file.path(folder, get_yaml_value(config_file, "config_files", "Simstrat")))
      sim_par <- file.path(folder, get_yaml_value(config_file, "config_files", "Simstrat"))
    }

    # Copy in template files from examples folder in the package
    qin_fil <- system.file("extdata/Qin.dat", package = "SimstratR")
    qout_fil <- system.file("extdata/Qout.dat", package = "SimstratR")
    tin_fil <- system.file("extdata/Tin.dat", package = "SimstratR")
    sin_fil <- system.file("extdata/Sin.dat", package = "SimstratR")
    file.copy(from = qin_fil, to = file.path(folder, "Simstrat", "Qin.dat"))
    file.copy(from = qout_fil, to = file.path(folder, "Simstrat", "Qout.dat"))
    file.copy(from = tin_fil, to = file.path(folder, "Simstrat", "Tin.dat"))
    file.copy(from = sin_fil, to = file.path(folder, "Simstrat", "Sin.dat"))

    # Create Simstrat bathymetry
    sim_hyp <- hyp
    sim_hyp[, 1] <- -sim_hyp[, 1]
    colnames(sim_hyp) <- c("Depth [m]",	"Area [m^2]")
    write.table(sim_hyp, "Simstrat/hypsograph.dat", quote = FALSE,
                sep = "\t", row.names = FALSE, col.names = TRUE)

    
    # Input parameters
    # need to source helper_functions/input_json.R for this function:
    input_json(sim_par, "Input", "Morphology", '"hypsograph.dat"')
    input_json(sim_par, "Input", "Absorption", '"light_absorption.dat"')
    input_json(sim_par, "Output", "Path", '"output"')
    input_json(sim_par, "ModelParameters", "lat", lat)

    # Set output depths
    input_json(sim_par, "Input", "Grid", round(max_depth / output_depths))
    input_json(sim_par, "Output", "Depths", output_depths)

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
    input_json(sim_par, "Output", "Times", out_tstep_d)


    # Turn off ice and snow
    if(use_ice){
      input_json(sim_par, "ModelConfig", "IceModel", 1)
    }else{
      input_json(sim_par, "ModelConfig", "IceModel", 0)
      input_json(sim_par, "ModelConfig", "SnowModel", 0)
    }

    # Turn off inflow
    if(!use_inflows){
      ## Set Qin and Qout to 0 inflow
      inflow_line_1 <- "Time [d]\tQ_in [m3/s]"
      # In case Kw is a single value for the whole simulation:
      inflow_line_2 <- "1"
      inflow_line_3 <- "-1 0.00"
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      inflow_line_4 <- paste(start_sim, 0.000)
      inflow_line_5 <- paste(end_sim, 0.000)

      file_connection <- file("Simstrat/Qin.dat")
      writeLines(c(inflow_line_1, inflow_line_2, inflow_line_3, inflow_line_4, inflow_line_5),
                 file_connection)
      close(file_connection)
      file_connection <- file("Simstrat/Qout.dat")
      writeLines(c(inflow_line_1, inflow_line_2, inflow_line_3, inflow_line_4, inflow_line_5),
                 file_connection)
      close(file_connection)
    }

    message("Simstrat configuration complete!")

  }

  ##--------------------- MyLake -------------------------------------------------------------------
  
  if("MyLake" %in% model){

    # wind sheltering coefficient (C_shelter)
    c_shelter <- 1.0 - exp(-0.3 * (hyp$Area_meterSquared[1] * 1e-6))

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("MyLake")){
      dir.create("MyLake")
    }

    # Load config file MyLake
    temp_fil <- get_yaml_value(config_file, "config_files", "MyLake")
    if(file.exists(temp_fil)){
      load(temp_fil)
    }else{
      # Load template config file from extdata
      mylake_path <- system.file(package = "LakeEnsemblR")
      load(file.path(mylake_path, "extdata", "mylake_config_template.Rdata"))
    }
    
    # update MyLakeR config file
    mylake_config[["M_start"]] <- start_date
    mylake_config[["M_stop"]] <- stop_date
    mylake_config[["Phys.par"]][5] <- c_shelter
    mylake_config[["Phys.par"]][6] <- lat
    mylake_config[["Phys.par"]][7] <- lon
    mylake_config[["In.Az"]] <- as.matrix(hyp$Area_meterSquared)
    mylake_config[["In.Z"]] <- as.matrix(hyp$Depth_meter)
    mylake_config[["In.FIM"]] <- matrix(rep(0.92, nrow(hyp)), ncol = 1)
    mylake_config[["In.Chlz.sed"]] <- matrix(rep(196747, nrow(hyp)), ncol = 1)
    mylake_config[["In.TPz.sed"]] <- matrix(rep(756732, nrow(hyp)), ncol = 1)
    mylake_config[["In.DOCz"]] <- matrix(rep(3000, nrow(hyp)), ncol = 1)
    mylake_config[["In.Chlz"]] <- matrix(rep(7, nrow(hyp)), ncol = 1)
    mylake_config[["In.DOPz"]] <- matrix(rep(7, nrow(hyp)), ncol = 1)
    mylake_config[["In.TPz"]] <- matrix(rep(21, nrow(hyp)), ncol = 1)
    mylake_config[["In.Sz"]] <- matrix(rep(0, nrow(hyp)), ncol = 1)
    mylake_config[["In.Cz"]] <- matrix(rep(0, nrow(hyp)), ncol = 1)

    if(!use_inflows){
        mylake_config[["Inflw"]] <- matrix(rep(0, 8 * length(seq.Date(from = as.Date(start_date),
                                                                      to = as.Date(stop_date),
                                                                      by = "day"))),
                                      ncol = 8)
    }

    temp_fil <- gsub(".*/", "", temp_fil)
    # save lake-specific config file for MyLake
    save(mylake_config, file = file.path(folder, "MyLake", temp_fil))

    message("MyLake configuration complete!")
  }

  # Light extinction (Kw) in separate function
  export_extinction(config_file, model = model, folder = folder)
  
  # Export user-defined inflow boundary condition
  if(use_inflows){
    export_inflow(config_file, model = model, folder = folder, use_outflows =
                    use_outflows, inflow_file = inflow_file)
  }
  
  # Export user-defined model-specific parameters
  export_model_parameters(config_file, model = model, folder = folder)
}
