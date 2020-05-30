#'Export model configuration setups
#'
#'Exports all settings from the master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file for.
#'  Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#'@param dirs boolean; create directories for each model and if needed copies templates.
#'  Calls export_dirs. Defaults to TRUE
#'@param time boolean; exports time settings. Calls export_time. Defaults to TRUE.
#'@param location boolean; exports location and hypsograph settings. 
#'  Calls export_location. Defaults to TRUE. 
#'@param meteo boolean; export meteorology data.
#'  Calls export_meteo. Defaults to TRUE.
#'@param init_cond boolean; export initial conditions.
#'  Calls export_init_cond. Defaults to TRUE.
#'@param extinction boolean; export light extinction data.
#'  Calls export_extinction. Defaults to TRUE.
#'@param model_parameters boolean; export model parameters specificed in the yaml
#'  configuration file. Calls export_model_parameters. Defaults to TRUE.
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
#'@importFrom lubridate year floor_date ceiling_date
#'@importFrom gotmtools get_yaml_value streams_switch input_yaml input_nml
#'@importFrom glmtools read_nml set_nml write_nml
#'
#'@export

export_config <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                          dirs = TRUE, time = TRUE, location = TRUE, 
                          meteo = TRUE, init_cond = TRUE, extinction = TRUE, inflow = TRUE,
                          model_parameters = TRUE,
                          folder = ".", inflow_file = NULL){

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

  
  # Output depths
  output_depths <- get_yaml_value(config_file, "output", "depths")
  
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
  
  
##--------------------- Export sub-functions ---------------
  # Export directories and copy template files if needed
  if(dirs){
    export_dirs(config_file = config_file, model = model, folder = folder)
  }
  
  # Export time settings (start, stop, time step)
  if(time){
    export_time(config_file = config_file, model = model, folder = folder)
  }
  
  # Export hypsograph, lat/lon, ice
  if(location){
    export_location(config_file = config_file, model = model, folder = folder)
  }
  
  
##--------------------- FLake --------------------------------------------------------------------

  if("FLake" %in% model){
    
    input_nml(fla_fil, label = "METEO", key = "outputfile", paste0("'output/output.dat'"))
    
    if(!use_inflows){
      input_nml(fla_fil, label = "inflow", key = "Qfromfile",  ".false.")
    } else {
      input_nml(fla_fil, label = "inflow", key = "Qfromfile",  ".true.")
    }

    message("FLake configuration complete!")

  }

  ##----------------------- GLM --------------------------------------------------------------------
  
  if("GLM" %in% model){
    
    # Read in nml and input parameters
    nml <- read_nml(glm_nml)

    

    inp_list <- list("nsave" = round(out_tstep_s / timestep),
                     "out_dir" = "output",
                     "out_fn" = "output")
    if(!use_inflows){
      inp_list$num_inflows <- 0
      inp_list$num_outlet <- 0
    } else {
      inp_list$num_inflows <- 1
      inp_list$num_outlet <- 0
    }
    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    write_nml(nml, glm_nml)


    message("GLM configuration complete!")

  }

  ##--------------------- GOTM ---------------------------------------------------------------------
  
  if("GOTM" %in% model){

    
    
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
      # streams_switch(file = got_yaml, method = "off")
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "inflow", key3 = "flow", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "inflow", key3 = "temp", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "inflow", key3 = "salt", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "outflow", key3 = "flow", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "outflow", key3 = "temp", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "outflow", key3 = "salt", key4 =
                            "method", value = 0)
    } else {
      # streams_switch(file = got_yaml, method = "on")
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "inflow", key3 = "flow", key4 =
                            "method", value = 2)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "inflow", key3 = "temp", key4 =
                            "method", value = 2)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "inflow", key3 = "salt", key4 =
                            "method", value = 2)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "outflow", key3 = "flow", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "outflow", key3 = "temp", key4 =
                            "method", value = 0)
      input_yaml_multiple(got_yaml, key1 = "streams", key2 = "outflow", key3 = "salt", key4 =
                            "method", value = 0)
      
      
    }

    message("GOTM configuration complete!")
  }

  ##--------------------- Simstrat -----------------------------------------------------------------
  
  if("Simstrat" %in% model){
    
    input_json(sim_par, "Output", "Path", '"output"')
    
    input_json(sim_par, "Input", "Absorption", '"light_absorption.dat"')
    
    # Set output depths
    input_json(sim_par, "Input", "Grid", round(max_depth / output_depths))
    input_json(sim_par, "Output", "Depths", output_depths)
    input_json(sim_par, "Output", "Times", round(out_tstep_s / timestep))
    
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
    }else{
      inflow_line_1 <- "Time [d]\tQ_in [m3/s]"
      # In case Kw is a single value for the whole simulation:
      inflow_line_2 <- "1"
      inflow_line_3 <- "-1 0.00"
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      inflow_line_4 <- paste(start_sim, 0.000)
      inflow_line_5 <- paste(end_sim, 0.000)
      
      file_connection <- file("Simstrat/Qout.dat")
      writeLines(c(inflow_line_1, inflow_line_2, inflow_line_3, inflow_line_4, inflow_line_5),
                 file_connection)
      close(file_connection)
    }

    message("Simstrat configuration complete!")

  }

  ##--------------------- MyLake -------------------------------------------------------------------
  
  if("MyLake" %in% model){

    
    if(!use_inflows){
        mylake_config[["Inflw"]] <- matrix(rep(0, 8 * length(seq.Date(from = as.Date(start_date),
                                                                      to = as.Date(stop_date),
                                                                      by = "day"))),
                                      ncol = 8)
    } 
    
    message("MyLake configuration complete!")
  }
  
  # Export meteo
  if(meteo){
    export_meteo(config_file, model = model, folder = folder)
  }
  
  # Export initial conditions
  if(init_cond){
    export_init_cond(config_file, model = model, print = TRUE, folder = folder)
  } 
  
  # Export light extinction (Kw)
  if(extinction){
    export_extinction(config_file, model = model, folder = folder)
  }
  
  
  # Export user-defined inflow boundary condition
  if(use_inflows){
    export_inflow(config_file, model = model, folder = folder, use_outflows =
                    use_outflows, inflow_file = inflow_file)
  }
  
  # Export user-defined model-specific parameters
  if(model_parameters){
    export_model_parameters(config_file, model = model, folder = folder)
  }
}
