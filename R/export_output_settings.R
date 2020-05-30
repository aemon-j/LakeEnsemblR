#'Export output settings for each model
#'
#'Exports settings related to output (time step, format),
#'  for each model
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'@importFrom gotmtools get_yaml_value input_yaml
#'@importFrom glmtools read_nml set_nml write_nml
#'
#'@export

export_output_settings <- function(config_file,
                                   model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
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
  
  ##-------------Read settings---------------
  
  # Time step
  timestep <- get_yaml_value(config_file, "time", "time_step")
  
  # Output depths
  output_depths <- get_yaml_value(config_file, "output", "depths")
  # Output time step
  out_tstep <- get_yaml_value(config_file, "output", "time_step")
  # Output time unit
  out_unit <- get_yaml_value(config_file, "output", "time_unit")
  # Output time step in seconds
  conv_l <- list(second = 1, hour = 3600, day = 86400)
  out_tstep_s <- out_tstep * conv_l[[out_unit]]
  
  
  ##---------------FLake-------------
  if("FLake" %in% model){
    fla_fil <- file.path(folder, get_yaml_value(config_file, "config_files", "FLake"))
    
    input_nml(fla_fil, label = "METEO", key = "outputfile", paste0("'output/output.dat'"))
  }
  
  ##---------------GLM-------------
  
  if("GLM" %in% model){
    glm_nml <- file.path(folder, get_yaml_value(config_file, "config_files", "GLM"))
    
    # Read in nml and input parameters
    nml <- read_nml(glm_nml)
    
    inp_list <- list("nsave" = round(out_tstep_s / timestep),
                     "out_dir" = "output",
                     "out_fn" = "output")
    
    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    write_nml(nml, glm_nml)
  }
  
  ##---------------GOTM-------------
  if("GOTM" %in% model){
    got_yaml <- file.path(folder, get_yaml_value(config_file, "config_files", "GOTM"))
    
    # Set GOTM output
    out_yaml <- file.path(folder, "GOTM", "output.yaml")
    input_yaml(out_yaml, "output", "time_step", out_tstep)
    input_yaml(out_yaml, "output", "time_unit", out_unit)
    # Need to input start and stop into yaml file
    time_method <- get_yaml_value(config_file, "output", "time_method")
    input_yaml(got_yaml, label = "output", key = "time_method", value = time_method)
    input_yaml(got_yaml, label = "output", key = "format", value = "netcdf")
    
  }
  
  ##---------------Simstrat-------------
  if("Simstrat" %in% model){
    sim_par <- file.path(folder, get_yaml_value(config_file, "config_files", "Simstrat"))
    
    input_json(sim_par, "Output", "Path", '"output"')
    
    # Set output
    input_json(sim_par, "Output", "Depths", output_depths)
    input_json(sim_par, "Output", "Times", round(out_tstep_s / timestep))
  }
  
  ##---------------MyLake-------------
  
  # MyLake has no specific settings for output
  
  
  message("export_output_settings complete!")
}