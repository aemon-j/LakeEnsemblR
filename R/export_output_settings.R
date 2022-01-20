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
#'@importFrom glmtools read_nml set_nml write_nml
#' @importFrom gotmtools read_yaml set_yaml write_yaml get_yaml_value
#'
#'@export

export_output_settings <- function(config_file,
                                   model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                                   folder = "."){

  if(!file.exists(file.path(folder, config_file))) {
    stop(paste0(file.path(folder, config_file), " does not exist. Make sure your file path is correct"))
  } else {
    yaml <- read_yaml(config_file)
  }
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

  # Time step
  timestep <- get_yaml_value(yaml, "time", "time_step")

  # Output depths
  output_depths <- get_yaml_value(yaml, "output", "depths")
  # Output time step
  out_tstep <- as.integer(get_yaml_value(yaml, "output", "time_step"))
  # Output time unit
  out_unit <- get_yaml_value(yaml, "output", "time_unit")
  # Output time step in seconds
  conv_l <- list(second = 1, hour = 3600, day = 86400)
  out_tstep_s <- out_tstep * conv_l[[out_unit]]


  ##---------------FLake-------------
  if("FLake" %in% model){
    fla_fil <- file.path(folder, get_yaml_value(yaml, "config_files", "FLake"))

    input_nml(fla_fil, label = "METEO", key = "outputfile", paste0("'output/output.dat'"))
  }

  ##---------------GLM-------------

  if("GLM" %in% model){
    glm_nml <- file.path(folder, get_yaml_value(yaml, "config_files", "GLM"))

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
    got_file <- file.path(folder, get_yaml_value(yaml, "config_files", "GOTM"))
    got_yaml <- read_yaml(got_file)

    # Set GOTM output
    out_file <- file.path(folder, "GOTM", "output.yaml")
    out_yaml <- read_yaml(out_file)
    out_yaml <- set_yaml(out_yaml, "output/output", "time_step", value = out_tstep)
    out_yaml <- set_yaml(out_yaml, "output/output", "time_unit", value = out_unit)
    write_yaml(out_yaml, out_file)

    # Need to input start and stop into yaml file
    time_method <- get_yaml_value(yaml, "output", "time_method")
    got_yaml <- set_yaml(got_yaml, label = "output", "output", key = "time_method", value = time_method)
    got_yaml <- set_yaml(got_yaml, label = "output", "output", key = "format", value = "netcdf")

    write_yaml(got_yaml, got_file)

  }

  ##---------------Simstrat-------------
  if("Simstrat" %in% model){
    sim_par <- file.path(folder, get_yaml_value(yaml, "config_files", "Simstrat"))

    input_json(sim_par, "Output", "Path", '"output/"')

    # Set output
    input_json(sim_par, "Output", "Depths", output_depths)
    input_json(sim_par, "Output", "Times", round(out_tstep_s / timestep))
  }

  ##---------------MyLake-------------

  # MyLake has no specific settings for output


  message("export_output_settings complete!")
}
