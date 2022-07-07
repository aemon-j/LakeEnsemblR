#' Export LakeEnsemblR standardized input to model specific driver format
#'
#' Export driver files for each model
#'
#' @inheritParams export_config
#'
#' @examples
#' \dontrun{
#' export_meteo(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'),
#'              meteo_file = 'LakeEnsemblR_meteo_standard.csv')
#' }
#' @importFrom gotmtools calc_cc calc_in_lwr read_yaml set_yaml write_yaml get_yaml_value
#' @importFrom glmtools read_nml set_nml write_nml
#' @importFrom zoo na.approx
#' @importFrom lubridate floor_date seconds
#'
#' @export
export_meteo <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")){

  if(!file.exists(config_file)) {
    stop(config_file, " does not exist. Make sure your file path is correct")
  } else {
    yaml <- gotmtools::read_yaml(config_file)
  }

  Sys.setenv(TZ = "GMT")

  # Load Rdata
  data("met_var_dic", package = "LakeEnsemblR", envir = environment())

  # check model input
  model <- check_models(model)

  meteo_file <- get_yaml_value(yaml, "input", "meteo", "file")
  # Check if file exists
  if(!file.exists(meteo_file)){
    stop(meteo_file, " does not exist. Check filepath in ", config_file)
  }

  met_timestep <- get_meteo_time_step(file.path(meteo_file))

  ### Import data
  message("Loading met data...", paste0("[", Sys.time(), "]"))
  # Read meteo file
  suppressMessages({
    met <- read.csv(meteo_file)
  })
  message("Finished loading met data!", paste0("[", Sys.time(), "]"))

  met[, 1] <- as.POSIXct(met[["datetime"]])
  # Check time step
  tstep <- diff(as.numeric(met[["datetime"]]))

  if((mean(tstep) - 86400) / 86400 < -0.05) {
    subdaily <- TRUE
  }else{
    subdaily <- FALSE
  }

  ### Naming conventions standard input
  # test if names are right
  chck_met <- sapply(list(colnames(met)), function(x) x %in% met_var_dic$standard_name)
  if(any(!chck_met)) {
    stop(paste0("Colnames of meteo file are not in standard notation!\n",
                "Colnames: ", paste0(colnames(met)[!chck_met], collapse = ", "),
                ifelse(sum(!chck_met)>1, " are", " is")," wrong.\n",
                "They should be one of: \n", paste0(met_var_dic$standard_name,
                                                    collapse = "\n")))
  }


  # FLake
  #####
  if("FLake" %in% model){

    fla_met <- format_met(met = met, model = "FLake", config_file = config_file)

    # Met output file name
    met_outfile <- "all_meteo_file.dat"
    met_outfpath <- file.path("FLake", met_outfile)


    # Write meteo file, potentially with the scaling factors in the config_file
    # Using create_scaling_factors in the helpers.R script
    scale_param <- create_scaling_factors(config_file, "FLake")
    scale_met(fla_met, pars = scale_param, model = "FLake", out_file = met_outfpath)


    # Input values to nml
    nml_file <- file.path(get_yaml_value(yaml, "config_files", "FLake"))

    nml <- glmtools::read_nml(nml_file)

    nml <- glmtools::set_nml(nml, "SIMULATION_PARAMS::time_step_number", nrow(fla_met))
    nml <- glmtools::set_nml(nml, "meteofile", met_outfile)

    glmtools::write_nml(nml, nml_file)

    message("FLake: Created file ", file.path("FLake", met_outfile))

  }

  # GLM
  #####
  if("GLM" %in% model){
    glm_met <- format_met(met = met, model = "GLM", config_file = config_file)

    met_outfile <- file.path("GLM", "meteo_file.csv")

    # Write meteo file, potentially with the scaling factors in the config_file
    # Using create_scaling_factors in the helpers.R script
    scale_param <- create_scaling_factors(config_file, "GLM")
    scale_met(glm_met, pars = scale_param, model = "GLM", out_file = met_outfile)

    # Input to nml file
    nml_path <- file.path(get_yaml_value(yaml, "config_files", "GLM"))
    nml <- glmtools::read_nml(nml_path)

    nml_list <- list("subdaily" = subdaily, "lw_type" = "LW_IN", "meteo_fl" = "meteo_file.csv")
    nml <- glmtools::set_nml(nml, arg_list = nml_list)

    glmtools::write_nml(nml, nml_path)
    message("GLM: Created file ", file.path("GLM", "meteo_file.csv"))

  }

  ## GOTM
  if("GOTM" %in% model){

    got_file <- file.path(get_yaml_value(yaml, "config_files", "GOTM"))

    met_outfile <- "meteo_file.dat"

    met_outfpath <- file.path("GOTM", met_outfile)

    got_met <- format_met(met, model = "GOTM", config_file = config_file)

    # Avoid bug where GOTM can crash if last date of met file == last date of simulation
    if(got_met[nrow(got_met), 1] == get_yaml_value(yaml, "time", "stop")){
      last_line <- got_met[nrow(got_met),]
      new_last_date <- format(as.POSIXct(last_line[, 1]) + seconds(met_timestep),
                              "%Y-%m-%d %H:%M:%S")
      last_line[1, 1] <- new_last_date
      got_met <- rbind(got_met, last_line)

      warning("Last date of met file equals last date of simulation. This could cause GOTM to crash ",
              "and therefore one extra time step has been added to the GOTM met file.")
    }

    # Write meteo file, potentially with the scaling factors in the config_file
    # Using create_scaling_factors in the helpers.R script
    scale_param <- create_scaling_factors(config_file, "GOTM")
    scale_met(got_met, pars = scale_param, model = "GOTM", out_file = met_outfpath)

    # Format gotm.yaml file
    ## Set gotm.yaml met config - helper function
    set_met_config_yaml(met = met_outfpath, yaml_file = got_file)

    message("GOTM: Created file ", file.path("GOTM", met_outfile))

  }

  ## Simstrat
  if("Simstrat" %in% model){

    met_outfile <- "meteo_file.dat"
    par_file <- file.path(get_yaml_value(yaml, "config_files", "Simstrat"))

    met_outfpath <- file.path("Simstrat", met_outfile)

    sim_met <- format_met(met = met, model = "Simstrat", config_file = config_file)

    # Write meteo file, potentially with the scaling factors in the config_file
    # Using create_scaling_factors in the helpers.R script
    scale_param <- create_scaling_factors(config_file, "Simstrat")
    scale_met(sim_met, pars = scale_param, model = "Simstrat", out_file = met_outfpath)

    ### Write the name of the Simstrat meteo file in the par file
    input_json(file = par_file, label = "Input", key = "Forcing", "meteo_file.dat")

    message("Simstrat: Created file ", file.path("Simstrat", met_outfile))
  }

  ## MyLake
  if("MyLake" %in% model){

    met_outfile <- "meteo_file.dat"
    met_outfpath <- file.path("MyLake", met_outfile)

    # If met_timestep is not 24 hours, MyLake would crash
    # If met_timestep is lower than 24 hours, met is averaged to 24 hours
    if(met_timestep < 86400){
      warning("Meteo time step less than daily; averaging met file for MyLake simulation.")
      met_temp <- aggregate(met,
                            by = list(lubridate::floor_date(met$datetime, unit = "days")),
                            FUN = mean)
      met_temp$datetime <- NULL
      colnames(met_temp)[1] <- "datetime"
    }else if(met_timestep > 86400){
      stop("MyLake cannot be run with meteo forcing time steps larger than 1 day.")
    }else{
      met_temp <- met
    }

    mylake_met <- format_met(met = met_temp, model = "MyLake", config_file = config_file)

    # Write meteo file, potentially with the scaling factors in the config_file
    # Using create_scaling_factors in the helpers.R script
    scale_param <- create_scaling_factors(config_file, "MyLake")
    scale_met(mylake_met, pars = scale_param, model = "MyLake", out_file = met_outfpath)

    message("MyLake: Created file ", file.path("MyLake", met_outfile))
  }

  message("export_meteo complete!")

}
