#' Check master config file
#'
#' Check if the master config file is correct
#'
#' @param config_file filepath; to LakeEnsemblr yaml master config file
#' @param exp_cnf boolean; check if the control files for the models are there
#' @importFrom gotmtools get_yaml_value

check_master_config <- function(config_file, exp_cnf = FALSE) {
  
  
  # test if init depth is <= max depth
  if(gotmtools::get_yaml_value(config_file, "location", "depth") <
     gotmtools::get_yaml_value(config_file, "location", "init_depth")) {
    stop("init depth larger than max depth")
  }
  
  # test if strat and stop time are in the right format
  start <- gotmtools::get_yaml_value(config_file, "time", "start")
  stop <- gotmtools::get_yaml_value(config_file, "time", "stop")
  
  # check if strat is in ISO8601
  if(!grepl("\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}", start)){
    stop("Start time format must be in ISO8601")
  }
  # check if stop is in ISO8601
  if(!grepl("\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}", stop)) {
    stop("Start time format must be in ISO8601")
  }
  
  # test if start time is before stop time
  if(as.POSIXct(start) >
     as.POSIXct(stop)) {
    stop("Start time after stop time")
  }
  
  if(!exp_cnf) {
    # test if the control files are available
    if(!file.exists(gotmtools::get_yaml_value(config_file, "config_files", "GOTM"))) {
      stop(paste0("GOTM control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "GOTM"),
                  " is not existing"))
    }
    if(!file.exists(gotmtools::get_yaml_value(config_file, "config_files", "GLM"))) {
      stop(paste0("GLM control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "GLM"),
                  " is not existing"))
    }
    if(!file.exists(get_yaml_value(config_file, "config_files", "Simstrat"))) {
      stop(paste0("Simstrat control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "Simstrat"),
                  " is not existing"))
    }
    if(!file.exists(get_yaml_value(config_file, "config_files", "FLake"))) {
      stop(paste0("FLake control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "FLake"),
                  " is not existing"))
    }
    if(!file.exists(get_yaml_value(config_file, "config_files", "MyLake"))) {
      stop(paste0("MyLake control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "MyLake"),
                  " is not existing"))
    }
    }

  
  # check if time_unit in output is OK
  time_unit <- gotmtools::get_yaml_value(config_file, "output", "time_unit")
  good_units <- c("second", "minute", "hour", "day")
  if(!time_unit %in% good_units) {
      stop(paste0('Unknown output time unit: "', time_unit, '" in input control file ',
                  config_file, ". Allowed units: ", paste0(good_units, collapse = ", ")))
  }
  # check if format in output is OK
  format <- gotmtools::get_yaml_value(config_file, "output", "format")
  good_format <- c("netcdf", "text")
  if(!format %in% good_format) {
    stop(paste0('Unknown output format: "', format, '" in control file ', config_file,
                ". Allowed units: ", paste0(good_format, collapse = ", ")))
  } 
  
  
  # check if time_method in output is OK
  time_method <- gotmtools::get_yaml_value(config_file, "output", "time_method")
  good_method <- c("point", "mean", "integrated")
  if(!time_method %in% good_method) {
    stop(paste0('Unknown output time variable "', time_method, '" in control file ', config_file,
                ". Allowed units: ", paste0(good_umethod, collapse = ", ")))
  }
  
  # check if time_method in output is OK
  variables <- gotmtools::get_yaml_value(config_file, "output", "variables")
  good_vars <- c("temp", "ice_height")
  if(any(!variables %in% good_vars)) {
    stop(paste0('Unknown output variable: "', variables[!variables %in% good_vars],
                '" in control file ', config_file,
                ". Allowed units: ", paste0(good_vars, collapse = ", ")))
  } 
  
}


#' @keywords internal
check_models <- function(model){

  # test if there are any duplicates  
  if(any(duplicated(model))){
    warning('Model: "', model[duplicated(model)], '" is redundant in the input to argument "model"')
    # remove duplicates
    model <- model[!duplicated(model)]
  }
  
  # available models
  av_models <- c("Simstrat", "GOTM", "GLM", "FLake", "MyLake")
  
  # test if the supplied models are allowed
  if(any(!model %in% av_models)){
    # test if just the capitalization is wrong
    if(any(!tolower(model) %in% tolower(av_models))){
      stop(paste0('Unknown model: "', model[!model %in% av_models], '" in input argument "model"'))
    } else {
      # if capitalization is wrong correct it
      model[!model %in% av_models] <- av_models[!model %in% av_models]
    }
  }
  # return model vector
  return(model)
}