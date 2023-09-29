#' Check master config file
#'
#' Check if the master config file is correct
#'
#' @param config_file filepath; to LakeEnsemblr yaml master config file
#' @param model vector; model to export driving data. Options include c("GOTM", "GLM", "Simstrat",
#' "FLake", "MyLake")
#' @param exp_cnf boolean; check if the control files for the models are there
#' @importFrom gotmtools get_yaml_value
#' @export

check_master_config <- function(config_file,
                                model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                                exp_cnf = FALSE) {


  # test if init depth is <= max depth
  if(gotmtools::get_yaml_value(config_file, "location", "depth") <
     gotmtools::get_yaml_value(config_file, "location", "init_depth")) {
    stop("init depth larger than max depth")
  }

  # test if strat and stop time are in the right format
  start <- gotmtools::get_yaml_value(config_file, "time", "start")
  stop <- gotmtools::get_yaml_value(config_file, "time", "stop")

  # check if strat is in right format
  if(!grepl("\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}", start)){
    stop("Start time format must be in yyyy-mm-dd hh:mm:ss format")
  }
  # check if stop is in right format
  if(!grepl("\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}:\\d{2}", stop)) {
    stop("Start time format must be in yyyy-mm-dd hh:mm:ss format")
  }

  # test if start time is before stop time
  if(as.POSIXct(start) >
     as.POSIXct(stop)) {
    stop("Start time after stop time")
  }

  if(!exp_cnf) {
    # test if the control files are available
    if(!file.exists(gotmtools::get_yaml_value(config_file, "config_files", "GOTM")) &&
       "GOTM" %in% model) {
      stop(paste0("GOTM control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "GOTM"),
                  " is not existing"))
    }
    if(!file.exists(gotmtools::get_yaml_value(config_file, "config_files", "GLM")) &&
       "GLM" %in% model) {
      stop(paste0("GLM control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "GLM"),
                  " is not existing"))
    }
    if(!file.exists(get_yaml_value(config_file, "config_files", "Simstrat")) &&
       "Simstrat" %in% model) {
      stop(paste0("Simstrat control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "Simstrat"),
                  " is not existing"))
    }
    if(!file.exists(get_yaml_value(config_file, "config_files", "FLake")) &&
       "FLake" %in% model) {
      stop(paste0("FLake control file ",
                  gotmtools::get_yaml_value(config_file, "config_files", "FLake"),
                  " is not existing"))
    }
    if(!file.exists(get_yaml_value(config_file, "config_files", "MyLake")) &&
       "MyLake" %in% model) {
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
  
  # check if the same number of in-/outflow scaling factors are given
  #   as the number of in-/outflows
  configr_master_config <- configr::read.config(file.path(config_file))
  use_inflows <- get_yaml_value(config_file, label = "inflows", key = "use")
  tryCatch({get_yaml_value(config_file, "inflows", "mass-balance")
    warning(paste0("The 'mass-balance' argument is no longer used after ",
                   "version 1.1. If you would like to have outflows ",
                   "matching the inflows, please add the inflow file ",
                   "manually to the 'outflows' section. You can use the same ",
                   "or a different file as for inflows."))},
    error = function(e) { })
  use_outflows <- get_yaml_value(config_file, label = "outflows", key = "use")
  
  if(!is.null(configr_master_config[["scaling_factors"]][["all"]]
              [["inflow"]]) & use_inflows){
    infl_scalings <- (configr_master_config[["scaling_factors"]][["all"]]
      [["inflow"]])
    num_inflows <- get_yaml_multiple(config_file, key1 = "inflows",
                                     key2 = "number_inflows")
    if(length(infl_scalings) != num_inflows){
      warning(paste0("There is a different number of inflows than there are ",
                     "inflow scaling factors in the control file ",
                     config_file, ". Provide the same number of ",
                     "scaling factors and inflows."))
    }
  }
  if(!is.null(configr_master_config[["scaling_factors"]][["all"]]
              [["outflow"]]) & use_outflows){
    outfl_scalings <- (configr_master_config[["scaling_factors"]][["all"]]
                       [["outflow"]])
    num_outflows <- get_yaml_multiple(config_file, key1 = "outflows",
                                     key2 = "number_outflows")
    if(length(outfl_scalings) != num_outflows){
      warning(paste0("There is a different number of outflows than there are ",
                     "outflow scaling factors in the control file ",
                     config_file, ". Provide the same number of ",
                     "scaling factors and outflows."))
    }
  }

  # check if variables in output are OK
  variables <- gotmtools::get_yaml_value(config_file, "output", "variables")
  good_vars <- c("temp", "ice_height", "dens", "salt", "w_level", "q_sens",
                 "q_lat")
  if(any(!variables %in% good_vars)) {
    stop(paste0('Unknown output variable: "', variables[!variables %in% good_vars],
                '" in control file ', config_file,
                ". Allowed units: ", paste0(good_vars, collapse = ", "),
                "\n"))
  }

  # Check if lower limits for calibration are smaller than upper limits
  # load master config file
  if ("calibration" %in% names(configr_master_config)) {
    
    # Kw parameter
    cal_section <- configr_master_config[["calibration"]][["Kw"]]
    if(!is.null(cal_section)) {
      p_lower_kw <- cal_section$lower
      p_upper_kw <- cal_section$upper
      if (p_lower_kw > p_upper_kw) {
        stop(paste0("Lower boundary for calibration of Kw",
                    " is larger than upper value!"))
      }
    }
    # meteo parameter
    cal_section <- configr_master_config[["calibration"]][["met"]]
    if(!is.null(cal_section)) {
      params_met <- sapply(names(cal_section), function(n)cal_section[[n]]$initial)
      p_lower_met <- sapply(names(cal_section), function(n)cal_section[[n]]$lower)
      p_upper_met <- sapply(names(cal_section), function(n)cal_section[[n]]$upper)
  
      # test if any of the lower limits are larger than the upper limits
      if (any(p_lower_met > p_upper_met)) {
        stop(paste0("Lower boundary for calibration of meteo variable ", names(params_met)[p_lower_met > p_upper_met],
                    " is larger than upper value!"))
      }
    }

    # get names of models for which parameter are given
    model_p <- model[model %in% names(configr_master_config[["calibration"]])]
    # model specific parameters
    cal_section <- lapply(model_p, function(m)configr_master_config[["calibration"]][[m]])
    names(cal_section) <- model_p
    # get parameters
    params_mod <- lapply(model_p, function(m) {
      sapply(names(cal_section[[m]]),
             function(n) as.numeric(cal_section[[m]][[n]]$initial))})
    names(params_mod) <- model_p
    # get lower bound
    p_lower_mod <- lapply(model_p, function(m) {
      sapply(names(cal_section[[m]]),
             function(n) as.numeric(cal_section[[m]][[n]]$lower))})
    names(p_lower_mod) <- model_p
    # get upper bound
    p_upper_mod <- lapply(model_p, function(m) {
      sapply(names(cal_section[[m]]),
             function(n) as.numeric(cal_section[[m]][[n]]$upper))})
    names(p_upper_mod) <- model_p
    for (m in model) {

      if(any(p_lower_mod[[m]] > p_upper_mod[[m]])) {
        stop(paste0("Lower bound for calibration of parameter ",
                    names(p_lower_mod[[m]])[p_lower_mod[[m]] > p_upper_mod[[m]]],
                    " for model ", m, " is larger than upper bound!"))
      }

    }
}
  # Check if there are tabs in the config file, which will cause
  # internal errors in some cases when relying on configr:read.config
  config_char <- readChar(config_file, nchars = file.info(config_file)$size)
  if(grepl("\t", config_char)){
    warning("Tabs detected in ", config_file, ". This could lead to errors,",
            " replace with spaces.")
  }

  # issue a warning if dens is used together with models that don't directly give dens output
  if("dens" %in% variables & any(c("MyLake", "FLake") %in% model)) {
    warning(paste0("Models ", paste0(model[model %in% c("MyLake", "FLake")], collapse = " and "),
                   " do not directly output density and the results in the output ncdf file ",
                   "are calculated from the models temperature output."))
  }

  # issue a warning if sens is used together with models that don't give sens output
  if("salt" %in% variables & any(c("MyLake", "FLake") %in% model)) {
    warning(paste0("Models ", paste0(model[model %in% c("MyLake", "FLake")], collapse = " and "),
                   " do not output salinity and the results in the output ncdf file ",
                   "are just NAs."))
  }
}


#' @keywords internal
check_models <- function(model, check_package_install = FALSE){

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

  # test if the required packages are installed
  if(check_package_install){
    model_packages <- list("FLake" = "FLakeR",
                           "GLM" = "GLM3r",
                           "GOTM" = "GOTMr",
                           "Simstrat" = "SimstratR",
                           "MyLake" = "MyLakeR")

    check_package_installation <- function(model){
      if(isFALSE(requireNamespace(model_packages[[model]], quietly = TRUE))){
        stop("You can't include ", model, " in this function call without having the ",
             "package ", model_packages[[model]], " installed!")
      }
    }

    sapply(model, check_package_installation)
  }

  # return model vector
  return(model)
}
