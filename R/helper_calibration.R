#' wrapper function for LHC calibration
#'  
#' A wrapper function for the latin hypercube calibration, used in cali_ensemble when
#' cmethod == "LHC".
#'  
#' @param pars data.frame with all parameter sets for the latinhypercube sampling, each row is
#'    a set of parameters for which the model is to be run. colnames must be the parameter names
#' @param type character vector specifying the type of parameter. can be either `"met"` for meteo
#'    scaling or "model" for model specific parameters, must have one value per column of `pars`
#' @param model the model name for which to run the function
#' @param var variable for which to calculate the model performance (usually this is `"temp"`)
#' @param config_file path to the master config file
#' @param met data.frame with the model specific meteo data, which is then feed to scale_meteo
#' @param folder root folder
#' @param out_f folder in the root folder where the output of the LHC should be written to
#' @param outf_n name of the output file in which the model performance for every parameter
#'    set is written
#' @param obs_deps depths of observations
#' @param obs_out data.frame with the observations of `var`
#' @param out_hour FLake specific: hout of output
#' @param qualfun function that takes data.frames of observations and simulations and returns
#'    model performance metrics
#' @param nout_fun number of values returned by `qualfun`.
#' 
#' @keywords internal

LHC_model <- function(pars, type, model, var, config_file, met, folder, out_f, outf_n,
                      obs_deps, obs_out, out_hour, qualfun, config_f, nout_fun) {
  
  message(paste0("\nStarted LHC for model: ", model, "\n"))
  # name of the output file to be written
  out_name <- paste0(model, "_", outf_n, ".csv")
  # create the output folder, if not existing
  dir.create(file.path(folder, out_f), showWarnings = FALSE)
  # loop over all parameter sets
  for (p in seq_len(nrow(pars))) {
    # change the paremeter/meteo scaling
    change_pars(config_file = config_file, model = model,
                pars = pars[p, -ncol(pars), drop = FALSE],
                type = type, met = met, folder = folder)
    # calculate quality measure
    qual_i <- cost_model(config_file = config_file, model = model, var = var, folder = folder,
                         obs_deps = obs_deps, obs_out = obs_out, out_hour = out_hour,
                         qualfun = qualfun, config_f = config_f)
    if(is.na(qual_i[1])) {
      qual_i <- rep(NA, nout_fun)
      out_i <- t(c(par_set = pars[p, ncol(pars)], qual_i))
    } else {
      # paste parameter values and quality measure
      out_i <- c(par_id = pars[p, ncol(pars)], qual_i)
    }
    # switch if file is existing
    flsw <- file.exists(file.path(folder, out_f, out_name))
    write.table(x = out_i, file = file.path(folder, out_f, out_name),
                append = ifelse(flsw, TRUE, FALSE), sep = ",", row.names = FALSE,
                col.names = ifelse(flsw, FALSE, TRUE), quote = FALSE)
    
  }
  
  message(paste0("\nFinished LHC for model: ", model, "\n"))
  return(data.frame(results = file.path(folder, out_f, out_name),
                    parameters = file.path(folder, out_f,
                                           paste0("params_", model, "_", outf_n, ".csv")),
                    stringsAsFactors = FALSE))
}

#' warpper function for other two methods (modMCMC and modFit)
#'  
#' A wrapper function for the modMCMC and modFit calibration methods, used in cali_ensemble when
#' cmethod == "modMCMC" or "modFit".
#'  
#' @param pars named vector with a set of parameter for wich the model is to be run and a
#'    performance metric is to be returned
#' @param type character vector specifying the type of parameter. can be either `"met"` for meteo
#'    scaling or "model" for model specific parameters, must have the same length as `pars`.
#' @param model the model name for which to run the function
#' @param var variable for which to calculate the model performance (usually this is `"temp"`)
#' @param config_file path to the master config file
#' @param met data.frame with the model specific meteo data, which is then feed to scale_meteo
#' @param folder root folder
#' @param out_f folder in the root folder where the output of the LHC should be written to
#' @param outf_n name of the output file in which the model performance for every parameter
#'    set is written
#' @param obs_deps depths of observations
#' @param obs_out data.frame with the observations of `var`.
#' @param out_hour FLake specific: hout of output
#' @param qualfun function that takes data.frames of observations and simulations and returns
#'    model performance metrics
#' @param nout_fun number of values returned by `qualfun`.
#' @param write write the results to a file?
#' 
#' @keywords internal

wrap_model <- function(pars, type, model, var, config_file, met, folder, out_f,
                       obs_deps, obs_out, out_hour, qualfun, config_f, outf_n,
                       write = TRUE) {
  par_name <- names(pars)
  # name of the output file to be written
  out_name <- paste0(model, "_", outf_n, ".csv")
  # name of the parameter
  pars <- data.frame(matrix(pars, nrow = 1))
  colnames(pars) <- par_name
  
  # change the paremeter/meteo scaling
  change_pars(config_file = config_file, model = model, pars = pars,
              type = type, met = met, folder = folder)
  # calculate quality measure
  qual <- cost_model(config_file = config_file, model = model, var = var, folder = folder,
                     obs_deps = obs_deps, obs_out = obs_out, out_hour = out_hour,
                     qualfun = qualfun, config_f = config_f)
  if(is.na(qual)) {
    qual <- NA
    out_w <- t(c(pars, qual))
  } else {
    # paste parameter values and quality measure
    out_w <- data.frame(pars, qual = qual)
  }
  if(write) {
    # switch if file is existing
    flsw <- file.exists(file.path(folder, out_f, out_name))
    write.table(x = out_w, file = file.path(folder, out_f, out_name),
                append = ifelse(flsw, TRUE, FALSE), sep = ",", row.names = FALSE,
                col.names = ifelse(flsw, FALSE, TRUE), quote = FALSE)
  }
  # return
  return(qual)
}

##---------------------------------- utility functions ---------------------------------------------

#' Change parameter or meteo scaling for a model
#' 
#' Input a specific parameter or scale the meteorological forcing for a selected model
#' 
#' @param config_file path to master config file
#' @param model name of the model
#' @param pars named vector of parameters to change
#' @param type character vector specifying the type of parameter. can be either `"met"` for meteo
#'     scaling or "model" for model specific parameters, must have the same length as `pars`
#' @param met data.frame with the model specific meteo data, which is then feed to scale_meteo
#' @param folder root folder
#' @keywords internal

change_pars <- function(config_file, model, pars, type, met, folder) {
  
  if(length(pars) != length(type)) {
    stop(paste0("pars and type vectors need to have the same length"))
  }
  
  # get name of model config file
  config_f <- gotmtools::get_yaml_value(config_file, "config_files", model)
  # names of the parameters
  par_names <- names(pars)  
  # meteo pars  
  met_pars <- pars[type == "met"]
  # model specific pars
  model_pars <- pars[type == "model"]
  
  if (length(met_pars) > 0){
    met_name <- get_model_met_name(model, config_f)
    met_pars <- setNames(data.frame(met_pars), names(met_pars))
    # scale meteo
    scale_met(met = met, pars = met_pars, model = model,
              out_file = file.path(folder, model, met_name))
  }
  
  if (length(model_pars) > 0){
    
    for (i in seq_len(length(model_pars))) {
      # get right names for parameter
      spl <- strsplit(names(model_pars)[i], "/")
      if(length(spl[[1]]) == 1){
        label <- NULL
        key <- spl[[1]]
      }else{
        label <- spl[[1]][1]
        key <- spl[[1]][2]
      }
      # change model specific parameters
      suppressMessages(input_config_value(model = model, file = config_f, label = label,
                                          key = key,
                                          value = model_pars[i]))
    }
  }
  
} 

#' get the name of model meteo file
#' 
#' get the name of the meteo file from the model config file
#' 
#' @param model Model name
#' @param config_f path to model specific controll file
#' @keywords internal

get_model_met_name <- function(model, config_f){
  if(model == "MyLake") {
    met_name <- "meteo_file.dat"
  } else {
    # get right lable and key for meteo file
    label <- dplyr::case_when(model == "GLM" ~ "meteorology",
                              model == "GOTM" ~ "meteo",
                              model == "FLake" ~ "METEO",
                              model == "Simstrat" ~ "Input")
    key <- dplyr::case_when(model == "GLM" ~ "meteo_fl",
                            model == "GOTM" ~ "file",
                            model == "FLake" ~ "meteofile",
                            model == "Simstrat" ~ "Forcing")
    # get name of meteo file
    met_name <- get_config_value(model, config_f, label, key)
  }
  if (model == "FLake") {
    met_name <- gsub(",", "", met_name)
  }
  return(met_name)
}
#' Run a model and calculate model cost 
#' 
#' Runns the selected model and calculates fit metrics using a provided funtion
#' 
#' @param config_file path to master config file
#' @param model name of the model
#' @param var name of variable for which to calculate model performance
#' @param folder root folder 
#' @param obs_deps depths of observations
#' @param obs_out data.frame with the observations of `var`
#' @param out_hour FLake specific: hout of output
#' @param qualfun function that takes data.frames of observations and simulations and returns
#'    model performance metrics
#' @param config_f path to model specific control file (e.g. "GLM/glm3.nml")
#' @keywords internal

cost_model <- function(config_file, model, var, folder, obs_deps, obs_out, out_hour, qualfun,
                       config_f) {
  # list with the function arguments for run_model()
  if(model == "FLake") {
    mod_arg <- list(sim_folder = file.path(folder, model),
                    nml_file =basename(config_f),
                    verbose = FALSE)
  } else if (model == "Simstrat") {
    mod_arg <- list(sim_folder = file.path(folder, model),
                    par_file = basename(config_f),
                    verbose = FALSE)
  } else if(model == "MyLake") {
    mod_arg <- list(sim_folder = file.path(folder),
                    config_dat = basename(config_f))
  } else {
    mod_arg <- list(sim_folder = file.path(folder, model),
                    verbose = FALSE)
  }
  # did de model runn successfully?
  ran <- FALSE
  # try to run the model
  tryCatch({
    invisible(do.call(paste0("run_", tolower(model)), mod_arg))
    ran <- TRUE
  }, error = function(e){})
  # if retrieving model output fails return NA
  quali <- NA
  # try to get output and calculate model perfomance
  if(ran) {
    tryCatch({
      out <- get_output(config_file = config_file, model = model, var = var,
                        folder = folder, obs_depths = obs_deps, out_time = obs_out,
                        out_hour = out_hour)
      out <- as.data.frame(out)
      colnames(out) <- gsub(paste0(var, "."), "", colnames(out))
      # match depths
      depths <- as.numeric(gsub(".*wtr_", "", colnames(out)[-1]))
      id <- which(depths %in% obs_deps) + 1
      id_obs <- which(obs_deps %in% depths) + 1
      # match times
      out <- out[out$datetime %in% obs_out$datetime, c(1, id)]
      # calculate quality function
      quali <- qualfun(O = obs_out[obs_out$datetime %in% out$datetime, c(1, id_obs)],
                       P = out)
      
    }, error = function(e){})
  }
  
  return(quali)
  
}


#' calculate model performance metrics
#'
#' function that calculates different estimations for model accuracy, namely: root mean squared
#' error (rmse), (Nash-Sutcliff) model efficiency (nse), Pearson corelation coefficient (r),
#' relative error (re), mean absolute error (mae), and normalized mean absolute error (nmae).
#' returns  a data.frame containing the six quality estimates
#' 
#' @param O data.frame containing observed values, first row is datetime
#' @param P: data.frame containing predicted values, first row is datetime
#' @keywords internal

qual_fun <- function(O, P){
  
  # remove datetime column
  O <- as.matrix(O[, -1])
  P <- as.matrix(P[, -1])
  
  # rmse
  rmse <- sqrt(mean((O - P)^2, na.rm = TRUE))
  
  
  # nash sutcliff
  nse <- 1 - sum((O - P)^2, na.rm = TRUE)/sum((O - mean(O, na.rm=TRUE))^2, na.rm = TRUE)
  
  # pearson corelation coef
  r <- sum((O - mean(O, na.rm = TRUE))*(P - mean(P, na.rm = TRUE)),
           na.rm = TRUE)/sqrt(sum((O - mean(O, na.rm = TRUE))^2, na.rm = TRUE)*
                                sum((P - mean(P, na.rm = TRUE))^2, na.rm = TRUE))
  
  # relative error
  re <- mean((P - O)/O, na.rm = TRUE)
  
  # mean absolute error
  mae <- mean(abs(O - P), na.rm = TRUE)
  
  # normalised mean absolute error
  nmae <- mean(abs((O - P)/O), na.rm = TRUE)
  
  qual <- data.frame(rmse = rmse, nse = nse, r = r, re = re, mae = mae, nmae = nmae)
  
  return(qual)
}


#' Creates a script to then run as a job
#'
#' @param call an object of class `call``
#' @param name name of the job
#' @keywords internal
make_script <- function(call, name) {
  script <- tempfile()
  
  call$job_name <- NULL
  wd <- getwd()

  lines <-
    writeLines(paste0(
      "library(LakeEnsemblR)\nsetwd('",wd,"')\n",
      name,
      " <- ",
      paste0(deparse(call), collapse = "")
    ), script)
  return(script)
}
