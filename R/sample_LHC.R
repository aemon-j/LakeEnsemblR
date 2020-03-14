#' Create Latin hypercube sample of parameters
#'
#' Create CSV file with parameter samples for the LHC runs
#'
#' @param config_file filepath; to LakeEnsemblr yaml master config file
#' @param num integer; the number of random parameter sets to generate.
#' @param method character; Method for calibration. Can be 'met', 'model' or 'both'. Needs to be
#' specified by the user.
#' @param MCMC boolean; TRUE will run MCMC instead of LHC calibration, default is FALSE
#' @param mcmc_sample character; method for distribution of parameters used in MCMC
#'
#' @examples
#' \dontrun{
#'pars <- c('wind_factor', 'swr_factor', 'lw_factor')
#'mat <- matrix(data = c(0.5,2,0.5,1.5,0.5,1.5), nrow = 3, byrow = T)
#'df <- as.data.frame(mat)
#'rownames(df) <- pars
#'param_file <- sample_LHC(par_range = df, num = 300)
#'run_Latin_hypercube(param_file = param_file,
#'obs_file = 'LakeEnsemblR_wtemp_profile_standard.csv',
#'config_file = 'Feeagh_master_config.yaml', model = 'FLake',
#'meteo_file = 'LakeEnsemblR_meteo_standard.csv')
#' }
#' @importFrom FME Latinhyper
#' @importFrom gotmtools get_yaml_value
#' @importFrom configr read.config
#'
#' @export
sample_LHC <- function(config_file, num, method = NULL, folder = ".", file.name = NULL,
                       MCMC = FALSE, mcmc_sample = "uniform"){

  configr_master_config <- configr::read.config(file.path(folder, config_file))
  
  if(method == "met"){
    cal_section <- configr_master_config[["calibration"]][["met"]]
  }else if(method == "model"){
    stop("Method == model currently not supported")
  }else if(method == "both"){
    stop("Method == both currently not supported")
  }else{
    stop("Select method either 'met', 'model' or 'both'.")
  }
  
  par_names <- names(cal_section)
  lb <- unlist(lapply(cal_section, `[`, "lower"), use.names = F)
  ub <- unlist(lapply(cal_section, `[`, "upper"), use.names = F)
  
  
  if(method == "met"){
    ind <- which(par_names %in% LakeEnsemblR::met_var_dic$short_name)
  }else if(method == "model"){
    stop("Currently not supported")
  }else if(method == "both"){
    stop("Currently not supported")
  }else{
    stop("Select method either 'met', 'model' or 'both'.")
  }

  par_range <- as.matrix(data.frame(lb = lb[ind], ub = ub[ind], row.names = par_names[ind]))
  print("Parameters used:")
  print(par_range)

  if(isFALSE(MCMC)){
    params <- Latinhyper(parRange = as.matrix(par_range), num = num)
    params <- signif(params, 4)
    colnames(params) <- par_names[ind]
    params <- as.data.frame(params)
    params$par_id <- paste0("p", formatC(seq_len(nrow(params)), width = 4, format = "d",
                                         flag = "0"))
    if(is.null(file.name)){
      return_name <- paste0("LHS_params_", format(Sys.time(), format = "%Y%m%d%H%M"), ".csv")
      return_name <- file.path(folder, return_name)
      write.csv(params, file <- return_name, quote = FALSE, row.names = FALSE)
    }else{
      return_name <- file.path(folder, paste0(file.name, ".csv"))
      write.csv(params, file = return_name, quote = FALSE, row.names = FALSE)
    }
    return(paste0(return_name))
  }else{
    return_name <- data.frame(par_range)
    if (mcmc_sample == "uniform"){
      return_name$method <- rep("uniform", nrow(return_name))
      return(return_name)
    }
  }
}
