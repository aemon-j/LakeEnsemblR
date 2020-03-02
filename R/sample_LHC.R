#' Create Latin hypercube sample of parameters
#'
#' Create CSV file with parameter samples for the LHC runs
#'
#' @param config_file filepath; to LakeEnsemblr yaml master config file
#' @param num integer; the number of random parameter sets to generate.
#' @param method character; Method for calibration. Can be 'met', 'model' or 'both'. Needs to be
#' specified by the user.
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
#'
#' @export
sample_LHC <- function(config_file, num, method = NULL, folder = ".", file.name = NULL){

  # Load dictionary
  var_names_dic <- load_dic()


  par_names <- get_yaml_value(file = config_file, label = "calibration", key = "parameter")
  lb <- get_yaml_value(file = config_file, label = "calibration", key = "lower")
  ub <- get_yaml_value(file = config_file, label = "calibration", key = "upper")


  if(method == "met"){
    ind <- which(par_names %in% var_names_dic$Variable)
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


  params <- Latinhyper(parRange = as.matrix(par_range), num = num)
  params <- signif(params, 4)
  colnames(params) <- par_names[ind]
  params <- as.data.frame(params)
  params$par_id <- paste0("p", formatC(seq_len(params), width = 4, format = "d", flag = "0"))
  if (is.null(file.name)){
    return.name <- paste0("LHS_params_", format(Sys.time(), format = "%Y%m%d%H%M"), ".csv")
    return.name <- file.path(folder, return.name)
    write.csv(params, file <- return.name, quote = FALSE, row.names = FALSE)

  } else {
    return.name <- file.path(folder, paste0(file.name, ".csv"))
    write.csv(params, file = return.name, quote = FALSE, row.names = FALSE)
    }

  return(paste0(return.name))
}
