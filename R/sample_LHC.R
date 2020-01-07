#' Create Latin hypercube sample of parameters
#'
#' Create CSV file with parameter samples for the LHC runs
#'
#' @param parRange dataframe; the range (min, max) of the parameters, a data.frame with one row for each parameter, and two columns with the minimum (1st) and maximum (2nd) column.
#' @param num integer; the number of random parameter sets to generate.
#'
#' @examples
#' \dontrun{
#'pars <- c('wind_factor', 'swr_factor', 'lw_factor')
#'mat <- matrix(data = c(0.5,2,0.5,1.5,0.5,1.5), nrow = 3, byrow = T)
#'df <- as.data.frame(mat)
#'rownames(df) <- pars
#'param_file <- sample_LHC(parRange = df, num = 300)
#'run_Latin_hypercube(param_file = param_file, obs_file = 'LakeEnsemblR_wtemp_profile_standard.csv', config_file = 'Feeagh_master_config.yaml', model = 'FLake', meteo_file = 'LakeEnsemblR_meteo_standard.csv')
#' }
#' @importFrom FME Latinhyper
#'
#' @export
sample_LHC <- function(parRange, num, folder = '.'){
  par_names <- row.names(parRange)
  params <- Latinhyper(parRange = as.matrix(parRange), num = num)
  params <- signif(params, 4)
  colnames(params) <- par_names
  params <- as.data.frame(params)
  params$par_id <- paste0('p', formatC(1:nrow(params), width = 4, format = "d", flag = "0"))
  write.csv(params, file = file.path(folder, paste0('latin_hypercube_params', '_', format(Sys.time(), format = '%Y%m%d%H%M'), '.csv')), quote = FALSE, row.names = FALSE)
  return(paste0('latin_hypercube_params', '_', format(Sys.time(), format = '%Y%m%d%H%M'), '.csv'))
}
