#' Analyse the output netCDF
#'
#' Analyse the LER output neCDF and produce summary statistics
#'
#' @param ncdf filepath; to the `ensemble_output.nc` file
#' @param model Vector of models for which to calculate the performance measures
#' @param dim character; NetCDF dimensions to extract. Must be either "member" or "model". Defaults to "model". Only used if plotting from netCDF file. Currently only works with "model".
#' @param dim_index numeric; Index of dimension chosen to extract from. Defaults to 1. Only used if plotting from netCDF file.
#' @param spin_up numeric; Number of days to disregard as spin-up for analysis.
#' @param drho numeric; density difference between top and bottom indicating stratification
#' [kg m^-3]
#'
#' @return list of four dataframes: 'out_df' = long data frame with date, depths, temp and model,
#' 'stats' = summary statistics of model fitness compared to observed and mean differences between modelled and observed phenological events,
#' 'strat' = stratification statistics for each year,
#' 'obs_temp' = long dataframe of observed data within the netCDF file
#' @author Tadhg Moore
#' @examples
#' \dontrun{
#' }
#' @importFrom gotmtools list_vars get_vari wide2long sum_stat
#'
#' @export
analyse_ncdf <- function(ncdf, model, dim = "model", dim_index = 1, spin_up = 0, drho = 0.1){
  
  # check if model input is correct
  model <- check_models(model)
  # check if netCDF exists
  if(!file.exists(ncdf)){
    stop("File: '", ncdf, "' does not exist!\nPlease check the file path.")
  }

  vars <- gotmtools::list_vars(ncdf)
  if(!("watertemp" %in% vars)){
    stop(paste("Variable 'watertemp', is not present in", ncdf,
               "\nAdd 'watertemp' to variables list in the yaml file and re-run 'run_ensemble()'"))
  }
  if(("ice_height" %in% vars)){
    ice_present <- TRUE
  }
  
  temp <- load_var(ncdf, "watertemp", return = "list", dim = dim,
                   dim_index = dim_index, print = FALSE)
  temp_name <- names(temp)
  temp_name <- temp_name[which(temp_name != "Obs")]
  if(sum(!(model %in% temp_name)) != 0){
    stop("Model(s): ", model[!(model %in% temp_name)], " is/are not present in '", ncdf, "'\nPlease select a model from: ", paste(temp_name, collapse = ", "))
  }
  
  temp <- temp[(which(names(temp) %in% c(model, "Obs")))]
  if(ice_present){
    ice <- load_var(ncdf, var = "ice_height", return = "list", print = FALSE)
    ice <- ice[(which(names(ice) %in% c(model, "Obs")))]
  }
  
  
  # Extract latitude for determining hemisphere
  lat <- gotmtools::get_1d(ncdf, "lat")
  if(lat > 0){
    NH <- TRUE
  }else{
    NH <- FALSE
  }

  # Check temperature observations and stop if none present
  tst <- apply(temp[["Obs"]], 2, function(x)sum(is.na(x)))
  tst2 <- sum(tst  == nrow(temp[["Obs"]]))
  if(tst2 == ncol(temp[["Obs"]])){
    stop("There are no temperature observations in ", ncdf)
  }
  
  # Check ice observations and stop if none present
  if(ice_present){
    tst <- apply(ice[["Obs"]], 2, function(x)sum(is.na(x)))
    tst2 <- sum(tst  == nrow(ice[["Obs"]]))
    if(tst2 == ncol(temp[["Obs"]])){
      stop("There are no ice_height observations in ", ncdf)
    }
  }
  


  # Remove temp spin-up period ----
  obs_temp <- temp[["Obs"]]
  z <- rLakeAnalyzer::get.offsets(obs_temp)
  obs_temp <- gotmtools::wide2long(obs_temp, z)
  # obs_temp <- na.exclude(obs_temp)
  if(!is.null(spin_up)){
    spin_date <- obs_temp[1, 1] + spin_up * (24 * 60 * 60)
    obs_temp <- obs_temp[obs_temp[, 1] >= spin_date, ]
  }
  
  if(ice_present){
    # Remove ice spin-up period ----
  obs_ice <- ice[["Obs"]]
  if(!is.null(spin_up)){
    spin_date <- obs_ice[1, 1] + spin_up * (24 * 60 * 60)
    obs_ice <- obs_ice[obs_ice[, 1] >= spin_date, ]
  }
  ice[["Obs"]] <- NULL
  }
  

  # Remove obs_watertemp
  temp[["Obs"]] <- NULL
  
  
  # colnames(obs_temp)[3] <- "obs"
  # colnames(obs_ice)[2] <- "obs"
  
  if(ice_present){
    obs_strat <- analyse_strat(data = obs_temp, NH = NH, H_ice = obs_ice[, 2], drho = drho)
    obs_strat$model <- "obs"
  }else{
    obs_strat <- analyse_strat(data = obs_temp, NH = NH, H_ice = NULL, drho = drho)
    obs_strat$model <- "obs"
  }
  
  
  # Loop through each model output
  out_list <- lapply(seq_len(length(temp)), function(x){
    
    z <- rLakeAnalyzer::get.offsets(temp[[x]])
    tmp <- gotmtools::wide2long(temp[[x]], z)
    # obs_temp <- na.exclude(obs_temp)
    if(!is.null(spin_up)){
      spin_date <- tmp[1, 1] + spin_up * (24 * 60 * 60)
      tmp <- tmp[tmp[, 1] >= spin_date, ]
    }
    
    if(ice_present){
      ic <- ice[[x]]
    if(!is.null(spin_up)){
      spin_date <- ic[1, 1] + spin_up * (24 * 60 * 60)
      ic <- ic[ic[, 1] >= spin_date, ]
      }
    }
    
    if(ice_present){
      str <- analyse_strat(data = tmp, NH = NH, H_ice = ic[, 2], drho = drho)
    }else{
      str <- analyse_strat(data = tmp, NH = NH, H_ice = NULL, drho = drho)
    }
    
    stats <- gotmtools::sum_stat(tmp, obs_temp, depth = T)
    
    tmp$model <- names(temp)[x]
    str$model <- names(temp)[x]
    stats$model <- names(temp)[x]
    
    
    return(list(temp = tmp, strat = str, fit = stats))
    
  })
  names(out_list) <- names(temp)
  
  
  out_df <- NULL
  out_strat <- NULL
  out_stat <- NULL
  
  if(length(out_list) == 1){
    out_df <- out_list[[1]][[1]]
    out_strat <- out_list[[1]][[2]]
    out_stat <- out_list[[1]][[3]]
  }else{
    for(i in seq_len(length(out_list))){
      
      out_df <- rbind(out_df, out_list[[i]][[1]])
      out_strat <- rbind(out_strat, out_list[[i]][[2]])
      out_stat <- rbind(out_stat, out_list[[i]][[3]])
      
    }
  }
  
  out_df <- na.exclude(out_df)
  out_strat <- rbind.data.frame(obs_strat, out_strat)
  obs_temp <- na.exclude(obs_temp)
  out_df$model <- factor(out_df$model)
  
  # Put the model in the first column
  out_stat <- out_stat[,c(ncol(out_stat), 1:(ncol(out_stat)-1))]
  out_strat <- out_strat[,c(ncol(out_strat), 1:(ncol(out_strat)-1))]
  
  
  out <- list(out_df = out_df,
              stats = out_stat,
              strat = out_strat,
              obs_temp = obs_temp)
  return(out)

}
