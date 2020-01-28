#' Analyse the output netCDF
#'
#' Analyse the LER output neCDF and produce summary statistics
#'
#' @param ncdf filepath; to the `ensemble_output.nc` file
#' @param spin_up numeric; Number of days to disregard as spin-up for analysis.
#'
#' @return list with the dataframes used for comparison, statistics on model performanc, stratification
#' @examples
#' \dontrun{
#' }
#' @importFrom gotmtools list_vars get_vari wide2long sum_stat analyse_strat
#'
#' @export
analyse_ncdf <- function(ncdf, spin_up = NULL){

  vars <- list_vars(ncdf)
  vars # Print variables
  temp_vars <- grep('watertemp', vars, value = TRUE)
  ice_vars <- grep('ice_height', vars, value = TRUE)

  # Extract latitude for determining hemisphere
  lat <- get_1d(ncdf, 'lat')
  if(lat > 0){
    NH <- TRUE
  }else{
    NH <- FALSE
  }



  if(!'obs_watertemp' %in% temp_vars){
    stop('Obs water temp is not in ', ncdf)
  }

  obs <- get_vari(ncdf = ncdf, var = 'obs_watertemp', incl_time = T)
  z <- get_vari(ncdf = ncdf, var = 'z', incl_time = F)
  obs <- wide2long(obs, z)
  obs <- na.exclude(obs)
  if(!is.null(spin_up)){
    spin_date <- obs[1,1] + spin_up*(24*60*60)
    obs <- obs[obs[,1] >= spin_date,]
  }

  # Remove obs_watertemp
  temp_vars <- temp_vars[-grep('obs', temp_vars)]

  colnames(obs)[3] <- 'obs'
  if(length(ice_vars) > 0){
    obs_strat <- analyse_strat(data = obs, NH = NH, H_ice = rep(0, length(unique(obs[,1]))))
  }else{
    obs_strat <- analyse_strat(data = obs, NH = NH)
  }
  obs_strat$model <- 'obs'

  out_df <- NULL
  out_stats <- NULL
  out_strat <- NULL

  for(i in 1:(length(temp_vars))){

    # Extract the model for assigning
    model = strsplit(temp_vars[i], '_')[[1]][1]



    temp1 <- get_vari(ncdf = ncdf,
                              var = temp_vars[i])
    temp <- wide2long(temp1,z)
    if(sum(is.na(temp))/nrow(temp) > 0.75){
      warning('NAs > 75% of data in ', temp_vars[i],'! - Skipping analysis...')
      next
    }

    stats <- sum_stat(temp, obs, depth = T)
    df <- merge(obs, temp, by = c(1,2))
    df <- na.exclude(df)

    if(length(ice_vars) > 1){
      i_var <- grep(model, ice_vars, value = TRUE)
      if(length(i_var) == 0){
        i_var <- NULL
        warning('No ice output for ', model)
      }else{
        ice <- get_vari(ncdf = ncdf, var = i_var)
        ice <- ice[ice[,1] >= df[1,1],]
        if(sum(is.na(ice[,2])) == nrow(ice)){
          message(i_var, ' is all NA values - Setting to 0...')
          ice[,2] <- 0
        }
      }
    }

    if(!is.null(spin_up)){
      spin_date <- obs[1,1] + spin_up*(24*60*60)
      temp <- temp[temp[,1] >= spin_date,]
      ice <- ice[ice[,1] >= spin_date,]
    }

    if(!is.null(i_var)){
      strat <- analyse_strat(data = temp, H_ice = ice[,2], NH = NH)
    }else{
      strat <- analyse_strat(data = temp, NH = NH)
    }
    df$res <- df[,4] - df[,3]
    df$model <- model
    stats$model <- model
    strat$model <- model
    if(is.null(nrow(out_df))){
      out_df <- df
      out_stats <- stats
      out_strat <- strat
    }else{
      out_df <- rbind.data.frame(out_df, df)
      out_stats <- rbind.data.frame(out_stats, stats)
      out_strat <- rbind.data.frame(out_strat, strat)
    }
  }
  out_strat <- rbind.data.frame(obs_strat, out_strat)

  out <- list(out_df = out_df,
              stats = out_stats,
              strat = out_strat,
              obs = obs)
  return(out)
}
