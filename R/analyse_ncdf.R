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
#' @importFrom gotmtools list_vars get_vari wide2long sum_stat
#'
#' @export
analyse_ncdf <- function(ncdf, spin_up = NULL){

  vars <- list_vars(ncdf)
  vars # Print variables

  if(!'obs_watertemp' %in% vars){
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

  colnames(obs)[3] <- 'obs'
  obs_strat <- analyse_strat(data = obs, NH = T)
  obs_strat$model <- 'obs'

  out_df <- NULL
  out_stats <- NULL
  out_strat <- NULL

  for(i in 1:(length(vars)-1)){
    model = strsplit(vars[i], '_')[[1]][1]
    l1 <- get_vari(ncdf = ncdf,
                              var = vars[i])
    l1 <- wide2long(l1,z)
    if(sum(is.na(l1))/nrow(l1) > 0.75){
      warning('NAs > 75% of data in ', vars[i],'! - Skipping analysis...')
      next
    }

    stats <- sum_stat(l1, obs, depth = T)
    df <- merge(obs, l1, by = c(1,2))
    df <- na.exclude(df)
    strat <- analyse_strat(df[,c(1,2,4)])
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
