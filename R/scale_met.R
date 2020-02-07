#' Scale meteorological data
#'@description
#'Scale meteorological parameters.
#'
#' @name scale_met
#' @param met dataframe; in the format for that particular model with the column headers
#' @param pars dataframe; scaling factors for meteorological data with column names corresponding to variable dictionary
#' @param model character; Model for which scaling parameters will be applied. Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#' @param out_file filepath; For scaled meteorlogical data
#' @export
scale_met <- function(met, pars, model, out_file = NULL){

  if('FLake' %in% model){
    par_nams <- names(pars)

    if('wind_speed' %in% par_nams){
      met$Ten_Meter_Elevation_Wind_Speed_meterPerSecond <- met$Ten_Meter_Elevation_Wind_Speed_meterPerSecond * pars$wind_speed
    }

    if('solar_radiation' %in% par_nams){
      met$Shortwave_Radiation_Downwelling_wattPerMeterSquared <- met$Shortwave_Radiation_Downwelling_wattPerMeterSquared * pars$solar_radiation
    }

    # Write to file
    write.table(met, out_file, sep = '\t', quote = FALSE, col.names = FALSE, row.names = FALSE)
  }

  if('GLM' %in% model){

    par_nams <- names(pars)

    if('wind_speed' %in% par_nams){
      met$WindSpeed <- met$WindSpeed * pars$wind_speed
    }

    if('solar_radiation' %in% par_nams){
      met$ShortWave <- met$ShortWave * pars$solar_radiation
    }

    if('longwave_radiation' %in% par_nams){
      met$LongWave <- met$LongWave * pars$longwave_radiation
    }

    # Write to file
    write.csv(met, out_file, quote = FALSE, row.names = FALSE)
  }

  if('GOTM' %in% model){

    par_nams <- names(pars)

    if('wind_speed' %in% par_nams){
      met$Uwind_meterPerSecond <- met$Uwind_meterPerSecond * pars$wind_speed
      met$Vwind_meterPerSecond <- met$Vwind_meterPerSecond * pars$wind_speed
    }

    if('solar_radiation' %in% par_nams){
      met$Shortwave_Radiation_Downwelling_wattPerMeterSquared <- met$Shortwave_Radiation_Downwelling_wattPerMeterSquared * pars$solar_radiation
    }

    # Write to file
    write.table(met, out_file, quote = FALSE, row.names = FALSE, sep = '\t', col.names = TRUE)

  }

  if('Simstrat' %in% model){

    par_nams <- names(pars)

    if('wind_speed' %in% par_nams){
      met$Uwind_meterPerSecond <- met$Uwind_meterPerSecond * pars$wind_speed
      met$Vwind_meterPerSecond <- met$Vwind_meterPerSecond * pars$wind_speed
    }

    if('solar_radiation' %in% par_nams){
      met$Shortwave_Radiation_Downwelling_wattPerMeterSquared <- met$Shortwave_Radiation_Downwelling_wattPerMeterSquared * pars$solar_radiation
    }

    if('longwave_radiation' %in% par_nams){
      met$Longwave_Radiation_Downwelling_wattPerMeterSquared <- met$Longwave_Radiation_Downwelling_wattPerMeterSquared * pars$longwave_radiation
    }

    # Write to file
    write.table(met, file = out_file,sep = "\t",quote = F,row.names = F)

  }

}
