#' Format meteorological data for each model
#'@description
#' Format dataframe into shape for the specified model
#'
#' @name scale_met
#' @param met dataframe; as read.csv() from standardised input.
#' @param model character; Model for which scaling parameters will be applied. Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#' @param daily logical; Is the data on a daily timestep. Defaults to FALSE
#' @param start character; start date in format '%Y-%m-%d %H:%M:%S'. Only used if model = 'FLake'. If NULL uses entire dataframe. Defaults to NULL.
#' @param stop character; stop date in format '%Y-%m-%d %H:%M:%S'. Only used if model = 'FLake'. If NULL uses entire dataframe. Defaults to NULL.
#' @param par_file filepath; To Simstrat par file. Only used if model = 'Simstrat'
#' @param config_file filepath; To LER config yaml file. Only used if model = 'GOTM'
#' @return dataframe of met data in the model format
#' @export
format_met <- function(met, model, daily = FALSE, start = NULL, stop = NULL, par_file, config_file){

  ### Naming conventions standard input
  # Depending on the setup of the standard config file, we can omit reading exact titles and read column numbers
  colname_time = "datetime"
  colname_wind_speed = "Ten_Meter_Elevation_Wind_Speed_meterPerSecond"
  colname_wind_direction = "Ten_Meter_Elevation_Wind_Direction_degree"
  colname_air_temperature = "Air_Temperature_celsius"
  colname_dewpoint_temperature = "Dewpoint_Temperature_celsius"
  colname_relative_humidity = "Relative_Humidity_percent"
  colname_solar_radiation = "Shortwave_Radiation_Downwelling_wattPerMeterSquared"
  colname_longwave_radiation = "Longwave_Radiation_Downwelling_wattPerMeterSquared"
  colname_surface_pressure = "Surface_Level_Barometric_Pressure_pascal"
  colname_precipitation = "Precipitation_meterPerSecond"
  colname_snow = "Snowfall_meterPerDay"
  colname_vapour_pressure = "Vapor_Pressure_milliBar"
  colname_cloud_cover = "Cloud_Cover_decimalFraction"

  ### Check what met data is available, as this determines what model forcing option to use (in the simstrat config file)
  datetime = colname_time %in% colnames(met)
  wind_speed = colname_wind_speed %in% colnames(met)
  wind_direction = colname_wind_direction %in% colnames(met)
  air_temperature = colname_air_temperature %in% colnames(met)
  dewpoint_temperature = colname_dewpoint_temperature %in% colnames(met)
  solar_radiation = colname_solar_radiation %in% colnames(met)
  vapour_pressure = colname_vapour_pressure %in% colnames(met)
  relative_humidity = colname_relative_humidity %in% colnames(met)
  longwave_radiation = colname_longwave_radiation %in% colnames(met)
  cloud_cover = colname_cloud_cover %in% colnames(met)
  # Availability of precipitation data only used for snow module
  precipitation = colname_precipitation %in% colnames(met)
  snowfall = colname_snow %in% colnames(met)


  if('FLake' %in% model){

    # Subset temporally
    if(!is.null(start) & !is.null(stop)){
      fla_met <- met[(met[,1] >= start & met[,1] < stop),]
    }else{
      fla_met <- met
    }


    fla_met$index <- 1:nrow(fla_met)

    # Re-organise
    fla_met <- fla_met[,c('index','Shortwave_Radiation_Downwelling_wattPerMeterSquared','Air_Temperature_celsius', "Vapor_Pressure_milliBar", "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", "Cloud_Cover_decimalFraction", "datetime")]
    fla_met$datetime <- format(fla_met$datetime, format = '%Y-%m-%d %H:%M:%S')
    colnames(fla_met)[1] <- paste0('!', colnames(fla_met)[1])

    return(fla_met)
  }

  if('GLM' %in% model){

    glm_met <- met

    # Convert units
    glm_met$Precipitation_meterPerDay <- glm_met$Precipitation_meterPerSecond * 86400


    glm_met <- glm_met[,c('datetime','Shortwave_Radiation_Downwelling_wattPerMeterSquared', "Longwave_Radiation_Downwelling_wattPerMeterSquared", 'Air_Temperature_celsius', 'Relative_Humidity_percent', "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", "Precipitation_meterPerDay", "Snowfall_meterPerDay")]

    colnames(glm_met) <- c('Date','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow')
    glm_met[,1] <- format(glm_met[,1], format = '%Y-%m-%d %H:%M:%S')

    return(glm_met)
  }

  if('GOTM' %in% model){

    got_met <- met

    lat <- get_yaml_value(file = config_file, label = 'location', key = 'latitude')
    lon <- get_yaml_value(file = config_file, label = 'location', key = 'longitude')
    elev <- get_yaml_value(file = config_file, label = 'location', key = 'elevation')

    met_outfile <- 'LHS_meteo_file.dat'




    got_met <- got_met[,c('datetime', 'Uwind_meterPerSecond', 'Vwind_meterPerSecond', 'Surface_Level_Barometric_Pressure_pascal', 'Air_Temperature_celsius', 'Relative_Humidity_percent', 'Cloud_Cover_decimalFraction', 'Shortwave_Radiation_Downwelling_wattPerMeterSquared', 'Precipitation_meterPerSecond')]

    colnames(got_met)[1] <- paste0('!', colnames(got_met)[1])
    got_met[,1] <- format(got_met[,1], '%Y-%m-%d %H:%M:%S')

    #Reduce number of digits
    got_met[,-1] <- signif(got_met[,-1], digits = 8)

    return(got_met)
  }

  if('Simstrat' %in% model){

    sim_met <- met

    # Required input file changes depending on the forcing mode in the config file
    forcing_mode <- get_json_value(par_file, "ModelConfig", "Forcing")

    ### Pre-processing
    # Time
    if('datetime' %in% colnames(sim_met)){
      # Time in simstrat is in decimal days since a defined start year
      start_year <- get_json_value(par_file, "Simulation", "Start year")

      sim_met$datetime = as.numeric(difftime(sim_met$datetime,as.POSIXct(paste0(start_year,"-01-01")),units = "days"))
    }else{
      stop("Cannot find \"datetime\" column in the input file. Without this column, the model cannot run")
    }

    # If snow_module is true, there needs to be a precipitation (or snowfall) columnn.
    snow_module <- get_json_value(par_file, "ModelConfig", "SnowModel") == 1
    # Optionally, if there is no precipitation/snowfall column, we can set the snow_module to FALSE

    if(snow_module & !(precipitation | snowfall)){
      stop("There is no precipitation data and the Simstrat snow_module is set to TRUE.")
    }


    ### Build sim_met file
    # Boolean to see if there is enough data to write the meteo file
    enoughData=T


    # Now build the simstrat forcing file, based on the forcing_mode. If data is not available, an error message is displayed
    if(forcing_mode == "5"){
      if(!(wind_speed & air_temperature & solar_radiation & (vapour_pressure | relative_humidity) & longwave_radiation)){
        enoughData = F
      }else{
        sim_met = sim_met[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond",
                                       colname_air_temperature, colname_solar_radiation, colname_vapour_pressure,
                                       colname_longwave_radiation)]
        if(snow_module){
          sim_met[["Precipitation_meterPerHour"]] = sim_met[["Precipitation_meterPerHour"]]
        }
      }
    }else if(forcing_mode == "4"){
      # Forcing mode 4 requires one column with "heat flux" input. LakeEnsemblR does not yet have functionality for this option
      enoughData = F
    }else if(forcing_mode == "3"){
      if(!(wind_speed & air_temperature & solar_radiation & (vapour_pressure | relative_humidity) & cloud_cover)){
        enoughData = F
      }else{
        sim_met = sim_met[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond",
                                       colname_air_temperature, colname_solar_radiation, colname_vapour_pressure,
                                       colname_cloud_cover)]
        if(snow_module){
          sim_met[["Precipitation_meterPerHour"]] = sim_met[["Precipitation_meterPerHour"]]
        }
      }
    }else if(forcing_mode == "2"){
      if(!(wind_speed & air_temperature & solar_radiation & (vapour_pressure | relative_humidity))){
        enoughData = F
      }else{
        sim_met = sim_met[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond",
                                       colname_air_temperature, colname_solar_radiation, colname_vapour_pressure)]
        if(snow_module){
          sim_met[["Precipitation_meterPerHour"]] = sim_met[["Precipitation_meterPerHour"]]
        }
      }
    }else if(forcing_mode == "1"){
      if(!(wind_speed & air_temperature & solar_radiation)){
        enoughData = F
      }else{
        sim_met = sim_met[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond",
                                       colname_air_temperature, colname_solar_radiation)]
        if(snow_module){
          sim_met[["Precipitation_meterPerHour"]] = sim_met[["Precipitation_meterPerHour"]]
        }
      }
    }

    if(!enoughData){stop(paste("There is no data to run the model in forcing mode",forcing_mode))}

    return(sim_met)
  }
}
