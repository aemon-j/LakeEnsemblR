#' Convert LakeEnsemblR standardized input to Simstrat format
#'
#' @name format_met_file_simstrat
#' @param lake_folder String representing the path of the folder with the standard input data
#' @param meteo_file String filename of the meteorology file
#' @param par_file String filename of the par_file, the config file of simstrat. Including path
#' 
#' @export


format_met_file_simstrat <- function(lake_folder, meteo_filename, par_file = NULL){
  # We can also merge all these similar functions into one format_met_file function, with argument model = c("GOTM","Simstrat", etc.), as suggested in the Google doc file
  # Depending on the setup of the standard config file, we can omit reading exact titles and read column numbers
  
  # It's advisable to set timezone to GMT in order to avoid errors when reading time
  original_tz = Sys.getenv("tz")
  Sys.setenv(tz="GMT")
  
  
  ### Import data
  # I'd prefer to use a function that can read both comma and tab delimited. data.table::fread does this, but then it's data.table
  meteo_file = read.csv(paste0(lake_folder,"/",meteo_filename))
  
  ### Naming conventions standard input
  # Depending on the setup of the standard config file, we can omit reading exact titles and read column numbers
  colname_time = "datetime"
  colname_wind_speed = "Ten_Meter_Elevation_Wind_Speed_meterPerSecond"
  colname_wind_direction = "Ten_Meter_Elevation_Wind_Direction_degree"
  colname_air_temperature = "Air_Temperature_celcius"
  colname_relative_humidity = "Relative_Humidity_percent"
  colname_solar_radiation = "Shortwave_Radiation_Downwelling_wattPerMeterSquared"
  colname_longwave_radiation = "Longwave_Radiation_Downwelling_wattPerMeterSquared"
  colname_surface_pressure = "Surface_Level_Barometric_Pressure_pascal"
  colname_precipitation = "Precipitation_meterPerSecond"
  colname_snow = "Snowfall_meterPerDay"
  colname_vapour_pressure = "Vapor_Pressure_milliBar"
  colname_cloud_cover = "Cloud_Cover_decimalFraction"
  
  ### Check what met data is available, as this determines what model forcing option to use (in the simstrat config file)
  datetime = colname_time %in% colnames(meteo_file)
  wind_speed = colname_wind_speed %in% colnames(meteo_file)
  wind_direction = colname_wind_direction %in% colnames(meteo_file)
  air_temperature = colname_air_temperature %in% colnames(meteo_file)
  solar_radiation = colname_solar_radiation %in% colnames(meteo_file)
  vapour_pressure = colname_vapour_pressure %in% colnames(meteo_file)
  relative_humidity = colname_relative_humidity %in% colnames(meteo_file)
  longwave_radiation = colname_longwave_radiation %in% colnames(meteo_file)
  cloud_cover = colname_cloud_cover %in% colnames(meteo_file)
  # Availability of precipitation data only used for snow module
  precipitation = colname_precipitation %in% colnames(meteo_file)
  snowfall = colname_snow %in% colnames(meteo_file)
  
  # Required input file changes depending on the forcing mode in the config file
  # Ideally; read from Simstrat par-file, now hard-coded
  if(!is.null(par_file)){
    par_text = readLines(par_file, warn = F)
    
    ind_label=grep("ModelConfig",par_text)
    ind_key = grep("Forcing", par_text)
    
    ind_key = ind_key[ind_key > ind_label]
    ind_label = ind_label[ind_key > ind_label]
    ind_map <- ind_key[which.min(ind_key - ind_label)]
    
    spl1 <- strsplit(par_text[ind_map], c("!"))[[1]]
    spl2 <- gsub("\"","",strsplit(spl1[1], ": ")[[1]][2])
    forcing_mode <- strsplit(spl2[1], ", ")[[1]][1]
  }else{
    forcing_mode = "5"
  }
  # Optionally, we can check the availability of data to set the forcing mode (5 possible, no? Then 3, then 2, then 1)
  
  
  ### Pre-processing
  # Time
  if(datetime){
    # Time in simstrat is in decimal days since a defined start year
    # Read from par_file. If not possible, hard-coded to 1861 (based on ISIMIP simulations)
    if(!is.null(par_file)){
      par_text = readLines(par_file, warn = F)
      
      ind_label=grep("Simulation",par_text)
      ind_key = grep("Start year", par_text)
      
      ind_key = ind_key[ind_key > ind_label]
      ind_label = ind_label[ind_key > ind_label]
      ind_map <- ind_key[which.min(ind_key - ind_label)]
      
      spl1 <- strsplit(par_text[ind_map], c("!"))[[1]]
      spl2 <- gsub("\"","",strsplit(spl1[1], ": ")[[1]][2])
      start_year <- strsplit(spl2[1], ", ")[[1]][1]
    }else{
      start_year = "1861"
    }
    meteo_file$datetime = as.numeric(difftime(meteo_file$datetime,as.POSIXct(paste0(start_year,"-01-01")),units = "days"))
  }else{
    stop("Cannot find \"datetime\" column in the input file. Without this column, the model cannot run")
  }
  
  # Wind
  # If wind direction is provided, U and V wind components are calculated. If not, V wind is set to 0
  if(wind_direction){
    direction=270-meteo_file[[colname_wind_direction]] # Converting the wind direction to the "math" direction
    rads=direction/180*pi
    xcomp=meteo_file[[colname_wind_speed]]*cos(rads)
    ycomp=meteo_file[[colname_wind_speed]]*sin(rads)
    meteo_file$Uwind = xcomp
    meteo_file$Vwind = ycomp
  }else{
    meteo_file$Uwind_meterPerSecond = meteo_file[[colname_wind_speed]]
    meteo_file$Vwind_meterPerSecond = 0
  }
  
  # Humidity
  if(!vapour_pressure & relative_humidity){
    # Calculate vapour pressure as: relhum * saturated vapour pressure
    # Used formula for saturated vapour pressure from: 
    # Woolway, R. I., Jones, I. D., Hamilton, D. P., Maberly, S. C., Muraoka, K., Read, J. S., . . . Winslow, L. A. (2015). 
    # Automated calculation of surface energy fluxes with high-frequency lake buoy data. 
    # Environmental Modelling & Software, 70, 191-198. 
    
    meteo_file[[colname_vapour_pressure]]=meteo_file[[colname_relative_humidity]]/100 * 6.11 * exp(17.27 * meteo_file[[colname_air_temperature]] / (237.3 + meteo_file[[colname_air_temperature]]))
    
  }
  
  # If snow_module is true, there needs to be a precipitation (or snowfall) columnn. 
  # Ideally; read from Simstrat par-file, now hard-coded
  if(!is.null(par_file)){
    par_text = readLines(par_file, warn = F)
    
    ind_label=grep("ModelConfig",par_text)
    ind_key = grep("SnowModel", par_text)
    
    ind_key = ind_key[ind_key > ind_label]
    ind_label = ind_label[ind_key > ind_label]
    ind_map <- ind_key[which.min(ind_key - ind_label)]
    
    spl1 <- strsplit(par_text[ind_map], c("!"))[[1]]
    spl2 <- gsub("\"","",strsplit(spl1[1], ": ")[[1]][2])
    snow_module <- strsplit(spl2[1], " ")[[1]][1]=="1"
  }else{
    snow_module = T
  }
  # Optionally, if there is no precipitation/snowfall column, we can set the snow_module to FALSE
  
  if(snow_module & !(precipitation | snowfall)){
    stop("There is no precipitation data and the snow_module is set to TRUE.")
  }
  
  
  # Precipitation
  # Precipitation needs to be in m h-1: 1 m s-1 = 3600 m h-1, or 1 m d-1 = 1/24 m h-1
  if(precipitation){
    meteo_file$`Precipitation_meterPerHour`=meteo_file[[colname_precipitation]]*3600
  }else if(snowfall){
    meteo_file$`Precipitation_meterPerHour`=meteo_file[[colname_snow]]/24
  }
  
  
  
  
  ### Build simstrat_forcing file
  # Boolean to see if there is enough data to write the meteo file
  enoughData=T

  
  # Now build the simstrat forcing file, based on the forcing_mode. If data is not available, an error message is displayed
  if(forcing_mode == "5"){
    if(!(wind_speed & air_temperature & solar_radiation & (vapour_pressure | relative_humidity) & longwave_radiation)){
      enoughData = F
    }else{
      simstrat_forcing = meteo_file[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond", 
                                        colname_air_temperature, colname_solar_radiation, colname_vapour_pressure,
                                        colname_longwave_radiation)]
      if(snow_module){
        simstrat_forcing[["Precipitation_meterPerHour"]] = meteo_file[["Precipitation_meterPerHour"]]
      }
    }
  }else if(forcing_mode == "4"){
    # Forcing mode 4 requires one column with "heat flux" input. LakeEnsemblR does not yet have functionality for this option
    enoughData = F
  }else if(forcing_mode == "3"){
    if(!(wind_speed & air_temperature & solar_radiation & (vapour_pressure | relative_humidity) & cloud_cover)){
      enoughData = F
    }else{
      simstrat_forcing = meteo_file[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond", 
                                        colname_air_temperature, colname_solar_radiation, colname_vapour_pressure,
                                        colname_cloud_cover)]
      if(snow_module){
        simstrat_forcing[["Precipitation_meterPerHour"]] = meteo_file[["Precipitation_meterPerHour"]]
      }
    }
  }else if(forcing_mode == "2"){
    if(!(wind_speed & air_temperature & solar_radiation & (vapour_pressure | relative_humidity))){
      enoughData = F
    }else{
      simstrat_forcing = meteo_file[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond", 
                                        colname_air_temperature, colname_solar_radiation, colname_vapour_pressure)]
      if(snow_module){
        simstrat_forcing[["Precipitation_meterPerHour"]] = meteo_file[["Precipitation_meterPerHour"]]
      }
    }
  }else if(forcing_mode == "1"){
    if(!(wind_speed & air_temperature & solar_radiation)){
      enoughData = F
    }else{
      simstrat_forcing = meteo_file[, c(colname_time, "Uwind_meterPerSecond", "Vwind_meterPerSecond", 
                                        colname_air_temperature, colname_solar_radiation)]
      if(snow_module){
        simstrat_forcing[["Precipitation_meterPerHour"]] = meteo_file[["Precipitation_meterPerHour"]]
      }
    }
  }
  
  if(!enoughData){stop(paste("There is no data to run the model in forcing mode",forcing_mode))}
  
  ### Write the table in the present working directory
  write.table(simstrat_forcing,file = "LakeEnsemblR_Simstrat_forcing.dat",sep = "\t",quote = F,row.names = F)
  
  # Set the timezone back to the original
  Sys.setenv(tz=original_tz)
  
  
}