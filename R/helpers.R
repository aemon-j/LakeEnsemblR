#' Set met files and column numbers in gotm.yaml
#'@description
#' Set met files and column numbers in gotm.yaml
#'
#' @name set_met_config_yaml
#' @param met_file filepath; to metfile
#' @param yaml_file filepath; to gotm.yaml
#' @noRd
set_met_config_yaml <- function(met_file, yaml_file){

  if(file.exists(met_file)){
    df <- read.delim(met_file, sep = '\t', nrows = 3)
  }else{
    stop('File ', met_file, 'does not exist!')
  }

  # met_var_dic <- load_dic()

  yaml = yaml_file

  met_inp <- basename(met_file)

  ######
  #u10
  input_yaml(file = yaml, label = 'u10', key = 'column', value = (which(colnames(df) == "Uwind_meterPerSecond")-1))
  input_yaml(file = yaml, label = 'u10', key = 'scale_factor', value = 1)
  #v10
  input_yaml(file = yaml, label = 'v10', key = 'column', value = (which(colnames(df) == "Vwind_meterPerSecond")-1))
  input_yaml(file = yaml, label = 'v10', key = 'scale_factor', value = 1)
  #airp
  input_yaml(file = yaml, label = 'airp', key = 'column', value = (which(colnames(df) == "Surface_Level_Barometric_Pressure_pascal" )-1))
  input_yaml(file = yaml, label = 'airp', key = 'scale_factor', value = 1)
  #airt
  input_yaml(file = yaml, label = 'airt', key = 'column', value = (which(colnames(df) == "Air_Temperature_celsius")-1))
  input_yaml(file = yaml, label = 'airt', key = 'scale_factor', value = 1)
  #cloud
  input_yaml(file = yaml, label = 'cloud', key = 'column', value = (which(colnames(df) == "Cloud_Cover_decimalFraction")-1))
  input_yaml(file = yaml, label = 'cloud', key = 'scale_factor', value = 1)
  #swr
  input_yaml(file = yaml, label = 'swr', key = 'column', value = (which(colnames(df) == "Shortwave_Radiation_Downwelling_wattPerMeterSquared")-1))
  input_yaml(file = yaml, label = 'swr', key = 'scale_factor', value = 1)
  #precip
  input_yaml(file = yaml, label = 'precip', key = 'column', value = (which(colnames(df) == "Precipitation_meterPerSecond")-1))
  input_yaml(file = yaml, label = 'precip', key = 'scale_factor', value = 1)

  if("Relative_Humidity_percent" %in% colnames(df)){
    #hum
    input_yaml(file = yaml, label = 'hum', key = 'column', value = (which(colnames(df) == "Relative_Humidity_percent")-1))
    input_yaml(file = yaml, label = 'hum', key = 'type', value = 1) #1=relative humidity (%), 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)
    input_yaml(file = yaml, label = 'hum', key = 'scale_factor', value = 1)
  }else if("Dewpoint_Temperature_celsius" %in% colnames(df)){
    #hum
    input_yaml(file = yaml, label = 'hum', key = 'file', value = met_outfile)
    input_yaml(file = yaml, label = 'hum', key = 'column', value = (which(colnames(df) == "Dewpoint_Temperature_celsius")-1))
    input_yaml(file = yaml, label = 'hum', key = 'type', value = 3) #1=relative humidity (%), 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg)
    input_yaml(file = yaml, label = 'hum', key = 'scale_factor', value = 1)
  }

  ######
  #u10
  input_yaml(file = yaml, label = 'u10', key = 'file', value = met_inp)
  #v10
  input_yaml(file = yaml, label = 'v10', key = 'file', value = met_inp)
  #airp
  input_yaml(file = yaml, label = 'airp', key = 'file', value = met_inp)
  #airt
  input_yaml(file = yaml, label = 'airt', key = 'file', value = met_inp)
  #cloud
  input_yaml(file = yaml, label = 'cloud', key = 'file', value = met_inp)
  #swr
  input_yaml(file = yaml, label = 'swr', key = 'file', value = met_inp)
  #precip
  input_yaml(file = yaml, label = 'precip', key = 'file', value = met_inp)
  #hum
  input_yaml(file = yaml, label = 'hum', key = 'file', value = met_inp)


}
