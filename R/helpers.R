#' Set met files and column numbers in gotm.yaml
#'@description
#' Set met files and column numbers in gotm.yaml
#'
#' @name set_met_config_yaml
#' @param met_file filepath; to metfile
#' @param yaml_file filepath; to gotm.yaml
#' @noRd
set_met_config_yaml <- function(met_file, yaml_file){

  ## list with long standard names
  l_names <- as.list(met_var_dic$standard_name)
  names(l_names) <- met_var_dic$short_name

  if(file.exists(met_file)){
    df <- read.delim(met_file, sep = "\t", nrows = 3)
  }else{
    stop("File ", met_file, "does not exist!")
  }

  # met_var_dic <- load_dic()

  yaml  <-  yaml_file

  met_inp <- basename(met_file)

  ######
  # u10
  input_yaml(file = yaml, label = "u10", key = "column",
             value = (which(colnames(df) == l_names$u10) - 1))
  input_yaml(file = yaml, label = "u10", key = "scale_factor", value = 1)
  # v10
  input_yaml(file = yaml, label = "v10", key = "column",
             value = (which(colnames(df) == l_names$v10) - 1))
  input_yaml(file = yaml, label = "v10", key = "scale_factor", value = 1)
  # airp
  input_yaml(file = yaml, label = "airp", key = "column",
             value = (which(colnames(df) == l_names$p_surf) - 1))
  input_yaml(file = yaml, label = "airp", key = "scale_factor", value = 1)
  # airt
  input_yaml(file = yaml, label = "airt", key = "column",
            value = (which(colnames(df) == l_names$airt) - 1))
  input_yaml(file = yaml, label = "airt", key = "scale_factor", value = 1)
  # cloud
  input_yaml(file = yaml, label = "cloud", key = "column",
             value = (which(colnames(df) == l_names$cc) - 1))
  input_yaml(file = yaml, label = "cloud", key = "scale_factor", value = 1)
  # swr
  input_yaml(file = yaml, label = "swr", key = "column",
             value = (which(colnames(df) == l_names$swr) - 1))
  input_yaml(file = yaml, label = "swr", key = "scale_factor", value = 1)
  # precip
  input_yaml(file = yaml, label = "precip", key = "column",
             value = (which(colnames(df) == l_names$precip) - 1))
  input_yaml(file = yaml, label = "precip", key = "scale_factor", value = 1)

  if(l_names$relh %in% colnames(df)){
    # hum
    input_yaml(file = yaml, label = "hum", key = "column",
               value = (which(colnames(df) == l_names$relh) - 1))
    # 1=relative humidity (%), 2=wet-bulb temperature, 3=dew point temperature,
    # 4=specific humidity (kg/kg)
    input_yaml(file = yaml, label = "hum", key = "type", value = 1)
    input_yaml(file = yaml, label = "hum", key = "scale_factor", value = 1)
  } else if(l_names$dewt %in% colnames(df)) {
    # hum
    input_yaml(file = yaml, label = "hum", key = "file", value = met_outfile)
    input_yaml(file = yaml, label = "hum", key = "column",
               value = (which(colnames(df) == l_names$dewt) - 1))
    # 1 = relative humidity (%), 2 = wet-bulb temperature, 3 = dew point temperature,
    # 4 = specific humidity (kg/kg)
    input_yaml(file = yaml, label = "hum", key = "type", value = 3)
    input_yaml(file = yaml, label = "hum", key = "scale_factor", value = 1)
  }

  ######
  # u10
  input_yaml(file = yaml, label = "u10", key = "file", value = met_inp)
  # v10
  input_yaml(file = yaml, label = "v10", key = "file", value = met_inp)
  # airp
  input_yaml(file = yaml, label = "airp", key = "file", value = met_inp)
  # airt
  input_yaml(file = yaml, label = "airt", key = "file", value = met_inp)
  # cloud
  input_yaml(file = yaml, label = "cloud", key = "file", value = met_inp)
  # swr
  input_yaml(file = yaml, label = "swr", key = "file", value = met_inp)
  # precip
  input_yaml(file = yaml, label = "precip", key = "file", value = met_inp)
  # hum
  input_yaml(file = yaml, label = "hum", key = "file", value = met_inp)


}

#' Convert DewT to RelH
#'@description
#' Convert DewT to RelH from weathermetrics package
#'
#' @name set_met_config_yaml
#' @param met_file filepath; to metfile
#' @param yaml_file filepath; to gotm.yaml
#' @noRd
dewt2relh <- function(dewt, airt) {

  if (length(dewt) != length(airt)) {
    stop(paste0("The vectors for temperature('airt') and dewpoint temperature ('dewt')",
                " must have the same length."))
  }
  if (length(dewt[dewt > airt & !is.na(dewt) & !is.na(airt)]) > 0) {
    dewt[dewt > airt] <- NA
    warning(paste0("For some observations, dew point temperature was higher than temperature.",
                   " Since dew point temperature cannot be higher than air temperature, ",
                   "relative humidty for these observations was set to 'NA'."))
  }
  beta <- (112 - (0.1 * airt) + dewt) / (112 + (0.9 * airt))
  relative_humidity <- 100 * beta^8
  return(relative_humidity)
}


#' Calculate wind direction (degrees) from u and v vectors
#' @description
#' Calculate wind direction (degrees) from u and v vectors.
#' Idea from: http://colaweb.gmu.edu/dev/clim301/lectures/wind/wind-uv
#'
#' @name calc_windDir
#' @param met_file filepath; to metfile
#' @param yaml_file filepath; to gotm.yaml
#' @noRd
calc_wind_dir <- function(u, v) {
  ws  <-  atan2(v, u) * (180 / pi)
  ws <-  ws + 180
  return(ws)
}
