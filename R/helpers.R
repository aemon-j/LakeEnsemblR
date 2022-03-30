#' @title Gets depths from data frame containing profile info.
#'
#' @description Extracts the depth information from a data frame containing multi-depth
#' observation data. Relies on the format of the header to get information and
#' may fail if your file format is incorrect. Please follow 'VAR_##.#' format,
#' where ##.# is the depth of data for that column. VAR is typically 'wtr' to
#' indicate water temperature. The function is taken from the rLakeAnalyzer package and
#' all credit is to the rLakeAnalyzer authors (https://github.com/GLEON/rLakeAnalyzer)
#'
#'
#' @param data Data frame returned from \code{\link{load.ts}}.
#' @return A numeric vector of depth values. Should be the \code{ncol(data) -
#' 1} in length as the first column contains date/time data.
#' @keywords manip
#' @examples
#'
#'
#' #Get the path for the package example file included
#' exampleFilePath <- system.file('extdata', 'Sparkling.wtr', package="rLakeAnalyzer")
#'
#' #Load
#' sparkling.temp = load.ts(exampleFilePath)
#'
#' #get the lake depths associated with each column
#' depths = get.offsets(sparkling.temp)
#'
#' print(depths)
#' @noRd
get.offsets <- function(data){

  header <- names(data)

  #check for existence of datetime header and drop if there
  dt_indx <- grep(pattern = "datetime", x = header, ignore.case= TRUE)
  if(length(dt_indx) > 0){
    header <- header[-dt_indx] #Drop datetime
  }

  #match anything digits after the last _ (at the end of the line)
  matches <- regexpr("_\\d+\\.?\\d*$", header)

  lengths <- attr(matches, "match.length")
  offsets <- vector(mode = "numeric", length = length(matches))

  for(i in 1:length(matches)){
    offsets[i] <- as.numeric(substr(header[i], matches[i]+1, matches[i] + lengths[i]))
  }

  if(any(is.na(offsets))){
    warning("Problem determining variable depths from column names.
            Please use the 'var_#.#' format for your data.frame header." )
    }

  return(offsets)
}



#' Set met files and column numbers in gotm.yaml
#'@description
#' Set met files and column numbers in gotm.yaml
#'
#' @name set_met_config_yaml
#' @param met_file filepath; to metfile
#' @param yaml_file filepath; to gotm.yaml
#' @importFrom gotmtools read_yaml set_yaml write_yaml get_yaml_value
#' @noRd
set_met_config_yaml <- function(met_file, yaml_file){

  # Load Rdata
  data("met_var_dic", package = "LakeEnsemblR", envir = environment())

  ## list with long standard names
  l_names <- as.list(met_var_dic$standard_name)
  names(l_names) <- met_var_dic$short_name

  if(file.exists(met_file)){
    df <- read.delim(met_file, sep = "\t", nrows = 3)
  }else{
    stop("File ", met_file, "does not exist!")
  }

  # met_var_dic <- load_dic()

  yaml <- read_yaml(yaml_file)

  met_inp <- basename(met_file)

  ######
  # u10
  yaml <- set_yaml(yaml, "surface", "meteo", "u10", key = "column",
             value = as.integer(which(colnames(df) == l_names$u10) - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "u10", key = "scale_factor", value = 1.0)
  # v10
  yaml <- set_yaml(yaml, "surface", "meteo", "v10", key = "column",
             value = as.integer(which(colnames(df) == l_names$v10) - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "v10", key = "scale_factor", value = 1.0)
  # airp
  yaml <- set_yaml(yaml, "surface", "meteo", "airp", key = "column",
             value = as.integer(which(colnames(df) == l_names$p_surf) - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "airp", key = "scale_factor", value = 1.0)
  # airt
  yaml <- set_yaml(yaml, "surface", "meteo", "airt", key = "column",
            value = as.integer(which(colnames(df) == l_names$airt) - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "airt", key = "scale_factor", value = 1.0)
  # cloud
  yaml <- set_yaml(yaml, "surface", "meteo", "cloud", key = "column",
             value = as.integer(which(colnames(df) == l_names$cc) - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "cloud", key = "scale_factor", value = 1.0)
  # swr
  yaml <- set_yaml(yaml, "surface", "meteo", "swr", key = "column",
             value = as.integer(which(colnames(df) == l_names$swr) - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "swr", key = "scale_factor", value = 1.0)
  # precip
  yaml <- set_yaml(yaml, "surface", "meteo", "precip", key = "column",
             value = as.integer(which(colnames(df) == "Precipitation_meterPerSecond") - 1))
  yaml <- set_yaml(yaml, "surface", "meteo", "precip", key = "scale_factor", value = 1.0)

  if(l_names$relh %in% colnames(df)){
    # hum
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "column",
               value = as.integer(which(colnames(df) == l_names$relh) - 1))
    # 1=relative humidity (%), 2=wet-bulb temperature, 3=dew point temperature,
    # 4=specific humidity (kg/kg)
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "type", value = 1L)
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "scale_factor", value = 1.0)
  } else if(l_names$dewt %in% colnames(df)) {
    # hum
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "file", value = met_outfile)
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "column",
               value = as.integer(which(colnames(df) == l_names$dewt) - 1))
    # 1 = relative humidity (%), 2 = wet-bulb temperature, 3 = dew point temperature,
    # 4 = specific humidity (kg/kg)
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "type", value = 3L)
    yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "scale_factor", value = 1.0)
  }

  ######
  # u10
  yaml <- set_yaml(yaml, "surface", "meteo", "u10", key = "file", value = met_inp)
  # v10
  yaml <- set_yaml(yaml, "surface", "meteo", "v10", key = "file", value = met_inp)
  # airp
  yaml <- set_yaml(yaml, "surface", "meteo", "airp", key = "file", value = met_inp)
  # airt
  yaml <- set_yaml(yaml, "surface", "meteo", "airt", key = "file", value = met_inp)
  # cloud
  yaml <- set_yaml(yaml, "surface", "meteo", "cloud", key = "file", value = met_inp)
  # swr
  yaml <- set_yaml(yaml, "surface", "meteo", "swr", key = "file", value = met_inp)
  # precip
  yaml <- set_yaml(yaml, "surface", "meteo", "precip", key = "file", value = met_inp)
  # hum
  yaml <- set_yaml(yaml, "surface", "meteo", "hum", key = "file", value = met_inp)

  write_yaml(yaml, yaml_file)


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

#' Get time step from meteo file
#' @description
#' Get time step from meteo file (in seconds) and check for irregular or missing time steps
#'
#' @name get_meteo_time_step
#' @param met_file filepath; to meteo file in LakeEnsemblR standard format
#' @noRd
get_meteo_time_step <- function(met_file){

  # Fix time zone
  original_tz <- Sys.getenv("TZ")

  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    Sys.setenv(TZ = original_tz)
  })

  Sys.setenv(TZ = "GMT")

  # Read meteo file
  met <- read.csv(met_file)

  # Check if met file has a datetime column
  if(!("datetime" %in% colnames(met))){
    stop("Meteo file has no datetime column")
  }

  # Check for missing forcing
  if(any(is.na(met))){
    stop("Meteo file contains NA values; forcing cannot contain missing values!")
  }

  # Check for irregular time steps
  met[["datetime"]] <- as.POSIXct(met[["datetime"]])
  timesteps <- difftime(head(met[["datetime"]], -1), tail(met[["datetime"]], -1), units = "secs")

  if(length(unique(timesteps)) > 1){
    stop("Meteo file has irregular time steps!")
  }else{
    return(-1 * unique(timesteps))
  }
}


#' Create scaling factor dataframe from the config_file that is needed as input to scale_met
#' @description
#' Creates dataframe for the pars argument of the scale_met function, based on the config_file
#'
#' @name create_scaling_factors
#' @param config_file string; name of the master LakeEnsemblR config file
#' @param model string; model to create scaling_factors dataframe for
#' @param folder string; file path to the config_file
#' @noRd
create_scaling_factors <- function(config_file, model, folder){

  master_config <- configr::read.config(file.path(folder, config_file))

  pars <- data.frame(wind_speed = 1,
                     swr = 1,
                     lwr = 1)

  for(i in colnames(pars)){
    # Fill the scaling_factors based on what is filled in for "all"
    if(!is.null(master_config[["scaling_factors"]][["all"]][[i]])){
      pars[[i]][1] = master_config[["scaling_factors"]][["all"]][[i]]
    }

    # Overwrite with value filled in for the specific model
    if(!is.null(master_config[["scaling_factors"]][[model]][[i]])){
      pars[[i]][1] = master_config[["scaling_factors"]][[model]][[i]]
    }
  }


  # Return the data frame
  return(pars)
}
