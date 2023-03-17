#'Export extinction coefficients
#'
#'Exports extinction coefficients for each model based on a master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'
#'@importFrom gotmtools get_yaml_value input_yaml input_nml
#'
#'@export

export_extinction <- function(config_file,
                              model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                              folder = "."){

  # Set working directory
  oldwd <- getwd()
  setwd(folder)

  # Fix time zone
  original_tz <- Sys.getenv("TZ")

  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
    Sys.setenv(TZ = original_tz)
  })

  Sys.setenv(TZ = "GMT")
  
  # check model input
  model <- check_models(model)
  
  # Check if the value in the config file is a fixed value, or a file (time series)
  Kw <- get_yaml_value(config_file, "light", "Kw")
  
  if(Kw == "") {
    Kw <- configr::read.config(config_file)$input$light$Kw
    chk_nms <- !(names(Kw) %in% c("all", "Simstrat",
                                "GOTM", "GLM", "FLake", "MyLake"))
    if(any(chk_nms)) {
      stop(paste0("Unknown model '",
                     paste0(names(Kw)[chk_nms]),
                  "' in section 'input/light/Kw' of config file '",
                  config_file, "'."))
    }
    constant_value <- is.numeric(Kw$all)
    
    if(is.null(Kw$FLake)) {
      Kw_flake <- Kw$all
      constant_value_flake <- constant_value
    } else {
      Kw_flake <- Kw$FLake
      constant_value_flake <- is.numeric(Kw$FLake)
      if(!constant_value_flake) {
        Kw_file <- read.csv(file.path(folder, Kw))
        Kw_file$datetime <- as.POSIXct(Kw_file$datetime)
        
        start_time_series <- as.POSIXct(get_yaml_value(config_file, "time", "start"))
        end_time_series <- as.POSIXct(get_yaml_value(config_file, "time", "stop"))
        Kw_mylake <- time_average(Kw_file,
                                  start = start_time_series,
                                  end = end_time_series,
                                  n = 1000)
      }
    }
    if(is.null(Kw$GLM)) {
      Kw_glm <- Kw$all
      constant_value_glm <- constant_value
    } else {
      Kw_glm <- Kw$GLM
      constant_value_glm <- is.numeric(Kw$GLM)
    }
    if(is.null(Kw$GOTM)) {
      Kw_gotm <- Kw$all
      constant_value_gotm <- constant_value
    } else {
      Kw_gotm <- Kw$GOTM
      constant_value_gotm <- is.numeric(Kw$GOTM)
    }
    if(is.null(Kw$Simstrat)) {
      Kw_simstrat <- Kw$all
      constant_value_simstrat <- constant_value
    } else {
      Kw_simstrat <- Kw$Simstrat
      constant_value_simstrat <- is.numeric(Kw$Simstrat)
    }
    if(is.null(Kw$MyLake)) {
      Kw_mylake <- Kw$all
      constant_value_mylake <- constant_value
    } else {
      Kw_mylake <- Kw$MyLake
      constant_value_mylake <- is.numeric(Kw$MyLake)
      if(!constant_value_mylake) {
        Kw_file <- read.csv(file.path(folder, Kw))
        Kw_file$datetime <- as.POSIXct(Kw_file$datetime)
        
        start_time_series <- as.POSIXct(get_yaml_value(config_file, "time", "start"))
        end_time_series <- as.POSIXct(get_yaml_value(config_file, "time", "stop"))
        Kw_mylake <- time_average(Kw_file,
                           start = start_time_series,
                           end = end_time_series,
                           n = 1000)
      }
    }
    
  } else {
    if(is.numeric(Kw)){
      constant_value <- TRUE
    }else{
      constant_value <- FALSE
      Kw_file <- read.csv(file.path(folder, Kw))
      Kw_file$datetime <- as.POSIXct(Kw_file$datetime)
      
      start_time_series <- as.POSIXct(get_yaml_value(config_file, "time", "start"))
      end_time_series <- as.POSIXct(get_yaml_value(config_file, "time", "stop"))
      
      # Calculate time-averaged extinction coefficient, for models that can only
      # use a constant value
      Kw <- time_average(Kw_file,
                         start = start_time_series,
                         end = end_time_series,
                         n = 1000)
    }
    
    Kw_flake <- Kw
    Kw_glm <- Kw
    Kw_gotm <- Kw
    Kw_simstrat <- Kw
    Kw_mylake <- Kw
    constant_value_flake <- constant_value
    constant_value_glm <- constant_value
    constant_value_gotm <- constant_value
    constant_value_simstrat <- constant_value
    constant_value_mylake <- constant_value
  }
  
  

  if("FLake" %in% model){

    if(!constant_value_flake){
      message("FLake does not accept varying extinction coefficient over time. ",
              "Average is used instead.")
    }

    fla_fil <- get_yaml_value(config_file, "config_files", "FLake")

    input_nml(file.path(folder, fla_fil), label = "TRANSPARENCY", key = "extincoef_optic", Kw_flake)
  }

  if("GLM" %in% model){

    # Read the GLM config file from config_file, and write it to the GLM directory
    glm_nml <- get_yaml_value(config_file, "config_files", "GLM")
    glm_nml <- file.path(folder, glm_nml)
    
    if(constant_value_glm){
      # Write to nml: if any, replace the line with Kw_file and put Kw
      file_con <- file(file.path(glm_nml))
      glm_string <- readLines(file_con)
      # find the line with "Kw" in it and write the new line
      line_nr <- grep("Kw", glm_string)
      glm_string[line_nr] <- paste("   Kw =", Kw_glm)

      # Write nml file
      writeLines(glm_string, glm_nml)
      close(file_con)
    }else{
      # Write csv file
      Kw_GLM <- Kw_file_glm
      colnames(Kw_GLM) <- c("Date", "Kd") # sic
      write.csv(Kw_GLM, file.path(folder, "GLM", "Kw_GLM.csv"), row.names = FALSE, quote = FALSE)

      # Write to nml: if any, replace the line with Kw and put Kw_file
      file_con <- file(file.path(glm_nml))
      glm_string <- readLines(file_con)
      # find the line with "Kw" in it and write the new line
      line_nr <- grep("Kw", glm_string)
      glm_string[line_nr] <- "   Kw_file = 'Kw_GLM.csv'"

      # Write nml file
      writeLines(glm_string, glm_nml)
      close(file_con)
    }

  }

  if("GOTM" %in% model){

    # Read the GOTM config file from config_file, and write it to the GOTM directory
    got_yaml <- get_yaml_value(config_file, "config_files", "GOTM")
    got_yaml <- file.path(folder, got_yaml)

    if(constant_value_gotm){
      gotmtools::input_yaml(got_yaml, "g2", "method", 0)
      gotmtools::input_yaml(got_yaml, "g2", "constant_value", 1 / Kw_gotm)
    } else {

      gotmtools::input_yaml(got_yaml, "g2", "method", 2)

      # Write GOTM g2 file to the GOTM folder
      Kw_GOTM <- Kw_file_gotm
      Kw_GOTM$datetime <- format(Kw_GOTM$datetime, "%Y-%m-%d %H:%M:%S")
      Kw_GOTM$Extinction_Coefficient_perMeter <- 1 / Kw_GOTM$Extinction_Coefficient_perMeter
      colnames(Kw_GOTM)[2] <- "g2"
      colnames(Kw_GOTM)[1] <- paste0("!", colnames(Kw_GOTM)[1])

      write.table(Kw_GOTM, file.path(folder, "GOTM", "LakeEnsemblR_g2_GOTM.dat"),
                  sep = "\t", row.names = FALSE, quote = FALSE)

      gotmtools::input_yaml(got_yaml, "g2", "file", value = " LakeEnsemblR_g2_GOTM.dat")
      gotmtools::input_yaml(got_yaml, "g2", "column", 1)
    }

  }

  if("Simstrat" %in% model){
    
    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    sim_par <- get_yaml_value(config_file, "config_files", "Simstrat")
    sim_par <- file.path(folder, sim_par)

    light_fil <- system.file("extdata/absorption_langtjern.dat", package = "SimstratR")
    file.copy(from = light_fil, to = file.path(folder, "Simstrat", "light_absorption.dat"))
    
    input_json(sim_par, "Input", "Absorption", '"light_absorption.dat"')
    
    # Write absorption file
    absorption_line_1 <- "Time [d] (1.col)    z [m] (1.row)    Absorption [m-1] (rest)"
    absorption_line_2 <- "1"
    absorption_line_3 <- "-1 -1.00"

    if(constant_value_simstrat){
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      absorption_line_4 <- paste(start_sim, Kw_simstrat)
      absorption_line_5 <- paste(end_sim, Kw_simstrat)

      file_connection <- file("Simstrat/light_absorption.dat")
      writeLines(c(absorption_line_1, absorption_line_2, absorption_line_3,
                   absorption_line_4, absorption_line_5),
                 file_connection)
      close(file_connection)
    }else{
      # Change POSIXct into the Simstrat time format
      Kw_Simstrat <- Kw_file_simstrat
      reference_year <- get_json_value(sim_par, "Simulation", "Start year")
      Kw_Simstrat$datetime <- as.numeric(difftime(Kw_Simstrat$datetime,
                                                  as.POSIXct(paste0(reference_year, "-01-01")),
                                                  units = "days"))

      # Write to light_absorption.dat
      cat(absorption_line_1, absorption_line_2, absorption_line_3,
          sep = "\n", file = "Simstrat/light_absorption.dat")
      cat(apply(Kw_Simstrat, 1, paste0, collapse = " "),
          file = "Simstrat/light_absorption.dat",
          append = TRUE, sep = "\n")
    }

  }
  
  if("MyLake" %in% model){
    
    if(!constant_value_mylake){
      message("MyLake does not accept varying extinction coefficient over time. ",
              "Average is used instead.")
    }
    
    # Load MyLake config file
    load(file.path(folder, get_yaml_value(config_file, "config_files", "MyLake")))
    
    mylake_config[["Bio.par"]][2] <- Kw_mylake
    
    cnf_name <- gsub(".*/", "", gotmtools::get_yaml_value(config_file, "config_files", "MyLake"))
    save(mylake_config, file = file.path(folder, "MyLake", cnf_name))
  }
  
  message("export_extinction complete!")
}
