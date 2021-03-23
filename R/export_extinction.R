#'Export extinction coefficients
#'
#'Exports extinction coefficients for each model based on a master LakeEnsemblR config file
#'
#' @param config_file name of the master LakeEnsemblR config file
#' @param model vector; model to export configuration file.
#'  Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#' @param folder folder
#' @keywords methods
#' @importFrom vroom vroom vroom_write
#' @importFrom gotmtools read_yaml set_yaml write_yaml get_yaml_value
#'
#'
#'@export

export_extinction <- function(config_file,
                              model = c("GOTM", "GLM", "Simstrat", "FLake"),
                              folder = "."){
  
  if(!file.exists(file.path(folder, config_file))) {
    stop(paste0(file.path(folder, config_file), " does not exist. Make sure your file path is correct"))
  } else {
    yaml <- gotmtools::read_yaml(config_file)
  }

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
  Kw <- gotmtools::get_yaml_value(yaml, "input", "light", "Kw")
  if(is.numeric(Kw)){
    constant_value <- TRUE
  }else{
    constant_value <- FALSE
    suppressMessages({Kw_file <- vroom::vroom(file.path(folder, Kw), col_types = list("c", "n"))})
    Kw_file$datetime <- as.POSIXct(Kw_file$datetime)

    start_time_series <- as.POSIXct(gotmtools::get_yaml_value(yaml, "time", "start"))
    end_time_series <- as.POSIXct(gotmtools::get_yaml_value(yaml, "time", "stop"))

    # Calculate time-averaged extinction coefficient, for models that can only
    # use a constant value
    Kw <- time_average(Kw_file,
                      start = start_time_series,
                      end = end_time_series,
                      n = 1000)

  }

  if("FLake" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("FLake")){
      dir.create("FLake")
    }
    if(!dir.exists("FLake/output")){
      dir.create("FLake/output")
    }

    if(!constant_value){
      message("FLake does not accept varying extinction coefficient over time. ",
              "Average is used instead.")
    }

    # Read the FLake config file from config_file, and write it to the FLake directory
    temp_fil <- gotmtools::get_yaml_value(yaml, "config_files", "FLake")
    if(file.exists(temp_fil)){
      fla_fil <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/flake_template.nml", package = packageName())
      file.copy(from = template_file,
                to = file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "FLake")))
      fla_fil <- file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "FLake"))
    }

    input_nml(fla_fil, label = "TRANSPARENCY", key = "extincoef_optic", Kw)
  }

  if("GLM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("GLM/output")){
      dir.create("GLM/output", recursive = TRUE)
    }

    # Read the GLM config file from config_file, and write it to the GLM directory
    temp_fil <- gotmtools::get_yaml_value(yaml, "config_files", "GLM")

    if(file.exists(temp_fil)){
      glm_nml <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/glm3_template.nml", package = packageName()) #
      file.copy(from = template_file,
                to = file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "GLM")))
      glm_nml <- file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "GLM"))
    }

    if(constant_value){
      # Write to nml: if any, replace the line with Kw_file and put Kw
      file_con <- file(file.path(glm_nml))
      glm_string <- readLines(file_con)
      # find the line with "Kw" in it and write the new line
      line_nr <- grep("Kw", glm_string)
      glm_string[line_nr] <- paste("   Kw =", Kw)

      # Write nml file
      writeLines(glm_string, glm_nml)
      close(file_con)
    }else{
      # Write csv file
      Kw_GLM <- Kw_file
      colnames(Kw_GLM) <- c("Date", "Kd") # sic
      Kw_GLM <- as.data.frame(Kw_GLM)
      Kw_GLM[, 1] <- format(Kw_GLM[, 1], format = "%Y-%m-%d %H:%M:%S")
      vroom::vroom_write(Kw_GLM, file.path(folder, "GLM", "Kw_GLM.csv"), delim = ",", )

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

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("GOTM/output")){
      dir.create("GOTM/output", recursive = TRUE)
    }

    # Read the GOTM config file from config_file, and write it to the GOTM directory
    temp_fil <- gotmtools::get_yaml_value(yaml, "config_files", "GOTM")
    if(file.exists(temp_fil)){
      got_file <- temp_fil
    } else {
      # This will work once we build the package
      template_file <- system.file("extdata/gotm_template.yaml", package = packageName())
      file.copy(from = template_file,
                to = file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "GOTM")))
      got_file <- file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "GOTM"))
    }
    
    got_yaml <- gotmtools::read_yaml(got_file)

    if(constant_value){
      got_yaml <- gotmtools::set_yaml(got_yaml, "light_extinction", "g2", "method", value = 0L)
      got_yaml <- gotmtools::set_yaml(got_yaml, "light_extinction", "g2", "constant_value", value = (1 / Kw))
    } else {

      got_yaml <- gotmtools::set_yaml(got_yaml, "light_extinction", "g2", "method", value = 2L)

      # Write GOTM g2 file to the GOTM folder
      Kw_GOTM <- Kw_file
      Kw_GOTM$Extinction_Coefficient_perMeter <- 1 / Kw_GOTM$Extinction_Coefficient_perMeter
      colnames(Kw_GOTM)[2] <- "g2"
      colnames(Kw_GOTM)[1] <- paste0("!", colnames(Kw_GOTM)[1])

      Kw_GOTM <- as.data.frame(Kw_GOTM)
      Kw_GOTM[, 1] <- format(Kw_GOTM[, 1], format = "%Y-%m-%d %H:%M:%S")
      vroom::vroom_write(Kw_GOTM, file.path(folder, "GOTM", "LakeEnsemblR_g2_GOTM.dat"), delim = "\t", quote = "none")

      got_yaml <- gotmtools::set_yaml(got_yaml, "light_extinction", "g2", "file",
                           value = " LakeEnsemblR_g2_GOTM.dat")
      got_yaml <- gotmtools::set_yaml(got_yaml, "light_extinction", "g2", "column", value =  1L)
    }
    
    gotmtools::write_yaml(got_yaml, got_file)

  }

  if("Simstrat" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("Simstrat/output")){
      dir.create("Simstrat/output", recursive = TRUE)
    }

    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    temp_fil <- gotmtools::get_yaml_value(yaml, "config_files", "Simstrat")
    if(file.exists(temp_fil)){
      sim_par <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/simstrat_template.par", package = packageName())
      file.copy(from = template_file,
                to = file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "Simstrat")))
      sim_par <- file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "Simstrat"))
    }

    light_fil <- system.file("extdata/absorption_langtjern.dat", package = "SimstratR")
    file.copy(from = light_fil, to = file.path(folder, "Simstrat", "light_absorption.dat"))
    
    input_json(sim_par, "Input", "Absorption", '"light_absorption.dat"')
    
    # Write absorption file
    absorption_line_1 <- "Time [d] (1.col)    z [m] (1.row)    Absorption [m-1] (rest)"
    absorption_line_2 <- "1"
    absorption_line_3 <- "-1 -1.00"

    if(constant_value){
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      absorption_line_4 <- paste(start_sim, Kw)
      absorption_line_5 <- paste(end_sim, Kw)

      file_connection <- file("Simstrat/light_absorption.dat")
      writeLines(c(absorption_line_1, absorption_line_2, absorption_line_3,
                   absorption_line_4, absorption_line_5),
                 file_connection)
      close(file_connection)
    }else{
      # Change POSIXct into the Simstrat time format
      Kw_Simstrat <- Kw_file
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
    
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("MyLake")){
      dir.create("MyLake")
    }
    if(!dir.exists("MyLake/output")){
      dir.create("MyLake/output")
    }
    
    if(!constant_value){
      message("MyLake does not accept varying extinction coefficient over time. ",
              "Average is used instead.")
    }
    
    # Load MyLake config file
    load(gotmtools::get_yaml_value(yaml, "config_files", "MyLake"))
    
    mylake_config[["Bio.par"]][2] <- Kw
    
    cnf_name <- gsub(".*/", "", gotmtools::get_yaml_value(yaml, "config_files", "MyLake"))
    save(mylake_config, file = file.path(folder, "MyLake", cnf_name))
  }
  
  message("export_extinction complete!")
}
