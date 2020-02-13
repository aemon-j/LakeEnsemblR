#'Export extinction coefficients
#'
#'Exports extinction coefficients for each model based on a master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file. Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'
#'@importFrom gotmtools get_yaml_value input_yaml input_nml
#'
#'@export

export_extinction <- function(config_file, model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.'){

  # Set working directory
  oldwd <- getwd()
  setwd(folder)

  # Fix time zone
  original_tz = Sys.getenv("TZ")

  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
    Sys.setenv(TZ=original_tz)
  })

  Sys.setenv(TZ="GMT")

  # Check if the value in the config file is a fixed value, or a file (time series)
  Kw <- get_yaml_value(config_file, "light", "Kw")
  if(is.numeric(Kw)){
    constantValue = T
  }else{
    constantValue = F
    Kw_file <- read.csv(file.path(folder,Kw))
    Kw_file$datetime <- as.POSIXct(Kw_file$datetime)

    startTimeSeries <- as.POSIXct(get_yaml_value(config_file, "time", "start"))
    endTimeSeries <- as.POSIXct(get_yaml_value(config_file, "time", "stop"))

    # Calculate time-averaged extinction coefficient, for models that can only
    # use a constant value
    Kw <- time_average(Kw_file,
                      start=startTimeSeries,
                      end=endTimeSeries,
                      n=1000)

  }

  if("FLake" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('FLake')){
      dir.create('FLake')
    }
    if(!dir.exists('FLake/output')){
      dir.create('FLake/output')
    }

    if(!constantValue){
      message("FLake does not accept varying extinction coefficient over time. Average is used instead.")
    }

    # Read the FLake config file from config_file, and write it to the FLake directory
    temp_fil <- get_yaml_value(config_file, "config_files", "flake")
    if(file.exists(temp_fil)){
      fla_fil <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file('extdata/flake_template.nml', package = packageName())
      file.copy(from = template_file, to = file.path(folder, 'FLake', basename(temp_fil)))
      fla_fil <- file.path(folder, 'FLake', basename(temp_fil))
    }

    input_nml(fla_fil, label = 'TRANSPARENCY', key = 'extincoef_optic', Kw)
  }

  if("GLM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('GLM/output')){
      dir.create('GLM/output', recursive = TRUE)
    }

    # Read the GLM config file from config_file, and write it to the GLM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "glm")

    if(file.exists(temp_fil)){
      glm_nml <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/glm3_template.nml", package = packageName()) #
      file.copy(from = template_file, to = file.path(folder, 'GLM', basename(temp_fil)))
      glm_nml <- file.path(folder, 'GLM', basename(temp_fil))
    }

    if(constantValue){
      # Write to nml: if any, replace the line with Kw_file and put Kw
      fileCon <- file(file.path(glm_nml))
      glm_string <- readLines(fileCon)
      # find the line with "Kw" in it and write the new line
      lineNr <- grep("Kw", glm_string)
      glm_string[lineNr] <- paste("   Kw =", Kw)

      # Write nml file
      writeLines(glm_string,glm_nml)
      close(fileCon)
    }else{
      # Write csv file
      Kw_GLM <- Kw_file
      colnames(Kw_GLM) <- c("Date","Kd") # sic
      write.csv(Kw_GLM, file.path(folder,"GLM","Kw_GLM.csv"), row.names = F, quote = F)

      # Write to nml: if any, replace the line with Kw and put Kw_file
      fileCon <- file(file.path(glm_nml))
      glm_string <- readLines(fileCon)
      # find the line with "Kw" in it and write the new line
      lineNr <- grep("Kw", glm_string)
      glm_string[lineNr] <- "   Kw_file = 'Kw_GLM.csv'"

      # Write nml file
      writeLines(glm_string,glm_nml)
      close(fileCon)
    }

  }

  if("GOTM" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('GOTM/output')){
      dir.create('GOTM/output', recursive = TRUE)
    }

    # Read the GOTM config file from config_file, and write it to the GOTM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "gotm")
    if(file.exists(temp_fil)){
      got_yaml <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/gotm_template.yaml", package = packageName())
      file.copy(from = template_file, to = file.path(folder, 'GOTM', basename(temp_fil)))
      got_yaml <- file.path(folder, 'GOTM', basename(temp_fil))
    }

    if(constantValue){
      gotmtools::input_yaml(got_yaml, 'g2', 'method',0)
      gotmtools::input_yaml(got_yaml, 'g2', 'constant_value',1/Kw)
    }else{

      gotmtools::input_yaml(got_yaml, 'g2', 'method',2)

      # Write GOTM g2 file to the GOTM folder
      Kw_GOTM <- Kw_file
      Kw_GOTM$Extinction_Coefficient_perMeter <- 1/Kw_GOTM$Extinction_Coefficient_perMeter
      colnames(Kw_GOTM)[2] <- "g2"
      colnames(Kw_GOTM)[1] <- paste0("!",colnames(Kw_GOTM)[1])

      write.table(Kw_GOTM, file.path(folder,"GOTM","LakeEnsemblR_g2_GOTM.dat"),
                  sep = "\t", row.names = F, quote = F)

      gotmtools::input_yaml(got_yaml, 'g2', 'file', value=" LakeEnsemblR_g2_GOTM.dat")
      gotmtools::input_yaml(got_yaml, 'g2', 'column', 1)
    }

  }

  if("Simstrat" %in% model){

    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('Simstrat/output')){
      dir.create('Simstrat/output', recursive = TRUE)
    }

    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    temp_fil <- get_yaml_value(config_file, "config_files", "simstrat")
    if(file.exists(temp_fil)){
      sim_par <- temp_fil
    }else{
      # This will work once we build the package
      template_file <- system.file("extdata/simstrat_template.par", package = packageName())
      file.copy(from = template_file, to = file.path(folder, 'Simstrat', basename(temp_fil)))
      sim_par <- file.path(folder, 'Simstrat', basename(temp_fil))
    }

    light_fil <- system.file('extdata/absorption_langtjern.dat', package= 'SimstratR')
    file.copy(from = light_fil, to = file.path(folder, 'Simstrat','light_absorption.dat'))

    # Write absorption file
    absorption_line_1 <- "Time [d] (1.col)    z [m] (1.row)    Absorption [m-1] (rest)"
    absorption_line_2 <- "1"
    absorption_line_3 <- "-1 -1.00"

    if(constantValue){
      start_sim <- get_json_value(sim_par, "Simulation", "Start d")
      end_sim <- get_json_value(sim_par, "Simulation", "End d")
      absorption_line_4 <- paste(start_sim,Kw)
      absorption_line_5 <- paste(end_sim,Kw)

      fileConnection <- file("Simstrat/light_absorption.dat")
      writeLines(c(absorption_line_1,absorption_line_2,absorption_line_3,absorption_line_4,absorption_line_5), fileConnection)
      close(fileConnection)
    }else{

      # Change POSIXct into the Simstrat time format
      Kw_Simstrat <- Kw_file
      reference_year <- get_json_value(sim_par, "Simulation", "Start year")
      Kw_Simstrat$datetime <- as.numeric(difftime(Kw_Simstrat$datetime, as.POSIXct(paste0(reference_year,"-01-01")), units = "days"))

      # Write to light_absorption.dat
      cat(absorption_line_1,absorption_line_2,absorption_line_3, sep="\n", file = "Simstrat/light_absorption.dat")
      cat(apply(Kw_Simstrat,1,paste0, collapse=" "), file = "Simstrat/light_absorption.dat", append = T, sep = "\n")
    }

  }
  
  if("MyLake" %in% model){
    
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists('MyLake')){
      dir.create('MyLake')
    }
    if(!dir.exists('MyLake/output')){
      dir.create('MyLake/output')
    }
    
    if(!constantValue){
      message("MyLake does not accept varying extinction coefficient over time. Average is used instead.")
    }
    
    # Read the MyLake config file from config_file, and write it to the MyLake directory
    mylake_path <- system.file(package="LakeEnsemblR")
    load(file.path(mylake_path, "extdata", "mylake_config_final.Rdata"))
    
    mylake_config[["Bio.par"]][2] <- Kw
    
    save(mylake_config, file = file.path(folder, "MyLake", "mylake_config_final.Rdata"))
    
  }
}
