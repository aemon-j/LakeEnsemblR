#'Creates directories for each model
#'
#'Creates directories with file setups for each model, based on the master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'
#'@export

export_dirs <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                          folder = "."){
  # Set working directory
  oldwd <- getwd()
  setwd(folder)
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
  })
  
  # check model input
  model <- check_models(model)
  
##---------------FLake-------------
  if("FLake" %in% model){
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("FLake/output")){
      dir.create("FLake/output", recursive = TRUE)
    }
    
    # Read the FLake config file from config_file, and write it to the FLake directory
    temp_fil <- get_yaml_value(config_file, "config_files", "FLake")
    if(!file.exists(temp_fil)){
      get_template("FLake_config", folder = folder, filename = temp_fil)
    }
  }
  
##---------------GLM-------------
  if("GLM" %in% model){
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("GLM/output")){
      dir.create("GLM/output", recursive = TRUE)
    }
    
    # Read the GLM config file from config_file, and write it to the GLM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "GLM")
    
    if(!file.exists(temp_fil)){
      get_template("GLM_config", folder = folder, filename = temp_fil)
    }
  }
  
##---------------GOTM-------------
  if("GOTM" %in% model){
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("GOTM/output")){
      dir.create("GOTM/output", recursive = TRUE)
    }
    
    # Read the GOTM config file from config_file, and write it to the GOTM directory
    temp_fil <- get_yaml_value(config_file, "config_files", "GOTM")
    if(!file.exists(temp_fil)){
      get_template("GOTM_config", folder = folder, filename = temp_fil)
    }
  }
  
##---------------Simstrat-------------
  if("Simstrat" %in% model){
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("Simstrat/output")){
      dir.create("Simstrat/output", recursive = TRUE)
    }
    
    # Read the Simstrat config file from config_file, and write it to the Simstrat directory
    temp_fil <- get_yaml_value(config_file, "config_files", "Simstrat")
    if(!file.exists(temp_fil)){
      get_template("Simstrat_config", folder = folder, filename = temp_fil)
    }
  }
  
##---------------MyLake-------------
  if("MyLake" %in% model){
    # Create directory and output directory, if they do not yet exist
    if(!dir.exists("MyLake")){
      dir.create("MyLake")
    }
    
    # Load config file MyLake
    temp_fil <- get_yaml_value(config_file, "config_files", "MyLake")
    if(!file.exists(temp_fil)){
      get_template("MyLake_config", folder = folder, filename = temp_fil)
      
      # Load template config file from extdata
      load(file.path(folder, temp_fil))
    }
  }
  
  message("export_dirs complete!")
}
