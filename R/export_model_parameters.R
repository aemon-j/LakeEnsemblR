#'Export model-specific parameters
#'
#'Exports model-specific parameters that are specified in the model_parameters
#'  section of the master config file.
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("FLake", "GLM, "GOTM", "Simstrat", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'
#'@importFrom configr read.config
#'@importFrom gotmtools get_yaml_value input_yaml input_nml
#'
#'@export

export_model_parameters <- function(config_file,
                              model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                              folder = "."){
  # Set working directory
  oldwd <- getwd()
  setwd(folder)
  
  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
  })
  
  # Read config_file with configr
  master_config <- configr::read.config(file.path(folder, config_file))
  
  # Now for each model:
  # - Check if model_parameters -> Model exists or is not empty (if not, continue)
  # - If it does, check if the model config file can be found, if not, warn and continue
  # - Loop through all variables in the master config files and write replacement
  #   value to the config file specified under config_files
  
  # Ideally, all this in a loop (but need if statement as for each model it's
  # different how to write to a model)
  # Place reference to this function in export_config.R
  # The hard-coded reference in export_config for GLM bsn_width, etc. can then be removed
  
  # Try to turn this into a loop, then later maybe into apply
  for(i in model){
    # Only continue if model-specific parameters are specified for this model
    if(is.null(master_config[["model_parameters"]][[i]])) next
    
    model_config <- file.path(folder, master_config[["config_files"]][[i]])
    
    # Loop through the specified parameters and write them to the config file
    for(j in names(master_config[["model_parameters"]][[i]])){
      spl <- strsplit(j, "/")
      if(length(spl[[1]]) == 1){
        label <- NULL
        key <- spl[[1]]
      }else{
        label <- spl[[1]][1]
        key <- spl[[1]][2]
      }
      
      tryCatch({input_config_value(model = i,
                                   file = model_config,
                                   label = label,
                                   key = key,
                                   value = master_config[["model_parameters"]][[i]][[j]])
      },
      error = function(e){
        return_val <- "Error"
        warning(paste("Could not replace the value of", j,
                      "in the", i, "configuration file. "))
      })
    }
  }
}
