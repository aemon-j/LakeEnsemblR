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
  
  # check model input
  model <- check_models(model)
  
  # Read config_file with configr
  master_config <- configr::read.config(file.path(folder, config_file))
  
  for(i in model){
    # Only continue if model-specific parameters are specified for this model
    if(is.null(master_config[["model_parameters"]][[i]])) next
    
    model_config <- file.path(folder, master_config[["config_files"]][[i]])
    
    # Loop through the specified parameters and write them to the config file
    for(j in names(master_config[["model_parameters"]][[i]])){
      spl <- strsplit(j, "/")
      if(i == "GOTM") {
        arg_list <- list(model = i, file = model_config, label = NULL, key = NULL, value = master_config[["model_parameters"]][[i]][[j]])
        for( k in seq_len(length(spl[[1]]))) {
          arg_list[[length(arg_list) + 1]] <- spl[[1]][k]
        }
        tryCatch({
          do.call(input_config_value, args = arg_list)
        },
        error = function(e){
          return_val <- "Error"
          warning(paste("Could not replace the value of", j,
                        "in the", i, "configuration file. "))
        })
      } else {
        if(length(spl[[1]]) == 1){
          label <- NULL
          key <- spl[[1]]
        }else{
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
    }
  message("export_model_parameters complete!")
}
