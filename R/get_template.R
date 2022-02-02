#' Copies LakeEnsemblR template from package to specified folder
#'
#' Copies LakeEnsemblR template from package to specified folder to be used
#'    as example for setting up a simulation.
#'
#' @param template string; name of file that you want to get the template for.
#'    In case it is NULL, all potential options are given. "All" returns all templates.
#' @param folder filepath; filepath where to copy the templates to
#' @param overwrite boolean; overwrite existing files
#' 
#' @export

get_template <- function(template = NULL, folder = ".", overwrite = FALSE){
  
  template_folder <- system.file("extdata", package = "LakeEnsemblR")
  
  # Names and subfolders of all the templates
  name_config <- "feeagh/LakeEnsemblR.yaml"
  name_flake <- "flake.nml" # Name in FLakeR
  name_glm <- "glm3.nml" # Name in GLM3r
  name_gotm <- "gotm.yaml" # Name in GOTMr
  name_simstrat <- "simstrat.par" # Name in SimstratR
  name_mylake <- "mylake.Rdata" # Name in MyLakeR
  name_init_temp_profile <- "LakeEnsemblR_init_temp_profile_template.csv"
  name_Kw <- "LakeEnsemblR_Kw_template.csv"
  name_inflow <- "feeagh/LakeEnsemblR_inflow_standard.csv"
  name_outflow <- "feeagh/LakeEnsemblR_outflow_standard.csv"
  name_hypsograph <- "feeagh/LakeEnsemblR_bathymetry_standard.csv"
  name_ice_height <- "feeagh/LakeEnsemblR_ice-height_standard.csv"
  name_meteo <- "feeagh/LakeEnsemblR_meteo_standard.csv"
  name_temp_obs <- "feeagh/LakeEnsemblR_wtemp_profile_standard.csv"
  
  all_names <- c(name_config,
                 name_flake,
                 name_glm,
                 name_gotm,
                 name_simstrat,
                 name_mylake,
                 name_init_temp_profile,
                 name_Kw,
                 name_inflow,
                 name_outflow,
                 name_hypsograph,
                 name_ice_height,
                 name_meteo,
                 name_temp_obs)
  
  if(is.null(template)){
    message("You need to specify what you want a template for. \nOptions are:\n",
            "all", "\nLakeEnsemblR_config", "\nFLake_config", "\nGLM_config",
            "\nGOTM_config", "\nSimstrat_config", "\nMyLake_config",
            "\nInitial temperature profile", "\nLight extinction", "\nInflow",
            "\nOutflow", "\nHypsograph", "\nIce height", "\nMeteo",
            "\nTemperature observations")
  }else if(template == "LakeEnsemblR_config"){
    file.copy(file.path(template_folder, name_config),
              file.path(folder, basename(name_config)),
              overwrite = overwrite)
  }else if(template == "FLake_config"){
    if("FLakeR" %in% rownames(installed.packages())){
      # Get the file
      flake_folder <- system.file("extdata", package = "FLakeR")
      file.copy(file.path(flake_folder, name_flake),
                file.path(folder, basename(name_flake)),
                overwrite = overwrite)
    }else{
      message("To retrieve the FLake config file, you need to install ",
              "the R package 'FLakeR'.")
    }
    
  }else if(template == "GLM_config"){
    if("GLM3r" %in% rownames(installed.packages())){
      # Get the file
      glm_folder <- system.file("extdata", package = "GLM3r")
      file.copy(file.path(glm_folder, name_glm),
                file.path(folder, basename(name_glm)),
                overwrite = overwrite)
    }else{
      message("To retrieve the GLM config file, you need to install ",
              "the R package 'GLM3r'.")
    }
  }else if(template == "GOTM_config"){
    if("GOTMr" %in% rownames(installed.packages())){
      # Get the file
      gotm_folder <- system.file("extdata", package = "GOTMr")
      file.copy(file.path(gotm_folder, name_gotm),
                file.path(folder, basename(name_gotm)),
                overwrite = overwrite)
    }else{
      message("To retrieve the GOTM config file, you need to install ",
              "the R package 'GOTMr'.")
    }
  }else if(template == "Simstrat_config"){
    if("SimstratR" %in% rownames(installed.packages())){
      # Get the file
      simstrat_folder <- system.file("extdata", package = "SimstratR")
      file.copy(file.path(simstrat_folder, name_simstrat),
                file.path(folder, basename(name_simstrat)),
                overwrite = overwrite)
    }else{
      message("To retrieve the Simstrat config file, you need to install ",
              "the R package 'SimstratR'.")
    }
  }else if(template == "MyLake_config"){
    if("MyLakeR" %in% rownames(installed.packages())){
      # Get the file
      mylake_folder <- system.file("extdata", package = "MyLakeR")
      file.copy(file.path(mylake_folder, name_mylake),
                file.path(folder, basename(name_mylake)),
                overwrite = overwrite)
    }else{
      message("To retrieve the MyLake config file, you need to install ",
              "the R package 'MyLakeR'.")
    }
  }else if(template == "Initial temperature profile"){
    file.copy(file.path(template_folder, name_init_temp_profile),
              file.path(folder, basename(name_init_temp_profile)),
              overwrite = overwrite)
  }else if(template == "Light extinction"){
    file.copy(file.path(template_folder, name_Kw),
              file.path(folder, basename(name_Kw)),
              overwrite = overwrite)
  }else if(template == "Inflow"){
    file.copy(file.path(template_folder, name_inflow),
              file.path(folder, basename(name_inflow)),
              overwrite = overwrite)
  }else if(template == "Outflow"){
    file.copy(file.path(template_folder, name_outflow),
              file.path(folder, basename(name_outflow)),
              overwrite = overwrite)
  }else if(template == "Hypsograph"){
    file.copy(file.path(template_folder, name_hypsograph),
              file.path(folder, basename(name_hypsograph)),
              overwrite = overwrite)
  }else if(template == "Ice height"){
    file.copy(file.path(template_folder, name_ice_height),
              file.path(folder, basename(name_ice_height)),
              overwrite = overwrite)
  }else if(template == "Meteo"){
    file.copy(file.path(template_folder, name_meteo),
              file.path(folder, basename(name_meteo)),
              overwrite = overwrite)
  }else if(template == "Temperature observations"){
    file.copy(file.path(template_folder, name_temp_obs),
              file.path(folder, basename(name_temp_obs)),
              overwrite = overwrite)
  }else if(template == "all"){
    for(i in all_names){
      if(i == "flake.nml" & "FLakeR" %in% rownames(installed.packages())){
        the_folder <- system.file("extdata", package = "FLakeR")
      }else if(i == "glm3.nml" & "GLM3r" %in% rownames(installed.packages())){
        the_folder <- system.file("extdata", package = "GLM3r")
      }else if(i == "gotm.yaml" & "GOTMr" %in% rownames(installed.packages())){
        the_folder <- system.file("extdata", package = "GOTMr")
      }else if(i == "simstrat.par" & "SimstratR" %in% rownames(installed.packages())){
        the_folder <- system.file("extdata", package = "SimstratR")
      }else if(i == "mylake.Rdata" & "MyLakeR" %in% rownames(installed.packages())){
        the_folder <- system.file("extdata", package = "MyLakeR")
      }else{
        the_folder <- template_folder
      }
      
      file.copy(file.path(the_folder, i),
                file.path(folder, basename(i)),
                overwrite = overwrite)
    }
  }else{
    message("The name of the template was not recognized.")
  }
}
