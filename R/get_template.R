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

get_template <- function(template = NULL, folder=".", overwrite = FALSE){
  
  template_folder <- system.file("extdata", package = "LakeEnsemblR")
  
  # Names and subfolders of all the templates
  name_config <- "feeagh/LakeEnsemblR.yaml"
  name_flake <- "flake_template.nml"
  name_glm <- "glm3_template.nml"
  name_gotm <- "gotm_template.yaml"
  name_simstrat <- "simstrat_template.par"
  name_mylake <- "mylake_config_template.Rdata"
  name_init_temp_profile <- "LakeEnsemblR_init_temp_profile_template.csv"
  name_Kw <- "LakeEnsemblR_Kw_template.csv"
  name_inflow <- "feeagh/LakeEnsemblR_inflow_standard.csv"
  name_hypsograph <- "feeagh/LakeEnsemblR_bathymetry_standard.csv"
  name_ice_height <- "feeagh/LakeEnsemblR_ice-height_standard"
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
                 name_hypsograph,
                 name_ice_height,
                 name_meteo,
                 name_temp_obs)
  
  if(is.null(template)){
    message("You need to specify what you want a template for. \nOptions are:\n",
            "all", "\nLakeEnsemblR_config", "\nFLake_config", "\nGLM_config",
            "\nGOTM_config", "\nSimstrat_config", "\nMyLake_config",
            "\nInitial temperature profile", "\nLight extinction", "\nInflow",
            "\nHypsograph", "\nIce height", "\nMeteo", "\nTemperature observations")
  }else if(template == "LakeEnsemblR_config"){
    file.copy(file.path(template_folder, name_config),
              file.path(folder, name_config),
              overwrite = overwrite)
  }else if(template == "FLake_config"){
    file.copy(file.path(template_folder, name_flake),
              file.path(folder, name_flake),
              overwrite = overwrite)
  }else if(template == "GLM_config"){
    file.copy(file.path(template_folder, name_glm),
              file.path(folder, name_glm),
              overwrite = overwrite)
  }else if(template == "GOTM_config"){
    file.copy(file.path(template_folder, name_gotm),
              file.path(folder, name_gotm),
              overwrite = overwrite)
  }else if(template == "Simstrat_config"){
    file.copy(file.path(template_folder, name_simstrat),
              file.path(folder, name_simstrat),
              overwrite = overwrite)
  }else if(template == "MyLake_config"){
    file.copy(file.path(template_folder, name_mylake),
              file.path(folder, name_mylake),
              overwrite = overwrite)
  }else if(template == "Initial temperature profile"){
    file.copy(file.path(template_folder, name_init_temp_profile),
              file.path(folder, name_init_temp_profile),
              overwrite = overwrite)
  }else if(template == "Light extinction"){
    file.copy(file.path(template_folder, name_Kw),
              file.path(folder, name_Kw),
              overwrite = overwrite)
  }else if(template == "Inflow"){
    file.copy(file.path(template_folder, name_inflow),
              file.path(folder, name_inflow),
              overwrite = overwrite)
  }else if(template == "Hypsograph"){
    file.copy(file.path(template_folder, name_hypsograph),
              file.path(folder, name_hypsograph),
              overwrite = overwrite)
  }else if(template == "Ice height"){
    file.copy(file.path(template_folder, name_ice_height),
              file.path(folder, name_ice_height),
              overwrite = overwrite)
  }else if(template == "Meteo"){
    file.copy(file.path(template_folder, name_meteo),
              file.path(folder, name_meteo),
              overwrite = overwrite)
  }else if(template == "Temperature observations"){
    file.copy(file.path(template_folder, name_temp_obs),
              file.path(folder, name_temp_obs),
              overwrite = overwrite)
  }else if(template == "all"){
    for(i in all_names){
      file.copy(file.path(template_folder, i),
                file.path(folder, i),
                overwrite = overwrite)
    }
  }else{
    message("The name of the template was not recognized.")
  }
}
