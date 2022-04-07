#' Export model-specific parameters
#'
#' Exports model-specific parameters that are specified in the model_parameters
#'   section of the master config file.
#'
#' @inheritParams export_config
#' @keywords methods
#' @examples
#'
#'
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' @importFrom gotmtools read_yaml get_yaml_value input_yaml input_nml
#' @importFrom glmtools read_nml get_nml_value
#'
#' @export

export_model_parameters <- function(config_file,
                              model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
                              folder = ".") {

  # Set working directory
  oldwd <- getwd()
  setwd(folder)

  # this way if the function exits for any reason, success or failure, these are reset:
  on.exit({
    setwd(oldwd)
  })

  if(!file.exists(config_file)) {
    stop(config_file, " does not exist. Make sure your file path is correct")
  } else {
    yaml <- gotmtools::read_yaml(config_file)
  }

  # check model input
  model <- check_models(model)

  if(yaml$restart$use & "GLM" %in% model) {

    glm_nml <- file.path(folder, gotmtools::get_yaml_value(yaml, "config_files", "GLM"))

    # Read in nml and input parameters
    nml <- glmtools::read_nml(glm_nml)
    z_out <- glmtools::get_nml_value(nml, "the_depths")

    glm_outfile <- file.path(folder, "GLM", nml$output$out_dir,
                             paste0(nml$output$out_fn, ".nc"))
    if(!file.exists(glm_outfile)) {
      stop("File: '", glm_outfile, "' does not exist. You need to run the model in normal mode first before switching on the restart function.")
    }

    glm_nc <- ncdf4::nc_open(glm_outfile)
    tallest_layer <- ncdf4::ncvar_get(glm_nc, "NS")
    final_time_step <- length(tallest_layer)
    tallest_layer <- tallest_layer[final_time_step]
    heights <- matrix(ncdf4::ncvar_get(glm_nc, "z"), ncol = final_time_step)
    heights_surf <- heights[tallest_layer, final_time_step]
    heights <- heights[1:tallest_layer, final_time_step]

    snow_thickness <-  matrix(ncdf4::ncvar_get(glm_nc, "hsnow"), ncol = final_time_step)[final_time_step]
    white_ice_thickness <- matrix(ncdf4::ncvar_get(glm_nc, "hwice"), ncol = final_time_step)[final_time_step]
    blue_ice_thickness <- matrix(ncdf4::ncvar_get(glm_nc, "hice"), ncol = final_time_step)[final_time_step]

    restart_variables <- ncdf4::ncvar_get(glm_nc, "restart_variables")
    avg_surf_temp <- ncdf4::ncvar_get(glm_nc, "avg_surf_temp")[final_time_step]

    glm_temps <- matrix(ncdf4::ncvar_get(glm_nc, "temp"), ncol = final_time_step)[1:tallest_layer,final_time_step]
    salt <- matrix(ncdf4::ncvar_get(glm_nc, "salt"), ncol = final_time_step)[1:tallest_layer, final_time_step]

    the_temps <- approx(heights, glm_temps, z_out, rule = 2)$y
    the_sals <- approx(heights, salt, z_out, rule = 2)$y

    ncdf4::nc_close(glm_nc)

    yaml$model_parameters$GLM$`init_profiles/the_temps` <- round(the_temps, 2)
    yaml$model_parameters$GLM$`init_profiles/the_sals` <- round(the_sals, 2)
    yaml$model_parameters$GLM$`init_profiles/snow_thickness` <- round(snow_thickness, 2)
    yaml$model_parameters$GLM$`init_profiles/white_ice_thickness` <- round(white_ice_thickness, 2)
    yaml$model_parameters$GLM$`init_profiles/blue_ice_thickness` <- round(blue_ice_thickness, 2)
    yaml$model_parameters$GLM$`init_profiles/avg_surf_temp` <- round(avg_surf_temp, 2)
    yaml$model_parameters$GLM$`init_profiles/restart_variables` <- signif(restart_variables, 5)

  } else if(yaml$restart$use & "GOTM" %in% model) {
    restart_file <- file.path(folder, "GOTM", "restart.nc")
    if(!file.exists(restart_file)) {
      stop("File: '", restart_file, "' does not exist. You need to run the model in normal mode first before switching on the restart function.")
    }
    yaml$model_parameters$GOTM$`restart/load` <- TRUE
  } else if(yaml$restart$use & "Simstrat" %in% model) {
    restart_file <- file.path(folder, "Simstrat", "output", "simulation-snapshot.dat")
    if(!file.exists(restart_file)) {
      stop("File: '", restart_file, "' does not exist. You need to run the model in normal mode first before switching on the restart function.")
    }
    # yaml$model_parameters$Simstrat$`Simulation/Continue from last snapshot` <- TRUE
  } else {
    yaml$model_parameters$GOTM$`restart/load` <- FALSE
    # yaml$model_parameters$Simstrat$`Simulation/Continue from last snapshot` <- FALSE
  }

  input_config_value(model, yaml, folder)

  message("export_model_parameters complete!")
}
