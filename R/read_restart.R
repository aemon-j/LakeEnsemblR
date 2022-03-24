#' Read model restart variables
#'
#' Read in model restart variables in a list format
#'
#'@param folder folder
#'@param model string; denoting which model you are extracting from. Can be 'GLM', 'GOTM', 'Simstrat'
#'@examples
#'
#'
#'@importFrom ncdf4 nc_open ncvar_get nc_close ncatt_get
#'@importFrom glmtools read_nml get_nml_value write_nml
#'
#'@export

read_restart <- function(folder = ".", model) {

  # GLM ----
  if(model == "GLM") {

    nml_file <- file.path(folder, model, "glm3.nml")
    if(!file.exists(nml_file)) {
      stop("No 'glm3.nml' file in ", folder)
    }
    nml <- glmtools::read_nml(nml_file)
    out_dir <- glmtools::get_nml_value(nml, "out_dir")
    the_depths <- glmtools::get_nml_value(nml, "the_depths")

    outfile <- file.path(folder, model, out_dir, "output.nc")
    if(!file.exists(outfile)) {
      stop("No 'output.nc' file in ", file.path(folder, model, out_dir))
    }
    glm_nc <- ncdf4::nc_open(outfile)
    on.exit({
      ncdf4::nc_close(glm_nc)
    })

    tallest_layer <- ncdf4::ncvar_get(glm_nc, "NS")
    final_time_step <- length(tallest_layer)
    tallest_layer <- tallest_layer[final_time_step]
    heights <- matrix(ncdf4::ncvar_get(glm_nc, "z"), ncol = final_time_step)
    lake_depth <- heights[tallest_layer, final_time_step]
    z <- heights[1:tallest_layer, final_time_step]
    the_temps <- matrix(ncdf4::ncvar_get(glm_nc, "temp"),
                        ncol = final_time_step)[tallest_layer:1,final_time_step]
    the_temps <- approx(z, the_temps, the_depths, rule = 2)$y
    the_sals <- matrix(ncdf4::ncvar_get(glm_nc, "salt"),
                       ncol = final_time_step)[tallest_layer:1, final_time_step]
    the_sals <- approx(z, the_sals, the_depths, rule = 2)$y



    snow_thickness <-  matrix(ncdf4::ncvar_get(glm_nc, "hsnow"), ncol = final_time_step)[final_time_step]
    white_ice_thickness <- matrix(ncdf4::ncvar_get(glm_nc, "hwice"), ncol = final_time_step)[final_time_step]
    blue_ice_thickness <- matrix(ncdf4::ncvar_get(glm_nc, "hice"), ncol = final_time_step)[final_time_step]
    avg_surf_temp <- matrix(ncdf4::ncvar_get(glm_nc, "avg_surf_temp"), ncol = final_time_step)[final_time_step]
    restart_variables <- ncdf4::ncvar_get(glm_nc, "restart_variables")

    restart_list = list(lake_depth = round(lake_depth, 2),
                        the_depths = round(the_depths, 3),
                        the_temps = round(the_temps, 3),
                        the_sals = round(the_sals, 4),
                        snow_thickness = snow_thickness,
                        white_ice_thickness = white_ice_thickness,
                        blue_ice_thickness = blue_ice_thickness,
                        avg_surf_temp = avg_surf_temp,
                        restart_variables = restart_variables)

  }

  # GOTM ----
  if(model == "GOTM") {

    restart_nc_file <- file.path(folder, model, "restart.nc")

    z_var_names <- list("z", "temp", "salt", "u", "uo", "v", "vo", "xP", "h", "ho")
    zi_var_names <- list("tke", "zi", "tkeo", "eps", "num", "nuh", "nus")


    nc <- ncdf4::nc_open(restart_nc_file)
    on.exit({
      ncdf4::nc_close(nc)
    })


    lat <- ncdf4::ncvar_get(nc, "lat")
    lon <- ncdf4::ncvar_get(nc, "lon")
    time <- ncdf4::ncvar_get(nc, "time")
    z <- ncdf4::ncvar_get(nc, "z")
    zi <- ncdf4::ncvar_get(nc, "zi")

    # z_att <- sapply(z_vars, function(x) ncatt_get(nc, x), USE.NAMES = TRUE, simplify = "array")

    var_list <- nc$var

    lat_dim <- nc$dim$lat
    lon_dim <- nc$dim$lon
    time_dim <- nc$dim$time
    z_dim <- nc$dim$z
    zi_dim <- nc$dim$zi

    lat_att <- ncdf4::ncatt_get(nc, "lat")
    lon_att <- ncdf4::ncatt_get(nc, "lon")
    time_att <- ncdf4::ncatt_get(nc, "time")

    z_att <- setNames(lapply(z_var_names, function(x) ncdf4::ncatt_get(nc, x)), z_var_names)
    z_vars <- setNames(lapply(z_var_names, function(x) ncdf4::ncvar_get(nc, x)), z_var_names)
    zi_att <- setNames(lapply(zi_var_names, function(x) ncdf4::ncatt_get(nc, x)), zi_var_names)
    zi_vars <- setNames(lapply(zi_var_names, function(x) ncdf4::ncvar_get(nc, x)), zi_var_names)

    restart_list <- list(# lat = lat,
                         # lon = lon,
                         # time = time,
                         z_vars = z_vars,
                         zi_vars = zi_vars
                         # nc_atts = list(lat = lat_att, lon = lon_att, time = time_att, z = z_att, zi = zi_att),
                         # dims = list(lat = lat_dim, lon = lon_dim, time = time_dim, z = z_dim, zi = zi_dim)
                         )

  }

  # Simstrat ----
  if(model == "Simstrat") {

    par_file <- file.path(folder, model, "simstrat.par")
    if(!file.exists(par_file)) {
      stop("No 'simstrat.par' file in ", folder)
    }
    out_dir <- get_json_value(par_file, "Output", "Path")
    output_folder <- file.path(folder, model, out_dir)
    init_cond_file <- file.path(output_folder, "save_end_conditions.dat")
    if(!file.exists(init_cond_file)) {
      stop("No 'save_end_conditions.dat' file in ", output_folder)
    }
    cnams <- read.csv(init_cond_file, header = FALSE, nrows = 1)
    init_cond <- read.csv(init_cond_file, header = FALSE, skip = 1)

    init_cond2_file <- file.path(output_folder, "save_end_conditions2.dat")
    if(!file.exists(init_cond2_file)) {
      stop("No 'save_end_conditions2.dat' file in ", output_folder)
    }
    init_cond2 <- read.csv(init_cond2_file, header = TRUE)

    restart_list = list(zi = init_cond[[1]],
                        u = init_cond[[2]],
                        v = init_cond[[3]],
                        temp = init_cond[[4]],
                        S = init_cond[[5]],
                        k = init_cond[[6]],
                        eps = init_cond[[7]],
                        num = init_cond[[8]],
                        nuh = init_cond[[9]],
                        seicheE = init_cond2[[1]],
                        b_ice = init_cond2[[2]],
                        w_ice = init_cond2[[3]],
                        snow = init_cond2[[4]])
  }

  return(restart_list)
}
