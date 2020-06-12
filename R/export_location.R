#'Export location settings for each model
#'
#'Exports settings like hypsograph, coordinates and ice settings,
#'  based on the master LakeEnsemblR config file
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'@importFrom stats approx
#'@importFrom gotmtools get_yaml_value input_yaml
#'@importFrom glmtools read_nml set_nml write_nml
#'
#'@export

export_location <- function(config_file, model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
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

  ##-------------Read settings---------------

  # Latitude
  lat <- get_yaml_value(config_file, "location", "latitude")
  # Longitude
  lon <- get_yaml_value(config_file, "location", "longitude")
  # Elevation
  elev <- get_yaml_value(config_file, "location", "elevation")
  # Maximum Depth
  max_depth <- get_yaml_value(config_file, "location", "depth")
  # initial depth
  init_depth <- get_yaml_value(config_file, "location", "init_depth")

  # Read in hypsograph data
  hyp_file <- get_yaml_value(config_file, "location", "hypsograph")
  if(!file.exists(hyp_file)){
    stop(hyp_file, " does not exist. Check filepath in ", config_file)
  }
  hyp <- read.csv(hyp_file)
  # Use ice
  use_ice <- get_yaml_value(config_file, "ice", "use")

  # Output depths
  output_depths <- get_yaml_value(config_file, "output", "depths")

  ##---------------FLake-------------
  if("FLake" %in% model){
    fla_fil <- file.path(folder, get_yaml_value(config_file, "config_files", "FLake"))

    # Calculate mean depth from hypsograph (mdepth = V / SA)
    # Calculate volume from hypsograph - converted to function?
    ## Needs to be double checked!
    bth_area <- hyp$Area_meterSquared
    bth_depth <- hyp$Depth_meter


    top <- min(bth_depth)
    bottom <- max(bth_depth)
    layer_d <- seq(top, bottom, 0.1)
    layer_a <- stats::approx(bth_depth, bth_area, layer_d)$y
    # if init depth is smaller than max depth change hypsograph
    if(init_depth < max_depth) {
      layer_a <- layer_a[(init_depth + (layer_d - max_depth)) >= 0]
      layer_d <- layer_d[(init_depth + (layer_d - max_depth)) >= 0]
      layer_d <- layer_d - min(layer_d)
    }
    vols <- c()
    for(i in 2:length(layer_d)){
      h <- layer_d[i] - layer_d[i - 1]
      cal_v <- (h / 3) * (layer_a[i] + layer_a[i - 1] + sqrt(layer_a[i] * layer_a[i - 1]))
      vols <- c(vols, cal_v)
    }
    vol <- sum(vols)
    mean_depth <- signif((vol / layer_a[1]), 4)
    ##


    input_nml(fla_fil, label = "SIMULATION_PARAMS", key = "h_ML_in", mean_depth)
    input_nml(fla_fil, label = "LAKE_PARAMS", key = "depth_w_lk", mean_depth)
    input_nml(fla_fil, label = "LAKE_PARAMS", key = "latitude_lk", lat)

  }

  ##---------------GLM-------------

  if("GLM" %in% model){
    glm_nml <- file.path(folder, get_yaml_value(config_file, "config_files", "GLM"))

    # Read in nml and input parameters
    nml <- read_nml(glm_nml)

    # Format hypsograph
    glm_hyp <- hyp
    glm_hyp[, 1] <- elev - glm_hyp[, 1] # this doesn't take into account GLM's lake elevation

    # Calculate bsn_len & bsn_wid:
    # Calculate basin dims assume ellipse with width is twice the length
    Ao <- max(glm_hyp[, 2])
    bsn_wid <- sqrt((2 * Ao) / pi)
    bsn_len <- 2 * bsn_wid
    # Can be overwritten by providing values in the model_parameters section of config_file

    # Calculate max number of layers
    min_layer_thick <- get_nml_value(nml, "min_layer_thick")
    max_layers <- round(max_depth / min_layer_thick)


    inp_list <- list("lake_name" = get_yaml_value(config_file, "location", "name"),
                     "latitude" = lat,
                     "longitude" = lon,
                     "lake_depth" = max_depth,
                     "crest_elev" = max((glm_hyp[, 1])),
                     "bsn_vals" = length(glm_hyp[, 1]),
                     "H" = rev(glm_hyp[, 1]),
                     "A" = rev(glm_hyp[, 2]),
                     "bsn_len" = bsn_len,
                     "bsn_wid" = bsn_wid,
                     "max_layers" = max_layers,
                     "max_layer_thick" = 1.0,
                     "lake_depth" = init_depth)

    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    write_nml(nml, glm_nml)
  }

  ##---------------GOTM-------------
  if("GOTM" %in% model){
    got_yaml <- file.path(folder, get_yaml_value(config_file, "config_files", "GOTM"))

    # Write input parameters to got_yaml
    input_yaml(got_yaml, "location", "name", get_yaml_value(config_file, "location", "name"))
    input_yaml(got_yaml, "location", "latitude", lat)
    input_yaml(got_yaml, "location", "longitude", lon)

    # Set max depth
    input_yaml(got_yaml, "location", "depth", max_depth)
    input_yaml(got_yaml, "grid", "nlev", round(max_depth / 0.5))

    # Switch on ice model - MyLake
    # input_yaml(got_yaml, "ice", "model", 2)

    # Create GOTM hypsograph file
    ndeps <- nrow(hyp)
    got_hyp <- hyp
    got_hyp[, 1] <- -got_hyp[, 1]
    # if init depth is lower than max depth change hypsograph
    if(init_depth < max_depth) {
      got_hyp$Depth_meter <- got_hyp$Depth_meter + (max_depth - init_depth)
    }
    colnames(got_hyp) <- c(as.character(ndeps), "2")
    write.table(got_hyp, "GOTM/hypsograph.dat", quote = FALSE,
                sep = "\t", row.names = FALSE, col.names = TRUE)
    input_yaml(got_yaml, "location", "hypsograph", "hypsograph.dat")

  }

  ##---------------Simstrat-------------
  if("Simstrat" %in% model){
    sim_par <- file.path(folder, get_yaml_value(config_file, "config_files", "Simstrat"))

    # Create Simstrat bathymetry
    sim_hyp <- hyp
    sim_hyp[, 1] <- -sim_hyp[, 1]

    # if init depth is lower than max depth change hypsograph
    if(init_depth < max_depth) {
      sim_hyp$Depth_meter <- sim_hyp$Depth_meter + (max_depth - init_depth)
    }
    colnames(sim_hyp) <- c("Depth [m]",	"Area [m^2]")

    write.table(sim_hyp, "Simstrat/hypsograph.dat", quote = FALSE,
                sep = "\t", row.names = FALSE, col.names = TRUE)


    # Input parameters
    input_json(sim_par, "Input", "Grid", round(max_depth / output_depths))
    input_json(sim_par, "Input", "Morphology", '"hypsograph.dat"')
    input_json(sim_par, "ModelParameters", "lat", lat)

    # Turn off ice and snow
    if(use_ice){
      input_json(sim_par, "ModelConfig", "IceModel", 1)
    }else{
      input_json(sim_par, "ModelConfig", "IceModel", 0)
      input_json(sim_par, "ModelConfig", "SnowModel", 0)
    }

    # Calculate default value a_seiche
    # Based on a relation between surface area and calibrated a_seiche
    # in the study of Gaudard et al. (2019). Data used to construct this
    # relation is to be found on https://simstrat.eawag.ch/lakes
    surf_area <- max(sim_hyp[, 2]) / 1000000 # in km2
    a_seiche <- 10^(-2.8591 + 0.7029 * log10(surf_area))
    input_json(sim_par, "ModelParameters", "a_seiche", a_seiche)
  }

  ##---------------MyLake-------------
  if("MyLake" %in% model){
    # Load config file MyLake
    load(get_yaml_value(config_file, "config_files", "MyLake"))

    # wind sheltering coefficient (C_shelter)
    c_shelter <- 1.0 - exp(-0.3 * (hyp$Area_meterSquared[1] * 1e-6))

    mylake_config[["Phys.par"]][5] <- c_shelter
    mylake_config[["Phys.par"]][6] <- lat
    mylake_config[["Phys.par"]][7] <- lon
    mylake_config[["In.Az"]] <- as.matrix(hyp$Area_meterSquared)
    mylake_config[["In.Z"]] <- as.matrix(hyp$Depth_meter)
    if(init_depth < max_depth) {
      myl_hyp <- hyp[(hyp$Depth_meter - max_depth) >= -init_depth, ]
      myl_hyp$Depth_meter <- myl_hyp$Depth_meter - min(myl_hyp$Depth_meter)
      mylake_config[["In.Az"]] <- as.matrix(myl_hyp$Area_meterSquared)
      mylake_config[["In.Z"]] <- as.matrix(myl_hyp$Depth_meter)
    }
    mylake_config[["In.FIM"]] <- matrix(rep(0.92, nrow(hyp)), ncol = 1)
    mylake_config[["In.Chlz.sed"]] <- matrix(rep(196747, nrow(hyp)), ncol = 1)
    mylake_config[["In.TPz.sed"]] <- matrix(rep(756732, nrow(hyp)), ncol = 1)
    mylake_config[["In.DOCz"]] <- matrix(rep(3000, nrow(hyp)), ncol = 1)
    mylake_config[["In.Chlz"]] <- matrix(rep(7, nrow(hyp)), ncol = 1)
    mylake_config[["In.DOPz"]] <- matrix(rep(7, nrow(hyp)), ncol = 1)
    mylake_config[["In.TPz"]] <- matrix(rep(21, nrow(hyp)), ncol = 1)
    mylake_config[["In.Sz"]] <- matrix(rep(0, nrow(hyp)), ncol = 1)
    mylake_config[["In.Cz"]] <- matrix(rep(0, nrow(hyp)), ncol = 1)

    # save lake-specific config file for MyLake
    temp_fil <- gsub(".*/", "", get_yaml_value(config_file, "config_files", "MyLake"))
    save(mylake_config, file = file.path(folder, "MyLake", temp_fil))
  }

  message("export_location complete!")
}
