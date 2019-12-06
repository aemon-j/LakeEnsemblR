#'@title Export model configuration setups
#'
#'@description
#'Create directory with file setups for each model
#'
#'@param model vector; model to export configuration file. Options include c('GOTM', 'GLM', 'Simstrat', 'FLake')
#'@param folder folder
#'@param hypsograph_file filepath; to file with LakeEnsemblR
#'@param lat integer; Latitude of the lake
#'@param lon integer; Longitude of the lake
#'@param name character; Name of lake
#'@param max_depth numeric; Value of max depth if different to hypsograph file
#'@param Kw numeric; Value of light extinction
#'@keywords methods
#'@author
#'Tadhg Moore
#'@examples
#'export_config(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.', hypsograph_file = 'Feeagh_hypsometry.csv', lat = 53, lon = -9, name = 'feeagh, Kw = 1.5)
#'
#'@importFrom stats approx
#'
#'@export

export_config <- function(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.', hypsograph_file, lat, lon, name, max_depth = NULL, Kw){
  
  # Set working directory
  oldwd <- getwd()
  setwd('.')
  
  # Read in hypsograph data
  hyp <- read.csv(hypsograph_file)
  
  # Set Max Depth
  if(is.null(max_depth)){
    max_depth = max(hyp$DEPTH)
  }
  
  if("GOTM" %in% model){
    dir.create('GOTM') # Create directory 
    temp_fil <- system.file('extdata/gotm.yaml', package= 'GOTMr')
    out_fil <- system.file('extdata/output.yaml', package= 'GOTMr')
    file.copy(from = temp_fil, to = 'GOTM')
    file.copy(from = out_fil, to = 'GOTM')
    got_yaml <- 'GOTM/gotm.yaml'
    
    gotmtools::input_yaml(got_yaml, 'location', 'name', name)
    gotmtools::input_yaml(got_yaml, 'location', 'latitude', lat)
    gotmtools::input_yaml(got_yaml, 'location', 'longitude', lon)
    
    # Set max depth
    gotmtools::input_yaml(got_yaml, 'location', 'depth', max_depth)
    
    
    # Create GOTM hypsograph file
    ndeps <- nrow(hyp)
    got_hyp <- hyp[,c('DEPTH', 'BATHYMETRY_AREA')]
    got_hyp[,1] <- -got_hyp[,1]
    colnames(got_hyp) <- c(as.character(ndeps), '2')
    write.table(got_hyp, 'GOTM/hypsograph.dat', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
    
    # Input light extinction data [To be continued...]
    
    
    message('GOTM configuration complete!')
  }
  
  if("GLM" %in% model){
    dir.create('GLM') # Create directory 
    temp_fil <- system.file('extdata/glm3.nml', package= 'GLM3r')
    file.copy(from = temp_fil, to = 'GLM')
    
    # Format hpsograph 
    glm_hyp <- hyp[,c('DEPTH', 'BATHYMETRY_AREA')]
    glm_hyp[,1] <- glm_hyp[nrow(glm_hyp),1] - glm_hyp[,1]
    
    # Read in nml and input parameters
    glm_nml <- 'GLM/glm3.nml'
    nml <- glmtools::read_nml(glm_nml)
    inp_list <- list('lake_name' = name, 'latitude' = lat, 'longitude' = lon, 'lake_depth' = max_depth, 'Kw' = Kw, 'H' = rev(glm_hyp[,1]), 'A' = rev(glm_hyp[,2]))
    nml <- glmtools::set_nml(nml, arg_list = inp_list)
    glmtools::write_nml(nml, 'GLM/glm3.nml')
    
    
    message('GLM configuration complete!')
    
  }
  
  if("Simstrat" %in% model){
    dir.create('Simstrat') # Create directory 
    temp_fil <- system.file('extdata/langtjern.par', package= 'SimstratR')
    file.copy(from = temp_fil, to = paste0('Simstrat/',name,'.par'))
    sim_par <- paste0('Simstrat/',name,'.par')
    
    # Create Simstrat bathymetry
    sim_hyp <- hyp[,c('DEPTH', 'BATHYMETRY_AREA')]
    sim_hyp[,1] <- -sim_hyp[,1]
    colnames(sim_hyp) <- c('Depth [m]',	'Area [m^2]')
    write.table(sim_hyp, 'Simstrat/hypsograph.dat', quote = FALSE, sep = '\t', row.names = FALSE, col.names = TRUE)
    
    # Input parameters
    input_json(sim_par, 'Input', 'Morphology', '"hypsograph.dat"')
    input_json(sim_par, 'Input', 'Morphology', '"hypsograph.dat"')
    input_json(sim_par, 'ModelParameters', 'lat', lat)
    
    message('Simstrat configuration complete!')
    
  }
  
  if("FLake" %in% model){
    dir.create('FLake') # Create directory 
    temp_fil <- system.file('extdata/Heiligensee80-96.nml', package= 'FLakeR')
    file.copy(from = temp_fil, to = paste0('FLake/',name,'.nml'))
    fla_fil <- paste0('FLake/',name,'.nml')
    
    # Calculate mean depth from hypsograph
    #   mdepth = V / SA
    
    # Calculator to volume from hypsograph - converted to function?
    bthA = hyp$BATHYMETRY_AREA
    bthD = hyp$DEPTH
    top = min(bthD)
    bottom = max(bthD)
    layerD <- seq(top, bottom, 0.1)
    layerA <- stats::approx(bthD, bthA, layerD)$y
    vols <- c()
    for(i in 2:length(layerD)){
      h = layerD[i] - layerD[i-1]
      cal_v <- (h/3)*(layerA[i] + layerA[i-1] + sqrt(layerA[i] * layerA[i-1]))
      vols <- c(vols, cal_v)
    }
    vol = sum(vols)
    mean_depth = vol / bthA[1]
    
    # Input parameters
    input_nml(fla_fil, label = 'LAKE_PARAMS', key = 'depth_w_lk', mean_depth)
    input_nml(fla_fil, label = 'LAKE_PARAMS', key = 'latitude_lk', lat)
    input_nml(fla_fil, label = 'TRANSPARENCY', key = 'extincoef_optic', lat)
    
    message('FLake configuration complete!')
    
  }
  
  
  setwd(oldwd)
}
