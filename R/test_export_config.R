setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../data/feeagh')

# Install packages
#install.packages('devtools')
devtools::install_github('GLEON/GLM3r')
devtools::install_github('hdugan/glmtools')
devtools::install_github('aemon-j/FLakeR')
devtools::install_github('aemon-j/GOTMr')
devtools::install_github('aemon-j/gotmtools')
devtools::install_github('aemon-j/SimstratR')


# Load libraries
library(GOTMr);library(SimstratR);library(GLM3r);library(FLakeR);library(gotmtools);library(glmtools)
library(lubridate);library(plyr);library(ncdf4)

# Load functions
source('../../R/export_config.R')
source('../../R/export_meteo.R')
source('../../R/export_init_cond.R')
source('../../R/run_ensemble.R')

# Load helper functions
source('../../R/helper_functions/input_json.R') # Potential function for 'simstrattools'
source('../../R/helper_functions/get_json_value.R') # Potential function for 'simstrattools'
source('../../R/helper_functions/input_nml.R') # This versions preserves comments in the nml
source('../../R/helper_functions/streams_switch.R') # Will be added to gotmtools in the future
source('../../R/helper_functions/get_yaml_value.R') # Will be added to gotmtools in the future
source('../../R/helper_functions/get_wtemp_df.R') # Function for flaketools?

# 1. Example - creates directories with all model setups
export_config(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.', hypsograph_file = 'LakeEnsemblR_bathymetry_standard.csv', lat = 53, lon = -9, name = 'feeagh', Kw = 1.5)

# 2. Create meteo driver files
export_meteo(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), meteo_file = 'LakeEnsemblR_meteo_standard.csv')

# 3. Create initial conditions
export_init_cond(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), wtemp_file = 'LakeEnsemblR_wtemp_profile_standard.csv', date = '1979-01-01 00:00:00', tprof_file = 'HOLDER.dat', month = 1, ndeps = 2, print = TRUE)

# 4. Run ensemble lake models
run_ensemble(return_list = F, make_output = TRUE)

# Plot model output
gotmtools::plot_wtemp('GOTM/output/output.nc')
glmtools::plot_var('GLM/output/output.nc', var_name = 'temp')  

