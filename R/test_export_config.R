setwd('...\\LakeEnsemblR\\data\\feeagh')

#library
library(GOTMr);library(SimstratR);library(GLM3r);library(FLakeR);library(gotmtools);library(glmtools)

# Load functions
source('../../R/export_config.R')
source('../../R/export_meteo.R')
source('../../R/export_init_cond.R')
source('../../R/run_ensemble.R')
source('../../R/input_json.R')
source('../../R/input_nml.R') # This versions preserves comments in the nml
source('../../R/streams_switch.R') # Will be added to gotmtools in the future

# 1. Example - creates directories with all model setups
export_config(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.', hypsograph_file = 'Feeagh_hypsometry.csv', lat = 53, lon = -9, name = 'feeagh', Kw = 1.5)

# 2. Create meteo driver files
export_meteo(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), meteo_file = 'LakeEnsemblR_meteo_standard.csv')

# 3. Create initial conditions
export_init_cond(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), wtemp_file = 'LakeEnsemblR_wtemp_profile_standard.csv', date = '1979-01-01 00:00:00', tprof_file = 'HOLDER.dat', month = 1, ndeps = 2, print = TRUE)

# 4. Run ensemble lake models
run_ensemble()
  