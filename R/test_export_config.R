setwd('...\\LakeEnsemblR\\data\\feeagh')

#library
library(GOTMr);library(SimstratR);library(GLM3r);library(FLakeR)

# Load functions
source('../../R/export_config.R')
source('../../R/input_json.R')
source('../../R/input_nml.R') # This versions preserves comments in the nml

# Example - creates directories with all model setups
export_config(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.', hypsograph_file = 'Feeagh_hypsometry.csv', lat = 53, lon = -9, name = 'feeagh', Kw = 1.5)
  