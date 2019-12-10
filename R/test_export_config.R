setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../data/feeagh')

# Install packages - Ensure all packages are up to date - parallel devlopment ongoing (especially gotmtools)
#install.packages('devtools')
devtools::install_github('GLEON/GLM3r')
devtools::install_github('hdugan/glmtools')
devtools::install_github('aemon-j/FLakeR')
devtools::install_github('aemon-j/GOTMr')
devtools::install_github('aemon-j/gotmtools')
devtools::install_github('aemon-j/SimstratR')


# Load libraries
library(GOTMr);library(SimstratR);library(GLM3r);library(FLakeR);library(gotmtools);library(glmtools)
library(lubridate);library(plyr);library(ncdf4); library(ggplot2)

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
# source('../../R/helper_functions/get_yaml_value.R') # Will be added to gotmtools in the future
source('../../R/helper_functions/get_wtemp_df.R') # Potential function for flaketools

# 1. Example - creates directories with all model setup
export_config(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), folder = '.',
              hypsograph_file = 'LakeEnsemblR_bathymetry_standard.csv', lat = 53, lon = -9,
              name = 'feeagh', Kw = 1.5)


# 2. Create meteo driver files
export_meteo(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'),
             meteo_file = 'LakeEnsemblR_meteo_standard.csv')

# 3. Create initial conditions
export_init_cond(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'),
                 wtemp_file = 'LakeEnsemblR_wtemp_profile_standard.csv',
                 date = '1979-01-01 00:00:00', tprof_file = 'HOLDER.dat',
                 month = 1, ndeps = 2, print = TRUE)

# 4. Run ensemble lake models
wtemp_list <- run_ensemble(model = c('GOTM', 'GLM', 'Simstrat', 'FLake'), return_list = TRUE, make_output = TRUE, config_file = 'HOLDER.yaml')


####
# Plot model output using rLakeAnalyzer
####
names(wtemp_list)
for(i in 1:length(wtemp_list)){
  rLakeAnalyzer::wtr.heat.map(wtemp_list[[i]], main = names(wtemp_list)[i])
}
####

####
# Plot model output using gotmtools/ggplot2
####
#Extract names of all the variables in netCDF
vars <- gotmtools::list_vars('ensemble_output.nc4')
vars # Print variables

plist <- list() # Initialize empty list for storing plots of each variable
for(i in 1:length(vars)){
  p1 <- gotmtools::plot_vari('ensemble_output.nc4',
                             var = vars[i],
                             incl_time = FALSE, 
                             limits = c(0,22),
                             zlab = 'degC')
  p1 <- p1 + scale_y_reverse() + #Reverse y-axis
    ggtitle(vars[i]) + # Add title using variable name
    xlab('')+ # Remove x-label
    theme_bw(base_size = 18) # Increase font size of plots
  plist[[i]] <- p1
}

# Plot all model simulations
# install.packages('ggpubr')
g1 <- ggpubr::ggarrange(plotlist = plist, ncol = 1, common.legend = TRUE, legend = 'right')
g1
ggsave('model_ensemble_watertemp.png', g1,  dpi = 300,width = 384,height = 300, units = 'mm')
####


