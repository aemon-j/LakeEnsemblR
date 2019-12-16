#initial clean up
rm(list = ls())
graphics.off()
cat("\f")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('../data/feeagh')

# Install packages - Ensure all packages are up to date - parallel devlopment ongoing (especially gotmtools)
#install.packages('devtools')
# devtools::install_github('GLEON/GLM3r')
# devtools::install_github('hdugan/glmtools')
# devtools::install_github('aemon-j/FLakeR')
devtools::install_github('aemon-j/GOTMr')
devtools::install_github('aemon-j/gotmtools')
# devtools::install_github('aemon-j/SimstratR')


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
source('../../R/helper_functions/get_wtemp_df.R') # Potential function for flaketools
source('../../R/helper_functions/analyse_strat.R') # Potential function for flaketools

masterConfigFile <- 'Feeagh_master_config.yaml'

# 1. Example - creates directories with all model setup
export_config(config_file = masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat'), folder = '.')

# 2. Create meteo driver files
export_meteo(masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat'),
             meteo_file = 'LakeEnsemblR_meteo_standard.csv')

# 3. Create initial conditions
start_date <- get_yaml_value(file = masterConfigFile, label =  "time", key = "start")

export_init_cond(model = c('FLake', 'GLM', 'GOTM', 'Simstrat'),
                 wtemp_file = 'LakeEnsemblR_wtemp_profile_standard.csv',
                 date = start_date, tprof_file = 'HOLDER.dat',
                 month = 1, ndeps = 2, print = TRUE)

# 4. Run ensemble lake models
wtemp_list <- run_ensemble(config_file = masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat'), return_list = TRUE,
                           create_netcdf = TRUE, obs_file = 'LakeEnsemblR_wtemp_profile_standard.csv')


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
ens_out <- 'output/ensemble_output.nc4'
vars <- gotmtools::list_vars(ens_out)
vars # Print variables

plist <- list() # Initialize empty list for storing plots of each variable
for(i in 1:(length(vars)-1)){
  p1 <- gotmtools::plot_vari(ncdf = ens_out,
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
ggsave('output/model_ensemble_watertemp.png', g1,  dpi = 300,width = 384,height = 300, units = 'mm')
####

###
# Calculate thermocline depth
###
t_d <- lapply(1:length(wtemp_list), function(x)rLakeAnalyzer::ts.thermo.depth(wtr = wtemp_list[[x]]))
names(t_d) <- names(wtemp_list)
datetime <- t_d[[1]][['datetime']]
data_sub <- lapply(t_d, function(x) x[["thermo.depth"]])
td_df <- data.frame(c(list('datetime' = datetime)), data_sub)
td_df <- reshape::melt(td_df, id.vars = 'datetime')

p2 <- ggplot(td_df, aes(datetime, value, colour = variable))+
  geom_line()+
  scale_y_reverse()+
  theme_bw(base_size = 12)+
  xlab('')+
  ylab('Depth (m)')+
  ggtitle('Thermocline Depth')+
  guides(colour = guide_legend(override.aes = list(size=4, alpha = 1, shape = 0), title = 'Model'))+
  theme_bw(base_size = 18)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p2
ggsave('output/model_ensemble_thermodepth.png', p2,  dpi = 300,width = 384,height = 300, units = 'mm')
####

###
# Calculate stratification statistics
strat <- lapply(1:length(wtemp_list), function(x){
  df = na.exclude(wtemp_list[[x]])
  dat = analyse_strat(Ts = df[,2], Tb = df[,ncol(df)], dates = df[,1])
  dat$model <- names(wtemp_list)[x]
  return(dat)
})
names(strat) <- names(wtemp_list)
strat <- do.call("rbind", strat) # Bind list into data.frame
strat
write.csv(strat, 'output/ensemble_strat_results.csv', row.names = F, quote = F)

if('Obs' %in% strat$model){
  error <- strat
  for(i in 1:nrow(strat)){
    if(strat$model[i] == 'Obs'){
      next
    }
    yr <- strat$year[i]
    obs <- strat[strat$model == 'Obs' & strat$year == yr,]
    error[i, -c(1, ncol(error))] <- strat[i,-c(1, ncol(strat))] - obs[1, -c(1, ncol(obs))]
  }
}
error[error$year == 2010,]
###

###
# Add model diagnostics plot... e.g. lapply(wtemp_list, diag_plot)
obs <- na.exclude(wtemp_list[[length(wtemp_list)]])
obs <- reshape2::melt(obs, id.vars = 1)
obs[,2] <- as.character(obs[,2])
obs[,2] <- as.numeric(gsub('wtr_','',obs[,2]))
colnames(obs) <- c('datetime','Depth_meter','Water_Temperature_celsius')
obs <- obs[order(obs[,1], obs[,2]),]

diag <- lapply(1:(length(wtemp_list)-1), function(x){
  print(names(wtemp_list)[x])
  mod <- reshape2::melt(wtemp_list[[x]], id.vars = 1)
  mod[,2] <- as.character(mod[,2])
  mod[,2] <- as.numeric(gsub('wtr_','',mod[,2]))
  colnames(mod) <- c('datetime','Depth_meter','Water_Temperature_celsius')
  mod <- mod[order(mod[,1], mod[,2]),]
  if(nrow(mod) != nrow(obs)){
    mod <- merge(obs, mod, by = c(1,2), all.x = T)
    mod <- mod[order(mod[,1], mod[,2]),]
    mod <- mod[,c(1,2,4)]
    colnames(mod) <- c('datetime','Depth_meter','Water_Temperature_celsius')
  }
  print(dim(mod))
  print(summary(mod))
  # diag_plots(mod, obs) # Needs to be sorted
  return(mod)
})
###