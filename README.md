LakeEnsemblR
=====

Tools for running an ensemble of lake models using standardised input data. Lake models currently incorporated are [Freshwater Lake Model (FLake)](http://www.flake.igb-berlin.de/), [General Lake Model (GLM)](http://aed.see.uwa.edu.au/research/models/GLM/), [General Ocean Turbulence Model (GOTM)](https://gotm.net/) and [Simstrat](https://www.eawag.ch/en/department/surf/projects/simstrat/).

## Installation

You can install `LakeEnsemblR` from Github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("aemon-j/LakeEnsemblR")
```

### Visualize

You can download [PyNcView](http://sourceforge.net/projects/pyncview/), a cross-platform NetCDF viewer, for viewing the NetCDF output.

## Example model run
```{r gh-installation, eval = FALSE}
# Install packages - Ensure all packages are up to date - parallel devlopment ongoing
#install.packages('devtools')
devtools::install_github('GLEON/GLM3r')
devtools::install_github('hdugan/glmtools')
devtools::install_github('aemon-j/FLakeR')
devtools::install_github('aemon-j/GOTMr')
devtools::install_github('aemon-j/gotmtools')
devtools::install_github('aemon-j/SimstratR')

# Load libraries
library(LakeEnsemblR)
library(gotmtools)

# Copy template folder
template_folder <- system.file("extdata\\feeagh", package= 'LakeEnsemblR')
dir.create('example') # Create example folder
file.copy(from = template_folder, to = 'example', recursive = TRUE)
setwd('example/feeagh') # Change working directory to example folder

# Set config file
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

```

## Post-processing
```{r gh-installation, eval = FALSE}
# Load libraries for post-processing
library(ggpubr)
library(ggplot2)

## Plot model output using gotmtools/ggplot2

# Extract names of all the variables in netCDF
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

```

## Run Latin hypercube sampling
```{r gh-installation, eval = FALSE}
masterConfigFile <- 'Feeagh_master_config.yaml'

pars <- c('wind_factor', 'swr_factor', 'lw_factor')
mat <- matrix(data = c(0.5,2,0.5,1.5,0.5,1.5), nrow = 3, byrow = T)
df <- as.data.frame(mat)
rownames(df) <- pars
df # Print parameter ranges

# Run Latin_hypercube sample
run_LHC(parRange = df, num = 300,
obs_file = 'LakeEnsemblR_wtemp_profile_standard.csv',
param_file = NULL,
config_file = 'Feeagh_master_config.yaml', model = c('FLake', 'GLM', 'GOTM', 'Simstrat'),
meteo_file = 'LakeEnsemblR_meteo_standard.csv')

## View parameter performance
# Load parameters used
pars <- read.csv('latin_hypercube_params_FLake_GLM_GOTM_Simstrat_XXXX.csv')

# Load results
res <- read.csv('output/latin_hypercube_calibration_results_p300_XXXX.csv')


## FLake
dat <- merge(res[res$model == 'FLake',], pars, by = 'par_id')
dat$model <- 'FLake'
all_par <- dat
fla_par <- dat[which.min(dat$RMSE), c(1,2,9:14)]

my.cols = RColorBrewer::brewer.pal(11, "Spectral")
sub <- which(mlt$variable %in% c('NSE', 'RMSE', 'Pearson_r'))
p1 <- ggplot(dat, aes(wind_factor, swr_factor, colour = RMSE))+
  geom_point(size =2)+
  geom_point(data = dat[which.min(dat$RMSE),], size =4, shape = 21)+
  scale_color_gradientn(colours = (my.cols))+
  geom_hline(yintercept = 1, linetype = 'dashed')+
  geom_vline(xintercept = 1, linetype = 'dashed')+
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p1

p2 <- ggplot(dat, aes(wind_factor, swr_factor, colour = NSE))+
  geom_point(size =2)+
  geom_point(data = dat[which.max(dat$NSE),], size =4, shape = 21)+
  scale_color_gradientn(colours = rev(my.cols))+
  geom_hline(yintercept = 1, linetype = 'dashed')+
  geom_vline(xintercept = 1, linetype = 'dashed')+
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

p3 <- ggplot(dat, aes(wind_factor, swr_factor, colour = Pearson_r))+
  geom_point(size =2)+
  geom_point(data = dat[which.max(dat$Pearson_r),], size =4, shape = 21)+
  scale_color_gradientn(colours = rev(my.cols))+
  geom_hline(yintercept = 1, linetype = 'dashed')+
  geom_vline(xintercept = 1, linetype = 'dashed')+
  theme_bw(base_size = 24)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

g1 <- ggpubr::ggarrange(p1,p2,p3,nrow=3, align = 'v')
g1
ggsave('output/FLake_LHC_plot.png', plot = g1, dpi = 200,width = 324,height = 312, units = 'mm')


# Create meteo driver files with best scaling parameters
export_meteo(masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat'),
             meteo_file = 'LakeEnsemblR_meteo_standard.csv',
             lhc_file = 'output/LHC_calibration_results_p300_XXXXXXXXXXXX.csv',
             metric = 'RMSE')

# Run ensemble lake models with new met file
wtemp_list <- run_ensemble(config_file = masterConfigFile, model = c('FLake', 'GLM', 'GOTM', 'Simstrat'), return_list = TRUE,
                           create_netcdf = TRUE, obs_file = 'LakeEnsemblR_wtemp_profile_standard.csv')
```

How do I contribute new code back to the `LakeEnsemblR` project?
==========================================================

In order to contribute to this code, we recommend the following workflow:

1.  "fork" this repository to your own personal github account

2.  clone the github repository to your computer:

    $git clone <git@github.com:{username}/LakeEnsemblR.git>

3.  modify code or add new functionality, save the code

4.  add the repository master to a remote master called "upstream"

    $cd LakeEnsemblR

    $git remote add upstream <git@github.com:aemon-j/LakeEnsemblR.git>

5.  before pushing your changes to your repository, pull in the current version of the aemon-j master:

    $git fetch upstream

6.  merge these differences with your own "master" version:

    $git merge upstream/master

7.  push your changes to your github repository, in addition to changes made by pulling in the aemon-j master:

    $git push

8.  submit a pull request to aemon-j master using your account at github.com
