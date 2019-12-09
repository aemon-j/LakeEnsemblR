# Example Configuration File LakeEnsemblR # simulation title used in output (based on the GOTM configuration file)
####################################################################################
#### Location
####################################################################################
name <- 'langtjern'                             # station name used in output [default=GOTM site]
latitude <- 60.37                             # latitude [degrees North; min=-90.0; max=90.0; default=0.0]
longitude <- 9.73                             # longitude [degrees East; min=-360.0; max=360.0; default=0.0]
depth <- 12.0                                 # maximum water depth [m; min=0.0; default=100.0]
hypsograph <- 'hypsograph.dat'                  # hypsograph [default=]
####################################################################################
#### Time:
####################################################################################
start <- '2006-12-01 00:00:00'                  # start date and time [yyyy-mm-dd HH:MM:SS; default=2017-01-01 00:00:00]
stop <- '2015-01-01 00:00:00'                   # stop date and time [yyyy-mm-dd HH:MM:SS; default=2018-01-01 00:00:00]
timestep <- 3600.0                            # time step for integration [s; min=0.0; default=3600.0]
####################################################################################
#### config_files:
####################################################################################
gotm_config <- 'GOTM/gotm.yaml'                 # GOTM config file (yaml format)
glm_config <- 'GLM/glm3.nml'                    # GLM config file (nml format)
simstrat_config <- 'Simstrat/simstrat.par'      # Simstrat config file (json format)
flake_config <- 'FLake/flake.nml'               # FLake config file (nml format)
####################################################################################
#### Input:                      
####################################################################################
## meteo:
# Ten_Meter_Elevation_Wind_Speed_meterPerSecond (wss):                         # wind speed in m/s at 10 m above the surface
wss_method <- 2                 # method [-1 = not available, 0=constant, 2=from file; default=0]
wss_constant_value <- 0.0       # value to use throughout the simulation [m/s; default=0.0]
wss_file <- 'meteo.dat'           # path to file with time series [default=]
wss_column <- 1                 # index of column to read from [default=1]
scale_factor <- 1.0         # scale factor to be applied to values read from file [default=1.0]
# Ten_Meter_Elevation_Wind_Direction_degree (wdir):                         # wind direction in degrees (0° is northern wind, 90° eastern wind, etc.
wdir_method <- 0                 # method [0=constant, 2=from file; default=0]
wdir_constant_value <- 0.0       # value to use throughout the simulation [m/s; default=0.0]
wdir_file <- 'meteo.dat'           # path to file with time series [default=]
wdir_column <- 2                 # index of column to read from [default=1]
wdir_scale_factor <- 1.0         # scale factor to be applied to values read from file [default=1.0]
# Surface_Level_Barometric_Pressure_pascal (airp):                        # air pressure at the surface of the lake
airp_method <- 2                 # method [0=constant, 2=from file; default=0]
airp_constant_value <- 0.0       # value to use throughout the simulation [Pa; default=0.0]
airp_file <- meteo.dat           # path to file with time series [default=]
airp_column <- 3                 # index of column to read from [default=1]
airp_scale_factor <- 1.0       # scale factor to be applied to values read from file [default=1.0]
# Air_Temperature_celsius (airt):                        # air temperature at 2 m above the surface, in degC
airt_method <- 2                 # method [0=constant, 2=from file; default=0]
airt_constant_value <- 0.0       # value to use throughout the simulation [Celsius or K; default=0.0]
airt_file <- 'meteo.dat'                    # path to file with time series [default=]
airt_column <- 4                 # index of column to read from [default=1]
# Relative_Humidity_percent (relh):                         # humidity @ 2 m
relh_method <- 2                 # method [0=constant, 2=from file; default=0]
relh_constant_value <- 0.0       # value to use throughout the simulation [default=0.0]
relh_file <- 'meteo.dat'                    # path to file with time series [default=]
relh_column <- 5                 # index of column to read from [default=1]
# Vapor_Pressure_milliBar (vap):                       # cloud vapour pressure
vap_method <- 2                 # method [0=constant, 2=from file; default=0]
vap_constant_value <- 0.0       # value to use throughout the simulation [fraction; min=0.0; max=1.0; default=0.0]
vap_file <- 'meteo.dat'                    # path to file with time series [default=]
vap_column <- 6                 # index of column to read from [default=1]
# Cloud_Cover_decimalFraction (cc):                         # cloud cover
cc_method <- 2                 # method [0=constant, 2=from file, 3=from time, location and cloud cover; default=0]
cc_constant_value <- 0.0       # value to use throughout the simulation [W/m^2; min=0.0; default=0.0]
cc_file <- 'surfWaterRad.dat'    # path to file with time series [default=]
cc_column <- 1                 # index of column to read from [default=1]
# Shortwave_Radiation_Downwelling_wattPerMeterSquared (swr):                         # shortwave radiation
swr_method <- 2                 # method [0=constant, 2=from file, 3=from time, location and cloud cover; default=0]
swr_constant_value <- 0.0       # value to use throughout the simulation [W/m^2; min=0.0; default=0.0]
swr_file <- 'surfWaterRad.dat'    # path to file with time series [default=]
swr_column <- 1                 # index of column to read from [default=1]
# Precipitation_meterPerSecond (pre):                      # precipitation in m/s
pre_method <- 0                 # method [0=constant, 2=from file; default=0]
pre_constant_value <- 0.0       # value to use throughout the simulation [m/s; default=0.0]
pre_file <- 'precip.dat'          # path to file with time series [default=]
pre_column <- 1                 # index of column to read from [default=1]
pre_scale_factor <- 1.0    # scale factor to be applied to values read from file [default=1.0]
###############
## ice:
ice_use <- TRUE                     # turn on ice models? [default=TRUE]
ice_H <- 0.0                       # initial ice thickness [m; default=0.0]
## salt:
sal_use <- FALSE                     # allow density variations due to salt? [default = TRUE]
## inflows:
inf_use <- FALSE                     # use in- and outflows?
## Ouflow1:                                    # stream configuration
flow_method <- 4                                 # inflow method [default=1 from surface, 2=from bottom, 3=depth interval, 4=density resolved]
flow_zu <- 0.0                                   # upper limit [m; default=0.0]
flow_zl <- 0.0                                   # lower limit [m; default=0.0]
#flow:                                     # water flow
flow_method <- 2                              # method [0=constant, 2=from file; default=0]
flow_constant_value <- 0.0                    # value to use throughout the simulation [m^3/s; default=0.0]
flow_file <- 'inputQ_kristine.dat'              # path to file with time series [default=]
flow_column <- 1                              # index of column to read from [default=1]
flow_scale_factor <- 2                        # 2 to compensate for ungauged area 
# inf_temp:                                     # flow temperature
inf_temp_method <- 2                              # method [0=constant, 2=from file; default=0]
inf_temp_constant_value <- -1.0                   # value to use throughout the simulation [Celcius; default=-1.0]
inf_temp_file <- 'inflowchem_kristine.dat'          # path to file with time series [default=]
inf_temp_column <- 2                              # index of column to read from [default=1]
####################################################################################
#### Output:
####################################################################################
output_fil <- 'output'                               # path of output file, excluding extension
format <- 'netcdf'                        # format [text, netcdf; default=netcdf]
time_unit <- 'day'                        # time unit [second, hour, day, month, year, dt=model time step; default=day]
time_step <- 1                          # number of time units between output [min=1; default=1]
time_method <- 'mean'                     # treatment of time dimension [point=instantaneous, mean, integrated; default=point]
sync_interval <- 0                      # number of output steps between sychronization to disk (<= 0: sync on close only) [default=1]
var_source <- '/*'                       # variable name in model
####################################################################################
#### Biogeochemistry:                         # not yet implemented
####################################################################################
bgc_use <- FALSE