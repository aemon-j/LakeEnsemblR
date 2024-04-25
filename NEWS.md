## version 1.2.7

### Changes
- bug fix of `plot_LHC` function; can be used also when not calibrating `Kw`

## version 1.2.6

### Changes

- addition to the bug fix from v1.2.1 (related to a change in the dplyr package), but now also applied when density and salinity output are requested
- bug fix: an issue could occur if output variables were requested in a different order than used by the `get_output` function, which has been solved
- water level changes are now handled correctly for GOTM salinity and density (previously only temperature)
- Rewrote the LHC calibration in parallel mode so that it can parallelize for each model rather than by model and thus speeds up the LHC in parallel. 
- This has been added as `cmethod = "LHC"` where the original LHC mode is preserved as `cmethod = "LHC_old"`.
- Corresponding update in MyLakeR which resets the working directory after running the model.
- fixed bug in `check_congig()` that would throw an error if `Kw` section was not present in the calibration part of the config yaml file

## version 1.2.5

### Changes

- fixed a bug with multiple outflows that would get `export_flow()` to crash
- fixed another small bug with the recent changes in `Kw`

## version 1.2.4

### Changes

- fixed a mistake with when time series of `Kw` values were given
- New version of the `plot_heatmap` function, as the old version could show unintended shifts or white lines with certain data compositions

## version 1.2.3

### Changes

- fixed a small mistake with the model specific `Kw` values

## version 1.2.2

### Changes

- added possibility to set model specific `Kw` values or time series in the config file. If you want to use the same value for all models or just modify the value for a single model you need to supply a value for `all`. Old config files with a single value still work.

## version 1.2.1

### Changes

- added possibility to calibrate Kw value for all models. to do so they LakeEnsemblR config file needs a new entry `Kw` in the calibration section of the file (see changed LakeEnsemblR.yaml template file)
- fixed an issue with `get_output()` for MyLake where under dplyr v. 1.1 a crash would occure

## version 1.1.9

### Changes

- fixed a bug in the LHC calibration where if a model would crash on the first run the headding of the ouput file would be wrong
- Add the number of rows in the hypsograph file to the calculated `max_layers` parameter in GLM, to avoid failure of Lagrangian algorithm

## version 1.1.8

### Changes

- To convert temperature observations to long-format, the `dcast` function within `run_ensemble` and `cali_ensemble` now uses `mean(..., na.rm = TRUE)` as aggregation function. Previously, this could cause problems in case of non-unique observations (a warning is thrown if these exist).
- added a check if the mean depth is below 1.5 meters as this could cause the `export_init_cond` function to crash for FLake as `calc_hmix` has a default value of 1.5 m for a minimum depth

## version 1.1.7

### Changes

Bug fixes:
- Warning message shows up when pre-v1.1 `mass-balance` argument is used, pointing towards a solution when the `outflows` section is missing.

## version 1.1.6

### Changes

Bug fixes:
- Fixed error in `analyse_ncdf` when ice is not present in the ncdf file and if not enough observational data.

## version 1.1.5

### Changes

Bug fixes:
- Fixed error in `export_flow` when using a start or stop date that was not included in the inflow or outflow file
- Fixed bug in `get_output` which would show wrong NA values in Simstrat temperature output due to water level change when including depths in observation files that were not included in the Simstrat output. 

## version 1.1.4

### Changes

Bug fixes:
- Fixed error in `export_extinction` that caused GOTM to crash because of formating of the datetime column

## version 1.1.3

### Changes

Bug fixes:
- Calculation of degrees East longitude in output netcdf file was corrected, with values 180-360 for locations on the Western Hemisphere

## version 1.1.2

### Changes

- it is now possible to add sensible and latent heat fluxes to the output netcdf file by adding q_sens and/or q_lat to the list of wanted output variables in the yaml config file

## version 1.1.1

### Changes

Bug fixes:
- fixed error in formatting of Simstrat flow where a flow of 0 would lead to NA in the Simstrat inflow forcing files

## version 1.1.0

### Changes

- Inflows and outflows are now separate inputs and can be different from each other
- Multiple inflows and outflows can be simulated. 
- Outflows can exit the lake at specified depths, or at the surface. 
- Water level ("w_level") is now a potential output of LakeEnsemblR. 
- "max_members" can be specified, which sets the number of members that the output-netcdf can have
- LakeEnsemblR now works with the latest version of Simstrat (v3.0.1), which included changes in some parameter names
- Model-specific template config files are removed from the LakeEnsemblR package and are instead retrieved from the model-specific packages

Bug fixes:
- get_yaml_multiple and input_yaml_multiple; works now if the first key is the highest level in the yaml file
- cali_ensemble for FLake; depths are now sorted
- Initial conditions Simstrat; Simstrat now always starts at the specified initial water lvl
- plot_heatmap now works in lakes where the water level changes

The setup of the LakeEnsemblR configuration file changed. More information can be found on: https://github.com/aemon-j/LakeEnsemblR/wiki/From-v1.0-to-v1.1


## version 1.0.5

### Changes

- added NEWS.md file
- Changed default branch name to "main". If you contribute to LakeEnsemblR and have a local clone of the repository please update your local branch name using:
```
git branch -m master main
git fetch origin
git branch -u origin/main main
git remote set-head origin -a
```
