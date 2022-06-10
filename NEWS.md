## version 1.1.2

### Changes

- it is now possible to add sensible and latent heat fluxes to the output netcdf file by adding q_sens and/or q_lat to the list of wanted output variables in the yaml config file

## version 1.1.1

### Changes

Bug fixes:
- fixed error in formating of Simstrat flow where a flow of 0 would lead to NA in the Simstrat inflow forcing files

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
