# Creates directories for each model

Creates directories with file setups for each model, based on the master
LakeEnsemblR config file

## Usage

``` r
export_dirs(
  config_file,
  model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
  folder = "."
)
```

## Arguments

- config_file:

  name of the master LakeEnsemblR config file

- model:

  vector; model to export configuration file. Options include c("GOTM",
  "GLM", "Simstrat", "FLake", "MyLake")

- folder:

  folder
