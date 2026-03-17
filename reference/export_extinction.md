# Export extinction coefficients

Exports extinction coefficients for each model based on a master
LakeEnsemblR config file

## Usage

``` r
export_extinction(
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
