# Export location settings for each model

Exports settings like hypsograph, coordinates and ice settings, based on
the master LakeEnsemblR config file

## Usage

``` r
export_location(
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
