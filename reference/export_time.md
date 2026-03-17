# Export time settings for each model

Exports settings like start and end time and time step, based on the
master LakeEnsemblR config file

## Usage

``` r
export_time(
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
