# Export output settings for each model

Exports settings related to output (time step, format), for each model

## Usage

``` r
export_output_settings(
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
