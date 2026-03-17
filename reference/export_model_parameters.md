# Export model-specific parameters

Exports model-specific parameters that are specified in the
model_parameters section of the master config file.

## Usage

``` r
export_model_parameters(
  config_file,
  model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
  folder = "."
)
```

## Arguments

- config_file:

  name of the master LakeEnsemblR config file

- model:

  vector; model to export configuration file. Options include c("FLake",
  "GLM, "GOTM", "Simstrat", "MyLake")

- folder:

  folder
