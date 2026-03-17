# Export model configuration setups

Exports all settings from the master LakeEnsemblR config file

## Usage

``` r
export_config(
  config_file,
  model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
  dirs = TRUE,
  time = TRUE,
  location = TRUE,
  output_settings = TRUE,
  meteo = TRUE,
  init_cond = TRUE,
  extinction = TRUE,
  flow = TRUE,
  model_parameters = TRUE,
  folder = "."
)
```

## Arguments

- config_file:

  name of the master LakeEnsemblR config file

- model:

  vector; model to export configuration file for. Options include
  c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")

- dirs:

  boolean; create directories for each model and if needed copies
  templates. Calls export_dirs. Defaults to TRUE

- time:

  boolean; exports time settings. Calls export_time. Defaults to TRUE.

- location:

  boolean; exports location and hypsograph settings. Calls
  export_location. Defaults to TRUE.

- output_settings:

  boolean; exports output settings. Calls export_output_settings.
  Defaults to TRUE.

- meteo:

  boolean; export meteorology data. Calls export_meteo. Defaults to
  TRUE.

- init_cond:

  boolean; export initial conditions. Calls export_init_cond. Defaults
  to TRUE.

- extinction:

  boolean; export light extinction data. Calls export_extinction.
  Defaults to TRUE.

- flow:

  boolean; export flow settings. Calls export_flow. Defaults to TRUE.

- model_parameters:

  boolean; export model parameters specificed in the yaml configuration
  file. Calls export_model_parameters. Defaults to TRUE.

- folder:

  folder

## Author

Tadhg Moore, Jorrit Mesman, Johannes Feldbauer, Robert Ladwig

## Examples

``` r
if (FALSE) { # \dontrun{
config_file <- "LakeEnsemblR.yaml"
export_config(config_file = config_file,
              model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
              folder = ".")
} # }
```
