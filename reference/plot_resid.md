# Function to plot residuals for each model of the ensemble run

Plot residual diagnostic plots. Residuals are calculated by (sim - obs)
for each corresponding depth and time step there is an observed value.

## Usage

``` r
plot_resid(
  ncdf = NULL,
  var = "temp",
  dim = "model",
  dim_index = 1,
  var_list = NULL,
  model = NULL
)
```

## Arguments

- ncdf:

  filepath; to the netcdf file created by \`run_ensemble()\`

- var:

  string; of variable which to plot. Defaults to "temp"

- dim:

  character; NetCDF dimensions to extract. Must be either "member" or
  "model". Defaults to "model". Only used if plotting from netCDF file.
  Currently only works with "model".

- dim_index:

  numeric; Index of dimension chosen to extract from. Defaults to 1.
  Only used if plotting from netCDF file.

- var_list:

  list; of variables in the format when loaded using \`load_var()\`.
  Defaults to NULL

- model:

  string vector; of models which should be included in the plot. If NULL
  all models in the netCDF/list are plotted. Defaults to NULL.

## Value

list with four ggplot objects: "obs_res" = Observations versus
residuals, "res_depth" = Residuals versus depth, "yday_res" = residuals
for day of year, "res_dist" = distribution of residuals

## Author

Tadhg Moore, Johannes Feldbauer

## Examples

``` r
if (FALSE) { # \dontrun{
plist <- plot_resid(ncdf = "output/ensemble_output.nc",var = "temp",
                   model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'))
plist[['obs_res']]+
theme_classic()
} # }

```
