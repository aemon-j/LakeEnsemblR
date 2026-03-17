# Plot heat map of ensemble model output

Plot a heat map of ensemble output data. It can either plot directly
from the netCDF file or a list in the format when loaded in with
\`load_var()\`.

## Usage

``` r
plot_heatmap(
  ncdf = NULL,
  var = "temp",
  dim = "model",
  dim_index = 1,
  var_list = NULL,
  model = NULL,
  tile_width = NULL,
  tile_height = NULL
)
```

## Arguments

- ncdf:

  Path to the netCDF file created by \`run_ensemble()\`

- var:

  Variable which to plot. Defaults to "temp"

- dim:

  character; NetCDF dimensions to extract. Must be either "member" or
  "model". Defaults to "model". Only used if plotting from netCDF file.

- dim_index:

  numeric; Index of dimension chosen to extract from. Defaults to 1.
  Only used if plotting from netCDF file.

- var_list:

  list of variables in the format when loaded using \`load_var()\`.
  Defaults to NULL

- model:

  Vector of models which should be included in the plot

- tile_width:

  width of tiles in geom_tile. Defaults to NULL to determine
  automatically

- tile_height:

  height of tiles in geom_tile. Default to NULL to determine
  automatically

## Value

ggplot object of heatmaps

## Author

Tadhg Moore, Johannes Feldbauer, Jorrit Mesman

## Examples

``` r
if (FALSE) { # \dontrun{
ncdf <- 'output/ensemble_output.nc'
# Plot heat maps
plot_heatmap(ncdf = ncdf, model = model, var = "temp")
} # }
```
