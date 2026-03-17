# Function to plot results of the ensemble run

Plot the outcome of the ensemble run for a given depth along with the
minimum, maximum, and an average value.

## Usage

``` r
plot_ensemble(
  ncdf,
  model = c("FLake", "GLM", "GOTM", "Simstrat", "MyLake"),
  var = "temp",
  dim = "model",
  dim_index = 1,
  depth = NULL,
  date = NULL,
  av_fun = "mean",
  residuals = FALSE,
  boxwhisker = FALSE
)
```

## Arguments

- ncdf:

  filepath; to the netcdf file created by \`run_ensemble()\`

- model:

  string vector; of models which should be included in the plot

- var:

  string; of variable which to plot

- dim:

  string; NetCDF dimensions to extract. Must be either "member" or
  "model". Defaults to "model". Only used if plotting from netCDF file.

- dim_index:

  numeric; Index of dimension chosen to extract from. Defaults to 1.
  Only used if plotting from netCDF file.

- depth:

  If \`var\` has a depth dimension, for which depth should it be
  plotted?

- date:

  Specific date for which depth profiles should be plotted

- av_fun:

  Averaging function to use, defaults to the arithmetic mean
  (\`mean()\`)

- residuals:

  Create an additional plot with model residuals over time

- boxwhisker:

  Create additional box-whisker plots for each model

## Author

Johannes Feldbauer, Robert Ladwig, Jorrit Mesman

## Examples

``` r
if (FALSE) { # \dontrun{
# time series
p1 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
                   model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
                   var = "temp", depth = 0.9, boxwhisker = TRUE)

# depth profiles
p2 <- plot_ensemble(ncdf = "output/ensemble_output.nc",
                   model = c('FLake', 'GLM',  'GOTM', 'Simstrat', 'MyLake'),
                   var = "temp", date = as.POSIXct("2010-06-13", tz = "UTC"),
                   boxwhisker = TRUE)

} # }
```
