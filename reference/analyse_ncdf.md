# Analyse the output netCDF

Analyse the LER output neCDF and produce summary statistics

## Usage

``` r
analyse_ncdf(
  ncdf,
  model,
  dim = "model",
  dim_index = 1,
  spin_up = 0,
  drho = 0.1
)

analyze_ncdf(
  ncdf,
  model,
  dim = "model",
  dim_index = 1,
  spin_up = 0,
  drho = 0.1
)
```

## Arguments

- ncdf:

  filepath; to the \`ensemble_output.nc\` file

- model:

  Vector of models for which to calculate the performance measures

- dim:

  character; NetCDF dimensions to extract. Must be either "member" or
  "model". Defaults to "model". Only used if plotting from netCDF file.
  Currently only works with "model".

- dim_index:

  numeric; Index of dimension chosen to extract from. Defaults to 1.
  Only used if plotting from netCDF file.

- spin_up:

  numeric; Number of days to disregard as spin-up for analysis.

- drho:

  numeric; density difference between top and bottom indicating
  stratification \[kg m^-3\]

## Value

list of four dataframes: 'out_df' = long data frame with date, depths,
temp and model, 'stats' = summary statistics of model fitness compared
to observed and mean differences between modelled and observed
phenological events, 'strat' = stratification statistics for each year,
'obs_temp' = long dataframe of observed data within the netCDF file

## Author

Tadhg Moore

## Examples

``` r
if (FALSE) { # \dontrun{
} # }
```
