# Extract variables from NetCDF file

Extracts a selected parameter from the netCDF file and formats it into a
dataframe.

## Usage

``` r
load_var(
  ncdf,
  var,
  return = "list",
  dim = "model",
  dim_index = 1,
  print = TRUE
)
```

## Arguments

- ncdf:

  filepath; Name of the netCDF file to extract variable

- var:

  character; Name of the variable to be extracted. Must match short name
  in netCDF file

- return:

  character; Must be either list or array

- dim:

  character; NetCDF dimensions to extract. Must be either "member" or
  "model". Defaults to "model".

- dim_index:

  numeric; Index of dimension chosen to extract from. Defaults to 1.

- print:

  logical; Print the name and units of the variable extracted, defaults
  to TRUE

## Value

dataframe in the same format as the observation file with the surface in
the top column and the bottom in the last column.
