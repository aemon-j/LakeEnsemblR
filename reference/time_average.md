# Calculates the average of an irregular time series

Calculates the average of an irregular time series

## Usage

``` r
time_average(timeseries, start, end, n = 1000)
```

## Arguments

- timeseries:

  dataframe; first column POSIXct, second column numeric

- start:

  POSIXct; start of the averaging window

- end:

  POSIXct; end of the averaging window

- n:

  integer; number of blocks
