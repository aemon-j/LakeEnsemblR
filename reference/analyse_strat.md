# Returns stratification statstics

Returns stratification statstics: annual mean, max and total length of
summer, winter stratification and ice duration. NOTE: summer strat
periods are allocated to the year in which the period starts. Winter
stratification and ice periods are allocated to the year in which they
end.

## Usage

``` r
analyse_strat(data = NULL, Ts, Tb, dates, H_ice = NULL, drho = 0.1, NH = TRUE)

analyze_strat(data = NULL, Ts, Tb, dates, H_ice = NULL, drho = 0.1, NH = TRUE)
```

## Arguments

- data:

  dataframe; water temperature data in long format with date, depths,
  value. Defaults to NULL

- Ts:

  vector; of surface temperatures which corresponds to date vector

- Tb:

  vector; of bottom temperatures which corresponds to date vector

- dates:

  vector; of POSIX style date corresponding to rows of Ts and Tb.

- H_ice:

  vector; of ice thickness which corresponds to date vector, set to NULL
  if analysis not required. Defaults to NULL

- drho:

  numeric; density difference between top and bottom indicating
  stratification \[kg m^-3\]

- NH:

  boolean; northern hemisphere? TRUE or FALSE. Defaults to true

## Author

Tom Shatwell

## Examples

``` r
if (FALSE) { # \dontrun{
strat <- analyse_strat(Ts = df[,2], Tb = df[,ncol(df)], dates = df[,1])
} # }
```
