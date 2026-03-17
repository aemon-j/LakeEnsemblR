# Estimate mixed-layer depth from temperature profiles

This function estimates the mixed depth. It finds the minimum curvature
in the profile (maximum for winter stratification) as the border of the
surface mixed layer. Then it finds the thermocline as the depth of
maximum T-gradient. It estimates the mixed layer depth as the depth
where the regression line through the surface layer temperatures
intersects with the regression line through the thermocline
temperatures. It performs some other checks, like whether stratification
exists (Ts-Tb) \> threshold min.dT, and whether the thermocline is above
the surface layer, or whether the lake is completely isothermal and
there is no intersection (and returns NA). If mixed, it assumes the
mixed layer depth is the maximum depth. It also plots the profile if
desired (plot=TRUE), marking the surface layer and inflection point in
red and the thermocline in blue.

## Usage

``` r
calc_hmix(
  temps,
  depths,
  min.dT = 1,
  therm.dz = 1,
  min.hmix = 1.5,
  plot = FALSE,
  ...
)
```

## Arguments

- temps:

  a numeric vector of water temperature in degC.

- depths:

  a numeric vector corresponding to the depths (in m) of the temps
  measurements

- min.dT:

  numeric; minimum change in water temperature

- therm.dz:

  numeric; depth resolution for calculating thermoclime

- min.hmix:

  numeric; minimum depth of hmix otherwise returns NA. Defaults to 1.5m

- plot:

  boolean; plot temperature profile with calculated mixed depth.
  Defaults to FALSE

- ...:

  arguments to be passed to base plot

## Author

Tom Shatwell
