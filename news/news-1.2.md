# Version 1.2

## version 1.2.1

### Changes

- added possibility to calibrate Kw value for all models. to do so they
  LakeEnsemblR config file needs a new entry `Kw` in the calibration
  section of the file (see changed LakeEnsemblR.yaml template file)
- fixed an issue with
  [`get_output()`](https://aemon-j.github.io/LakeEnsemblR/reference/get_output.md)
  for MyLake where under dplyr v. 1.1 a crash would occure

## version 1.2.2

### Changes

- added possibility to set model specific `Kw` values or time series in
  the config file. If you want to use the same value for all models or
  just modify the value for a single model you need to supply a value
  for `all`. Old config files with a single value still work.

## version 1.2.3

### Changes

- fixed a small mistake with the model specific `Kw` values

## version 1.2.4

### Changes

- fixed a mistake with when time series of `Kw` values were given
- New version of the `plot_heatmap` function, as the old version could
  show unintended shifts or white lines with certain data compositions

## version 1.2.5

### Changes

- fixed a bug with multiple outflows that would get
  [`export_flow()`](https://aemon-j.github.io/LakeEnsemblR/reference/export_flow.md)
  to crash
- fixed another small bug with the recent changes in `Kw`

## version 1.2.6

### Changes

- addition to the bug fix from v1.2.1 (related to a change in the dplyr
  package), but now also applied when density and salinity output are
  requested
- bug fix: an issue could occur if output variables were requested in a
  different order than used by the `get_output` function, which has been
  solved
- water level changes are now handled correctly for GOTM salinity and
  density (previously only temperature)
- Rewrote the LHC calibration in parallel mode so that it can
  parallelize for each model rather than by model and thus speeds up the
  LHC in parallel.
- This has been added as `cmethod = "LHC"` where the original LHC mode
  is preserved as `cmethod = "LHC_old"`.
- Corresponding update in MyLakeR which resets the working directory
  after running the model.
- fixed bug in `check_congig()` that would throw an error if `Kw`
  section was not present in the calibration part of the config yaml
  file

## version 1.2.7

### Changes

- bug fix of `plot_LHC` function; can be used also when not calibrating
  `Kw`

## version 1.2.8

### Changes

- bug fix of `export_model_parameters` function when using `folder`
  argument other than `"."`
- bug fix in `cali_ensemble` when unit of observations is seconds

## version 1.2.9

### Changes

- Added pkgdown website for LakeEnsemblR
- Added pkgdown build to GitHub Actions
- Updated DESCRIPTION file with GitHub dependencies
- Updated old GitHub Actions to new format
