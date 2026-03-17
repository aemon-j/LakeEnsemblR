# warpper function for other two methods (modMCMC and modFit)

A wrapper function for the modMCMC and modFit calibration methods, used
in cali_ensemble when cmethod == "modMCMC" or "modFit".

## Usage

``` r
wrap_model(
  pars,
  type,
  model,
  var,
  config_file,
  met,
  folder,
  out_f,
  obs_deps,
  obs_out,
  out_hour,
  qualfun,
  config_f,
  outf_n,
  write = TRUE
)
```

## Arguments

- pars:

  named vector with a set of parameter for wich the model is to be run
  and a performance metric is to be returned

- type:

  character vector specifying the type of parameter. can be either
  \`"met"\` for meteo scaling or "model" for model specific parameters,
  must have the same length as \`pars\`.

- model:

  the model name for which to run the function

- var:

  variable for which to calculate the model performance (usually this is
  \`"temp"\`)

- config_file:

  path to the master config file

- met:

  data.frame with the model specific meteo data, which is then feed to
  scale_meteo

- folder:

  root folder

- out_f:

  folder in the root folder where the output of the LHC should be
  written to

- obs_deps:

  depths of observations

- obs_out:

  data.frame with the observations of \`var\`.

- out_hour:

  FLake specific: hout of output

- qualfun:

  function that takes data.frames of observations and simulations and
  returns model performance metrics

- outf_n:

  name of the output file in which the model performance for every
  parameter set is written

- write:

  write the results to a file?

- nout_fun:

  number of values returned by \`qualfun\`.
