# wrapper function for LHC calibration

A wrapper function for the latin hypercube calibration, used in
cali_ensemble when cmethod == "LHC".

## Usage

``` r
LHC_model(
  pars,
  type,
  model,
  var,
  config_file,
  met,
  folder,
  out_f,
  outf_n,
  obs_deps,
  obs_out,
  out_hour,
  qualfun,
  config_f,
  nout_fun
)
```

## Arguments

- pars:

  data.frame with all parameter sets for the latinhypercube sampling,
  each row is a set of parameters for which the model is to be run.
  colnames must be the parameter names

- type:

  character vector specifying the type of parameter. can be either
  \`"met"\` for meteo scaling or "model" for model specific parameters,
  must have one value per column of \`pars\`

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

- outf_n:

  name of the output file in which the model performance for every
  parameter set is written

- obs_deps:

  depths of observations

- obs_out:

  data.frame with the observations of \`var\`

- out_hour:

  FLake specific: hout of output

- qualfun:

  function that takes data.frames of observations and simulations and
  returns model performance metrics

- nout_fun:

  number of values returned by \`qualfun\`.
