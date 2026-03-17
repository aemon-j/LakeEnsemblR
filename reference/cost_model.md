# Run a model and calculate model cost

Runns the selected model and calculates fit metrics using a provided
funtion

## Usage

``` r
cost_model(
  config_file,
  model,
  var,
  folder,
  obs_deps,
  obs_out,
  out_hour,
  qualfun,
  config_f
)
```

## Arguments

- config_file:

  path to master config file

- model:

  name of the model

- var:

  name of variable for which to calculate model performance

- folder:

  root folder

- obs_deps:

  depths of observations

- obs_out:

  data.frame with the observations of \`var\`

- out_hour:

  FLake specific: hout of output

- qualfun:

  function that takes data.frames of observations and simulations and
  returns model performance metrics

- config_f:

  path to model specific control file (e.g. "GLM/glm3.nml")
