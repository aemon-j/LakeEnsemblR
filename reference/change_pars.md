# Change parameter or meteo scaling for a model

Input a specific parameter or scale the meteorological forcing for a
selected model

## Usage

``` r
change_pars(config_file, model, pars, type, met, folder)
```

## Arguments

- config_file:

  path to master config file

- model:

  name of the model

- pars:

  named vector of parameters to change

- type:

  character vector specifying the type of parameter. can be either
  \`"met"\` for meteo scaling or "model" for model specific parameters,
  must have the same length as \`pars\`

- met:

  data.frame with the model specific meteo data, which is then feed to
  scale_meteo

- folder:

  root folder
