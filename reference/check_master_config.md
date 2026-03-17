# Check master config file

Check if the master config file is correct

## Usage

``` r
check_master_config(
  config_file,
  model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
  exp_cnf = FALSE
)
```

## Arguments

- config_file:

  filepath; to LakeEnsemblr yaml master config file

- model:

  vector; model to export driving data. Options include c("GOTM", "GLM",
  "Simstrat", "FLake", "MyLake")

- exp_cnf:

  boolean; check if the control files for the models are there
