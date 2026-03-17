# Extract value from any LER model config file

Wrapper function for functions that extract values from model config
files

## Usage

``` r
get_config_value(model, file, label, key)
```

## Arguments

- model:

  string; name of a model in the LER package

- file:

  filepath; to model-specific config file

- label:

  string; which corresponds to section where the key is located

- key:

  string; name of key in which to extract the value

## Examples

``` r
if (FALSE) { # \dontrun{
get_config_value(model = "GOTM", file = "gotm.yaml", label = "turb_param", key = "k_min")
} # }
```
