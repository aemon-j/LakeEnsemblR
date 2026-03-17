# Extract values from yaml file

Extract values from yaml file, like gotmtools::get_yaml_value However,
an unlimited amount of keys can be provided. NOTE: this does not use a
yaml parser so if there are yaml formatting errors this function will
not pick them up.

## Usage

``` r
get_yaml_multiple(file = "gotm.yaml", ...)
```

## Arguments

- file:

  filepath; to yaml file which you wish to edit

- ...:

  string key1, key2, etc.: multiple keys pointing toward the line that
  you want to edit in the yaml file. Keys must be listed consecutively,
  without skipping numbers.

## Author

Jorrit Mesman

## Examples

``` r
if (FALSE) { # \dontrun{
get_yaml_multiple(file = "example.yaml", key1 = "streams", key2 = "inflow", key3 = "file")
} # }
```
