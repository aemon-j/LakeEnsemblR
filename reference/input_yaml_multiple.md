# Inputs values into yaml file

Inputs values into yaml file, like gotmtools::input_yaml However, an
unlimited amount of keys can be provided. Preserves comments (#) if
present. NOTE: this does not use a yaml parser so if there are yaml
formatting errors this function will not pick them up.

## Usage

``` r
input_yaml_multiple(
  file = "gotm.yaml",
  value,
  out_file = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- file:

  filepath; to yaml file which you wish to edit

- value:

  string; to be input into the the yaml file. Note boolean values must
  be input as "true"/"false" as per the json format

- out_file:

  filepath; to write the output json file (optional); defaults to
  overwriting file if not specified

- verbose:

  Logical; output a message which values were replaced

- ...:

  string key1, key2, etc.: multiple keys pointing toward the line that
  you want to edit in the yaml file. Keys must be listed consecutively,
  without skipping numbers.

## Author

Jorrit Mesman

## Examples

``` r
if (FALSE) { # \dontrun{
input_yaml_multiple(file = "example.yaml", value = "something",
  key1 = "streams", key2 = "inflow", key3 = "file")
} # }
```
