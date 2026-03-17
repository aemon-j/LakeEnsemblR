# Extracts values from a json file

Extracts values from a json file by locating the label and key within
the json file.

## Usage

``` r
get_json_value(file, label, key)
```

## Arguments

- file:

  filepath; to json file

- label:

  string; which corresponds to section where the key is located

- key:

  string; name of key in which to extract the value

## Author

Tadhg Moore, Jorrit Mesman

## Examples

``` r
if (FALSE) { # \dontrun{
input_json(file = "samp.par", label = "ModelParameters", key = "f_wind", value = 1.2, out_file = NULL)
} # }
```
