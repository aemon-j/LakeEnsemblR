# Input values into json file

Inputs values into json file by locating the label and key within the
json file. Preserves comments (!) if present. NOTE: this does not use a
json parser so if there are json formatting errors this function will
not pick them up.

## Usage

``` r
input_json(file, label, key, value, out_file = NULL)
```

## Arguments

- file:

  filepath; to json file which you wish to edit

- label:

  string; which corresponds to section where the key is located

- key:

  string; name of key in which to input the value

- value:

  string; to be input into the key/value pair. Note boolean values must
  be input as 'true'/'false' as per the json format

- out_file:

  filepath; to write the output json file (optional); defaults to
  overwriting file if not specified

## Author

Tadhg Moore, Jorrit Mesman

## Examples

``` r
if (FALSE) { # \dontrun{
input_json(file = "samp.par", label = "ModelParameters", key = "f_wind", value = 1.2, out_file = NULL)
} # }
```
