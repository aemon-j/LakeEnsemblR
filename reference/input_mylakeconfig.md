# Inputs value into the MyLake config file

Inputs value into the MyLake config file by locating the label and key
within the file.

## Usage

``` r
input_mylakeconfig(file, label, key, value, out_file = NULL)
```

## Arguments

- file:

  filepath; to R object (loaded Rdata file)

- label:

  string; which corresponds to section where the key is located

- key:

  string; name of key in which to extract the value

- value:

  string; name of key in which to extract the value

- out_file:

  filepath; to write the output config file (optional); defaults to
  overwriting file if not specified
