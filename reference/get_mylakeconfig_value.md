# Extracts values from the MyLake config file

Extracts values from the MyLake config file by locating the label and
key within the file.

## Usage

``` r
get_mylakeconfig_value(file, label, key)
```

## Arguments

- file:

  filepath; to R object (loaded Rdata file)

- label:

  string; which corresponds to section where the key is located

- key:

  string; name of key in which to extract the value
