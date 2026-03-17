# Copies LakeEnsemblR template from package to specified folder

Copies LakeEnsemblR template from package to specified folder to be used
as example for setting up a simulation.

## Usage

``` r
get_template(template = NULL, folder = ".", filename = NULL, overwrite = FALSE)
```

## Arguments

- template:

  string; name of file that you want to get the template for. In case it
  is NULL, all potential options are given. "All" returns all templates.

- folder:

  filepath; filepath where to copy the templates to

- filename:

  string; filename (including .filetype). Default NULL, for the standard
  name

- overwrite:

  boolean; overwrite existing files
