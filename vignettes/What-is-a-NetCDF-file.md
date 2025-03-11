## What is a netCDF file?
Network Common Data Form (netCDF) is a file format for storing multidimensional scientific data (variables). From the [Unidata NCAR website](https://www.unidata.ucar.edu/software/netcdf/) Data in netCDF format is:
* Self-Describing. A netCDF file includes information about the data it contains.
* Portable. A netCDF file can be accessed by computers with different ways of storing integers, characters, and floating-point numbers.
* Scalable. Small subsets of large datasets in various formats may be accessed efficiently through netCDF interfaces, even from remote servers.
* Appendable. Data may be appended to a properly structured netCDF file without copying the dataset or redefining its structure.
* Sharable. One writer and multiple readers may simultaneously access the same netCDF file.
* Archivable. Access to all earlier forms of netCDF data will be supported by current and future versions of the software.

## Why use a netCDF file for LakeEnsemblR?
It was chosen as the main file format because of its ability to store multi-dimensional variables (e.g. time, depth, model) in a relatively small file size. The ability to store metadata is extremely useful to facilitate post-processing and its flexibility means as LakeEnsemblR grows to allow for parameter and meteorological ensemble runs, it will be easy to store all these variables within one relatively small file compared to multiple text files?

## How do I view a netCDF file?
We use the program PyNcView for viewing netCDF files. PyNcView is a cross-platform NetCDF viewer written in Python. It provides an easy-to-use graphical user interface to the creation of animations and publication-quality figures. It is free to download from SourceForge [here](https://sourceforge.net/projects/pyncview/).

## Do I _*HAVE*_ to work with netCDF files?
No you do not. We are aware that many users might prefer simple and easy to use output data so we have incorporated the functionality to output ".csv" files with [rLakeAnalyzer](https://github.com/GLEON/rLakeAnalyzer) formatting to allow the use of the functions from that package for loading and analysing the data within R. To do this you just need to change the "format" option in the LakeEnsmblr.yaml file. e.g.
```{}
output:
   file: ensemble_output                       # path of output file, excluding extension
   format: text                                # format [text, netcdf; default=netcdf]
   depths: 0.5                                 # depths to extract output [m; default=0.5]
   compression: 4                              # Set to an integer between 1 (least) and 9 (most)
   time_unit: hour                             # time unit [second, hour, day, dt=model time step; default=day]
   time_step: 24                               # number of time units between output [min=1; default=1]
   time_method: mean                           # treatment of time dimension [point=instantaneous, mean, integrated; default=point]
   variables:
      - temp
```

