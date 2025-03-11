**Why does FLake not simulate the full depth of the lake?**

-   FLake assumes a rectangular shape of the basin and does not use the
    hypsograph file. The assumptions of FLake match best when using the
    mean depth of the lake. This is calculated from the hypsograph file
    inside *export_location*. So shallower depth of FLake is intended.
-   Should you really wish to simulate the full depth of the lake, you
    can add "depth_w\_lk:" to the "FLake" section of the
    "model_parameters" section in the LER config file, with the maximum
    depth of the lake.

**Why am I getting the error "Error in get_meteo_time_step(...):Meteo
file has irregular time steps!"?**

-   This error is thrown if the program registers irregular time steps
    in your meteo file. It would be good to check if this is indeed the
    case, but it can also be caused by not using the datetime format
    *yyyy-mm-dd HH:MM:SS*, in which case R reads the date in an
    incorrect manner.
-   On many computers, Microsoft Excel is used as default when opening
    .csv files. Some versions of Excel automatically convert *yyyy-mm-dd
    HH:MM:SS* to *dd/mm/yyyy HH:MM*, which can cause this error. This
    can either be fixed **a)** in Excel by right-clicking the datetime
    cells, going to "Format Cells", and then entering in the "Type" box:
    yyyy-mm-dd hh:mm:ss (this needs to be done every time opening the
    file in Excel and will be your local timezone) or **b)** by not
    using Excel and opening your input files in R or a text editor.
-   Another common reason for this error is the absence of leap years.
    Some weather models give output always in 365 days a year without
    considering leap years, which will result in this error. So you
    would need to insert the missing leap days and fill in the missing
    forcing data on those dates (for example by interpolation).

**What parameters can I enter in the model_parameters section of the
LakeEnsemblR config file?**

-   You can specify any parameter that occurs in the model-specific
    configuration file of that model (check the model-specific
    directories). Functions inside LER will try to find the line where
    this parameter occurs and replace the value by the value you
    requested.
-   You can either add one keyword, or two, split by "/" (e.g.
    "turb_params/k_min"). The first keyword is used to look for the
    section in which the parameter occurs, which can come in handy if
    the same parameter occurs in multiple sections.
-   It is not yet possible to enter more than two keywords.
-   A warning is thrown if the parameter cannot be found.

**Can I enter a light-extinction coefficient (Kw) that varies over
time?**

-   Yes, you can either add a value (i.e. constant over time) or a file
    name, in which you can vary Kw over time. Type
    *LakeEnsemblR::get_template("Light extinction")* to get the right
    format.

**Can my outflow be different from my inflow?**

-   This was not possible in version 1.0 of LER. You could manually
    change the values of in- and outflow in the model-specific
    directories, but LER did not assist in this.
-   In version 1.1 of LER, you can now vary inflows and outflows
    independently from each other. For more information, see [our
    wiki](https://github.com/aemon-j/LakeEnsemblR/wiki/From-v1.0-to-v1.1).

**How do I add additional members to my netcdf file?**

-   When you call *run_ensemble*, add the argument *add = TRUE*. This
    will add the run to the existing netcdf as a new member.

**Why do I get the error "Error in master_config config_files: subscript
out of bounds"?**

-   This is most likely due to an error in the yaml-format of the LER
    configuration file. You can't use tabs in a yaml file, and the
    indentation needs to be consistent (in the LER template, three
    spaces per "level" are used). You can check if this is the case by
    running configr::read.config(config_file). If this returns "FALSE",
    something is wrong in the yaml format.
-   If this is the case, search for use of tabs (although you should
    receive a warning that tabs occur when running export_config), or an
    inconsistent use of spacing. A relatively easy way to locate the
    issue is to comment out the whole yaml file (in Notepad, right-click
    and click "Block comment"). Then start from the top, uncomment one
    section at a time, and try to run configr::read.config(config_file).
    The section where it starts to return "FALSE", is the section where
    the issue is.

**Why do I get the error "Error in signif(deps, 4) : non-numeric
argument to mathematical function"?**

-   This could be caused by using the wrong headers in your initial
    temperature profile file. Ensure the file is in .csv format and has
    the headers `<code>`{=html}Depth_meter,
    Water_Temperature_celsius`</code>`{=html}

**Why is my MyLake run failing?**

-   An example of an error message is: "Error in polyroot(c(a, b, c)) :
    invalid polynomial coefficient"
-   For some combinations of parameters, MyLake can become unstable and
    crash. While it is sometimes hard to figure out what makes it
    unstable, we found that especially the C_shelter parameter is rather
    sensitive. Automatic calculation of C_shelter could lead to unstable
    calculations. As an initial guess, you could try to use a value of
    0.15, by adding to the MyLake part of the "model_parameters" section
    in the LER config file "Phys.par/C_shelter: 0.15"

**Simstrat runs well, but occasionally becomes instable and simulates
extremely high or low temperatures. What can I do?**

-   If you plot the results of Simstrat, the high maximum and low
    minimum values can cause an almost unicoloured plot.
-   The maintainer of the Simstrat code, Fabian Baerenbold, advises to
    use a small time step for Simstrat when using in-/outflows,
    potentially smaller than for other models.\
    Usually 5 minutes (300 seconds) is enough, but sometimes an even
    shorter time step is required. To do this in LakeEnsemblR, you can
    add `<code>`{=html}Simulation/Timestep s: 300`</code>`{=html} to the
    Simstrat part of the "model_parameters" section of the configuration
    file before running `<code>`{=html}export_config`</code>`{=html}.

**My setup was working well, but I get errors after updating to version
1.1 (March 2022). What should I do?**

-   If you want to use the new version, please don't forget to also
    reinstall the model-specific packages (GLM3r, SimstratR, etc.) and
    to use a LER config file that corresponds to the new template, e.g.
    `<code>`{=html}get_template("LakeEnsemblR_config")`</code>`{=html}.
    More information can be found on [our
    wiki](https://github.com/aemon-j/LakeEnsemblR/wiki/From-v1.0-to-v1.1).
-   If you would like to keep using the old version, please also have a
    look at the [wiki
    page](https://github.com/aemon-j/LakeEnsemblR/wiki/From-v1.0-to-v1.1)
    for instructions.
-   If you are still experiencing problems, don't hesitate to create a
    new Issue on this GitHub.
