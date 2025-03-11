```{=html}
<h2>
```
Version 1.1

```{=html}
</h2>
```
In March 2022, we updated LakeEnsemblR to version 1.1. The main
advantage of this new version is the possibility to use multiple inflows
and outflows at specified depths in your simulated lake or reservoir.
Moreover, these inflows and outflows no longer have to balance each
other out, and in the models that support this (Simstrat, GLM, GOTM) you
can therefore now also simulate varying water levels. Simstrat has been
updated to a later version (v3.0.1) and there have been other bug fixes
as well, for example regarding plotting heatmaps in lakes with varying
water level, or in the YAML-parser functions.

However, some of these improvements required a change in the
configuration file, which may cause unexpected errors when updating
LakeEnsemblR to v1.1. Here we describe how to avoid or fix these
problems, and we also give instructions on how you can keep working with
v1.0, if you prefer this.

As a general advice, please first reinstall the latest versions of the
model-specific packages (GLM3r, SimstratR, etc.) after updating
LakeEnsemblR. Also, new example test cases and configuration files are
provided in LakeEnsemblR, the model-specific packages, and at the
[LER_examples Github page](https://github.com/aemon-j/LER_examples). The
advice below is therefore mostly if you want to adapt your existing
setup to v1.1.

```{=html}
<h3>
```
Changes in the LakeEnsemblR configuration file

```{=html}
</h3>
```
```{=html}
<h4>
```
`<em>`{=html}inflows`</em>`{=html} and
`<em>`{=html}outflows`</em>`{=html} sections

```{=html}
</h4>
```
In v1.0, there was a single `<em>`{=html}inflows`</em>`{=html} section,
but now inflows and outflows are split into two sections.

-   "use": Each section has the "use" argument; if set to "false", no
    inflows or outflows are used and the rest of the section is not
    read.
-   "file": If use = true, provide the file where inflow discharge,
    temperature, and salinity, or outflow discharge are provided. Note
    that multiple flows can be provided in this file, by adding the
    suffixes "\_1", "\_2", "\_3", etc. See
    `<code>`{=html}get_template("Inflow")`</code>`{=html} for an example
    inflow file with two inflows, or
    `<code>`{=html}get_template("Outflow")`</code>`{=html} for an
    example outflow file with a single outflow.
-   "number_inflows" and "number_outflows": The number of inflows or
    outflows to use.
-   "outflow_lvl" (`<em>`{=html}outflows`</em>`{=html}-only): The height
    of the outflows, relative to the deepest point in the lake. If the
    outflow is a surface outflow use "--1". If there are more than one
    outflow in the outflow file, this must be a list with one value per
    outflow, e.g.:
        <code>  outflow_lvl:
          - -1
          - 10
        </code>

    Inflows are always assumed to be surface inflows.
-   "fix_wlvl" (`<em>`{=html}inflows`</em>`{=html}-only and optional):
    If set to "true", the water level in GOTM will be fixed to the
    initial water level. This reproduces v1.0 behaviour. However, note
    that this means that the water balance is no longer correctly
    resolved in GOTM. If this argument is absent, it is assumed to be
    "false"

Note that the "mass_balance" argument from v1.0 is no longer used. If
you want to recreate the situation of v1.0 where inflow = outflow, you
can just provide the same file name in the "file" argument of both the
inflows and outflows section. Also the "scale_param" parameter from the
v1.0 `<em>`{=html}inflows`</em>`{=html} section has been (re)moved (see
next paragraph).

```{=html}
<h4>
```
Inflow and outflow scaling factors

```{=html}
</h4>
```
If you want to scale the discharge in your inflows or outflows (for
example if your measured streamflow only accounts for part of the
catchment runoff), you need to put this in the
`<em>`{=html}scaling_factors`</em>`{=html} section. It's important to
note that you need to provide multiple scaling factors if you have
multiple inflows or outflows, in a list-format. As with other scaling
factors, model-specific scaling factors overwrite generic ("all")
scaling factors. Here is an example for a reservoir with a single inflow
and two outflows, in which the first (surface) outflow in Simstrat has a
10% higher discharge:

    <code>scaling_factors:
       all:
          inflow: 1.0
          outflow:
          - 1.0
          - 1.0
       Simstrat:
          outflow:
          - 1.1
          - 1.0
    </code>

```{=html}
<h4>
```
The `<em>`{=html}output`</em>`{=html} section

```{=html}
</h4>
```
This section has remained largely the same, but with two small updates.
The most important one is that "w_level" is now a potential output of
the models, to be listed under "variables". Note that for MyLake and
FLake, this is set to `<code>`{=html}NA`</code>`{=html}, because these
models do not simulate water level, although you can assume water level
in these models to be equal to the model parameters
`<code>`{=html}In.Z`</code>`{=html} and
`<code>`{=html}depth_w\_lk`</code>`{=html}, respectively.

A second update is that you can provide "max_members", which is the
maximum number of members that you can add to the same netcdf file using
the `<code>`{=html}add = TRUE`</code>`{=html} argument in
`<code>`{=html}run_ensemble()`</code>`{=html}. If it is not provided, it
is assumed to be 25.

```{=html}
<h3>
```
Changes in the Simstrat configuration file

```{=html}
</h3>
```
A new version of Simstrat is being used (v3.0.1), which included some
changes in the names of parameters. The easiest solution to this problem
will be to remove your simstrat.par file and run export_config() again,
which will create a new file based on the template in SimstratR. If you
want to make the changes manually:

-   p_albedo -\> p_sw_ice
-   p_sw -\> p_sw_water
-   InflowPlacement -\> InflowMode. However, also the options changed:
    Old 0 = New 1, Old 1 = New 2, New 0 = no inflows or outflows used.

```{=html}
<h3>
```
How do the different models handle the water balance?

```{=html}
</h3>
```
If you use inflows and outflows of different intensity and then look at
the "w_level" output of LakeEnsemblR, you will probably notice that each
model handles the water balance differently. Here we quickly mention the
differences. These differences are inherent to the models, and
LakeEnsemblR does not attempt to standardise this. However, if you want
to create a similar water level between the models, you could for
example change the scaling factors of the inflow or the outflow.

-   FLake: There is no water balance, as only the heat transferred by
    the inflow is added to the model. w_level output is therefore "NA".
-   GLM: water balance = delta-discharge + precipitation + evaporation +
    overflow∗
-   GOTM∗∗: water balance = delta-discharge + precipitation +
    evaporation
-   Simstrat: water balance = delta-discharge + overflow∗
-   MyLake: Outflow discharge is assumed to be equal to the inflow
    discharge, and no other flows are computed. w_level output is
    therefore "NA".

∗With "overflow" we mean that if the water level exceeds the highest
level in the hypsograph, the excess water is removed from the model.\
∗∗In GOTM, there are also other options of computing the water balance,
for example by forcing the water level to a prescribed time series.
These options are not supported by LakeEnsemblR, but can still be used
by accessing the gotm.yaml file.

```{=html}
<h3>
```
What if I want to keep working with v1.0?

```{=html}
</h3>
```
Perhaps you have already set up LakeEnsemblR v1.0 in your lake or
reservoir, and you don't want this update to affect your simulations.
Because LakeEnsemblR and the model-specific packages have been attached
to a specific "release" in GitHub, you can still install these old
versions. These are the versions that are described in the publication
Moore et al. (2021), and you will still be able to use your old
workflow.

    <code>devtools::install_github("aemon-j/LakeEnsemblR@v1.0.0")
    devtools::install_github("aemon-j/FLakeR@LakeEnsemblR_v1.0.0")
    devtools::install_github("aemon-j/GLM3r@LakeEnsemblR_v1.0.0")
    devtools::install_github("aemon-j/GOTMr@LakeEnsemblR_v1.0.0")
    devtools::install_github("aemon-j/SimstratR@LakeEnsemblR_v1.0.0")
    devtools::install_github("aemon-j/MyLakeR@LakeEnsemblR_v1.0.0")</code>

To access the LER_examples corresponding to v1.0 of LakeEnsemblR,
download [this zip
file](https://github.com/aemon-j/LER_examples/releases/tag/v1.0.0).

Alternatively, you can install v1.1 and the new model-specific packages,
change your configuration file as described above, set the outflow file
to the same file as the inflow file, and "fix_wlvl" to "true". This
should result in very similar behaviour compared to v1.0 (although a
later version of the Simstrat model will be used).
