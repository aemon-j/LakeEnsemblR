% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_ensemble.R
\name{run_ensemble}
\alias{run_ensemble}
\title{Run Ensemble of lake models}
\usage{
run_ensemble(
  config_file,
  model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
  folder = ".",
  verbose = FALSE,
  parallel = FALSE,
  ncores = NULL,
  return_list = FALSE,
  create_output = TRUE,
  add = FALSE
)
}
\arguments{
\item{config_file}{filepath; to LakeEnsemblr yaml master config file}

\item{model}{vector; model to export driving data. Options include
c('GOTM', 'GLM', 'Simstrat', 'FLake', 'MyLake')}

\item{folder}{filepath; to folder which contains the model folders generated by export_config()}

\item{verbose}{Boolean; Should model output be shown in the console. Defaults to FALSE}

\item{parallel}{Boolean; should the model calibration be parallelized}

\item{ncores}{integer; number of cores to use for parallelization. Defaults 
to NULL. If NULL then the number of cores used is either the number of models
selecte or the number of cores on the machine, whichever is smaller.}

\item{return_list}{boolean; Return a list of dataframes of model output. Defaults to FALSE}

\item{create_output}{boolean; Create ensemble output file otherwise it just runs the models and
generates model output in their respective folders. Defaults to TRUE}

\item{add}{boolean; Add results to an existing netcdf file with new dimension "member"}
}
\description{
Run each of the lake models
}
