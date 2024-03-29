% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_model_parameters.R
\name{export_model_parameters}
\alias{export_model_parameters}
\title{Export model-specific parameters}
\usage{
export_model_parameters(
  config_file,
  model = c("GOTM", "GLM", "Simstrat", "FLake", "MyLake"),
  folder = "."
)
}
\arguments{
\item{config_file}{name of the master LakeEnsemblR config file}

\item{model}{vector; model to export configuration file.
Options include c("FLake", "GLM, "GOTM", "Simstrat", "MyLake")}

\item{folder}{folder}
}
\description{
Exports model-specific parameters that are specified in the model_parameters
 section of the master config file.
}
\keyword{methods}
