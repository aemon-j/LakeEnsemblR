% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_flow.R
\name{format_outflow}
\alias{format_outflow}
\title{Format outflow data for each model}
\usage{
format_outflow(outflow, model, config_file, folder = ".")
}
\arguments{
\item{outflow}{dataframe; as read.csv() from standardised input.}

\item{model}{character; Model for which scaling parameters will be applied. Options include
c('GOTM', 'GLM', 'Simstrat', 'FLake')}

\item{config_file}{filepath; To LER config yaml file. Only used if model = 'GOTM'}

\item{folder}{filepath; to folder which contains the model folders generated by export_config()}
}
\value{
dataframe of met data in the model format
}
\description{
Format dataframe into shape for the specified model
}
