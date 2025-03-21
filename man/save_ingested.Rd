% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{save_ingested}
\alias{save_ingested}
\title{Save Ingested Data}
\usage{
save_ingested(
  data,
  ...,
  filename = NULL,
  path = NULL,
  config_data_path = "data_ingested_dir"
)
}
\arguments{
\item{data}{A data object to save.}

\item{...}{Additional arguments passed to saveRDS().}

\item{filename}{Optional. Name of the file to save. If NULL, uses the comment attribute of data.}

\item{path}{Optional. Path where to save the file.}

\item{config_data_path}{Optional. Config key for the save directory. Defaults to "data_ingested_dir".}
}
\value{
No return value. Saves the data as an RDS file.
}
\description{
Save processed data as an RDS file in a specified directory
}
\keyword{internal}
