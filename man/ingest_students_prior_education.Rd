% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingest.R
\name{ingest_students_prior_education}
\alias{ingest_students_prior_education}
\title{Ingest Prior Education Data}
\usage{
ingest_students_prior_education(
  ...,
  filename = NULL,
  path = NULL,
  config_key = "students_prior_education",
  config_data_path = "data_raw_dir"
)
}
\arguments{
\item{...}{Additional arguments passed to readr::read_delim}

\item{filename}{Character string specifying the name of the CSV file to read}

\item{path}{Character string specifying the path to the CSV file}

\item{config_key}{Character string specifying the configuration key to use (default: "enrollments_prior_education")}

\item{config_data_path}{Character string specifying the config path for raw data (default: "data_raw_dir")}
}
\value{
A tibble containing processed prior education data with columns:
\itemize{
\item DEELNEMER_ID: Student identifier
\item Additional columns with prefix DEELNEMER_vooropleiding_
}
}
\description{
Reads and processes prior education data from a CSV file.
}
\details{
The function standardizes column names by:
\enumerate{
\item Converting all names to lowercase using clean_names
\item Renaming id_deelnemer to DEELNEMER_ID
\item Prefixing remaining columns with DEELNEMER_vooropleiding_
}
}
