% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingest.R
\name{ingest_employee_answers_satisfaction_codebook_2020}
\alias{ingest_employee_answers_satisfaction_codebook_2020}
\title{Ingest 2020 Employee Satisfaction Survey Codebook}
\usage{
ingest_employee_answers_satisfaction_codebook_2020(
  ...,
  filename = NULL,
  path = NULL,
  config_key = "codebook_employees_satisfaction_2020",
  config_data_path = "data_raw_dir"
)
}
\arguments{
\item{...}{Additional arguments passed to the underlying helper function.}

\item{filename}{Optional. A single string for the input file name.}

\item{path}{Optional. A single string for the input file path.}

\item{config_key}{Optional. A single string specifying the configuration key (defaults to "codebook_employees_satisfaction_2020").}

\item{config_data_path}{Optional. A single string specifying the data directory in config (defaults to "data_raw_dir").}
}
\value{
A processed data frame containing the employee satisfaction survey codebook data.
}
\description{
Load and process the 2020 employee satisfaction survey codebook data
}
