% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingest.R
\name{ingest_bpv_statusses}
\alias{ingest_bpv_statusses}
\title{Ingest BPV (Internship) Status Data}
\usage{
ingest_bpv_statusses(
  ...,
  filename = NULL,
  path = NULL,
  config_key = "bpv_statusses",
  config_data_path = "data_raw_dir"
)
}
\arguments{
\item{...}{Additional arguments passed to readr::read_delim}

\item{filename}{Character string specifying the name of the CSV file to read}

\item{path}{Character string specifying the path to the CSV file}

\item{config_key}{Character string specifying the configuration key to use (default: "bpv_statusses")}

\item{config_data_path}{Character string specifying the config path for raw data (default: "data_raw_dir")}
}
\value{
A tibble containing processed internship status data with columns:
\itemize{
\item BPV_ID: Internship identifier
\item BPV_status_begin_datum: Status start date
\item BPV_status_eind_datum: Status end date
\item BPV_status: Status description
\item TEAM_ID: Team identifier
}
}
\description{
Reads and processes internship status data from a CSV file.
Expects CSV files with semicolon (;) as separator.
}
