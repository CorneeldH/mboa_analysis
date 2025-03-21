% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ingest.R
\name{ingest_attendance_observations}
\alias{ingest_attendance_observations}
\title{Ingest Enrollment Absence Information}
\usage{
ingest_attendance_observations(
  ...,
  filename = NULL,
  path = NULL,
  config_key = "attendance_observations",
  config_data_path = "data_raw_dir"
)
}
\arguments{
\item{...}{Additional arguments passed to readr::read_delim}

\item{filename}{Optional character string specifying the name of the file to read.
If NULL, filename is retrieved from config using config_key.}

\item{path}{Optional character string specifying the path to read from.
If NULL, path is retrieved from config using config_data_path.}

\item{config_key}{Character string specifying the configuration key to use.
Defaults to "attendance_observations".}

\item{config_data_path}{Character string specifying the config key for raw data directory.
Defaults to "data_raw_dir".}
}
\value{
A tibble containing processed absence data with columns:
\itemize{
\item SK_GroepInschrijving: Group enrollment identifier
\item SK_Leereenheid: Learning unit identifier
\item Datum: Date of the absence record
\item SK_afspraak: Appointment identifier
\item Waarnemingsduur: Duration of observation
\item Presentietekst: Attendance status text
}
}
\description{
Reads and processes enrollment absence data from a CSV file, selecting relevant columns
for tracking student attendance and absences.
}
\details{
The function loads raw data using the load_data function and selects specific columns
for absence tracking. The original config_key is preserved as a comment in the
returned data for traceability.
}
