% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_prepare.R
\name{prepare_model_data}
\alias{prepare_model_data}
\title{Prepare Data for Predictive Modeling}
\usage{
prepare_model_data(
  data,
  outcome_var = "DEELNEMER_BC_uitval",
  program_var = "OPLEIDING_naam_kort",
  level_var = "VERBINTENIS_niveau",
  cohort_var = "COHORT_startjaar",
  program_filter = NULL,
  level_filter = NULL,
  test_cohort = 2023,
  week_vars = "none",
  save = TRUE,
  path = NULL
)
}
\arguments{
\item{data}{A data frame containing the raw data}

\item{outcome_var}{The name of the outcome variable (default: "DEELNEMER_BC_uitval")}

\item{program_var}{The name of the program variable (default: "OPLEIDING_naam_kort")}

\item{level_var}{The name of the education level variable (default: "VERBINTENIS_niveau")}

\item{cohort_var}{The name of the cohort year variable (default: "COHORT_startjaar")}

\item{program_filter}{Optional character vector of programs to include}

\item{level_filter}{Optional numeric vector of education levels to include}

\item{test_cohort}{The cohort year to use as test data (default: 2023)}

\item{week_vars}{Strategy for handling week variables: "none" = exclude all, "early" = keep weeks 1-5, "all" = keep all (default: "none")}

\item{save}{Logical indicating whether to save the prepared data (default: TRUE)}

\item{path}{Optional custom path to save the data}
}
\value{
A list containing the prepared training and test datasets
}
\description{
Master function to prepare data for modeling by filtering, encoding, and splitting.
This function handles all preprocessing steps required before model training.
}
