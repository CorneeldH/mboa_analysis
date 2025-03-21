% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{add_helper_variables}
\alias{add_helper_variables}
\title{Add Helper Variables to Employee Satisfaction Data}
\usage{
add_helper_variables(employee_answers_satisfaction)
}
\arguments{
\item{employee_answers_satisfaction}{A data frame containing employee satisfaction survey responses.}
}
\value{
A data frame with additional columns:
\itemize{
\item group_number: A unique identifier for each group
\item appearance_number: Sequential numbering within each group
}
}
\description{
Adds grouping and appearance variables to employee satisfaction survey data
}
