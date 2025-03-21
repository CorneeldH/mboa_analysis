% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{summarise_employee_absence_in_days_to_year}
\alias{summarise_employee_absence_in_days_to_year}
\title{Summarize Employee Absence Data by Year}
\usage{
summarise_employee_absence_in_days_to_year(employees_absences_in_days)
}
\arguments{
\item{employees_absences_in_days}{A data frame containing daily employee absence records
with columns for employee ID (MEDEWERKER_ID), school year (SCHOOLJAAR_naam),
absence percentage (verzuim_percentage), and absence duration category (verzuim_duur).}
}
\value{
A data frame summarizing yearly absence metrics per employee, including:
\itemize{
\item Total days in year
\item Absolute absence days and percentages for long-term absences
\item Absolute absence days and percentages for medium-term absences
\item Absolute absence days and percentages for short-term absences
}
}
\description{
Calculate yearly summaries of employee absences, breaking down absence percentages
by duration category (short, medium, and long-term).
}
