% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrich.R
\name{calculate_exam_plan_to_start}
\alias{calculate_exam_plan_to_start}
\title{Calculate Days Between Exam Plan Dates and Start Date}
\usage{
calculate_exam_plan_to_start(data)
}
\arguments{
\item{data}{A data frame containing exam plan dates and cohort start dates}
}
\value{
A data frame with additional columns calculating days between exam plan dates
and cohort start date. Values outside the range of -365 to 365 days or infinite
values are converted to NA.
}
\description{
Calculate the number of days between various exam plan dates and the cohort start date
}
