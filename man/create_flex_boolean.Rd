% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare.R
\name{create_flex_boolean}
\alias{create_flex_boolean}
\title{Create Boolean Flex Status}
\usage{
create_flex_boolean(data)
}
\arguments{
\item{data}{A tibble containing a VERBINTENIS_is_flex_omschrijving column}
}
\value{
A tibble with additional VERBINTENIS_is_flex column:
\itemize{
\item VERBINTENIS_is_flex: Boolean indicating flex status
}
}
\description{
Converts flex status from text ("Ja") to boolean (TRUE/FALSE).
}
