% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enrich.R
\name{create_total_fte}
\alias{create_total_fte}
\title{Create Total FTE}
\usage{
create_total_fte(data)
}
\arguments{
\item{data}{A data frame containing columns MEDEWERKER_contract_fte and
MEDEWERKER_contract_fte_aanpassing}
}
\value{
A data frame with an additional column MEDEWERKER_contract_fte_totaal
}
\description{
Calculate the total FTE by adding contract FTE and FTE adjustments
}
