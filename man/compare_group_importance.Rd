% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_interpret.R
\name{compare_group_importance}
\alias{compare_group_importance}
\title{Compare Variable Importance Across Groups}
\usage{
compare_group_importance(
  interpretation_list,
  n_vars = 10,
  save = TRUE,
  path = NULL,
  use_friendly_names = TRUE
)
}
\arguments{
\item{interpretation_list}{A list of interpretation results from interpret_model()}

\item{n_vars}{Number of top variables to include (default: 10)}

\item{save}{Logical indicating whether to save the comparison (default: TRUE)}

\item{path}{Optional custom path to save the comparison}

\item{use_friendly_names}{Logical indicating whether to use user-friendly variable names (default: TRUE)}
}
\value{
A data frame with importance comparisons across groups
}
\description{
Compare variable importance across different program/level groups.
}
