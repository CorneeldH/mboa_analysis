% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_run.R
\name{create_model_spec}
\alias{create_model_spec}
\title{Create Model Specification}
\usage{
create_model_spec(model_type = "random_forest", trees = 1000)
}
\arguments{
\item{model_type}{Type of model to create (e.g., "random_forest", "boost_tree")}

\item{trees}{Number of trees for ensemble models}
}
\value{
A model specification object
}
\description{
Create a model specification based on the selected model type.
}
