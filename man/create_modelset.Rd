% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_modelset.R
\name{create_modelset}
\alias{create_modelset}
\title{Create matrix selecting covariates in models}
\usage{
create_modelset(
  modelset = c("all_subsets", "nested", "part_subset"),
  common = NULL,
  p = 5
)
}
\arguments{
\item{modelset}{Type of model set:
\itemize{
\item \code{"all_subsets"} - all possible models using all covariates
\item \code{"nested"} - a sequence of nested models using all covariates
\item \code{"part_subset"} - all possible models using a subset of covariates
}}

\item{common}{A vector indicating variables forced to be
present in each model. Default: \code{NULL}}

\item{p}{Number of all covariates under consideration
(intercept included). Default: \code{5}}
}
\value{
\item{modelset_matrix}{Matrix composed of zeros and ones.
Ones correspond to covariates in a model which is represented in nth row.}
}
\description{
Function \code{create_modelset} creates a matrix indicating
covariates (fixed effects) in each model considered by the user
}
\details{
\describe{
\item{\code{modelset = "nested"}}{The first model contains one covariate from the first column of X,
then two covariates from the first and the second column of X, etc.}
\item{\code{common}}{A vector of length at most \code{p}, consisting of those column
numbers of X for which the covariates are forced to be present
in every model (e.g. \code{c(2,5)} for variables 2 and 5). If provided,
it is used in \code{model.set = "partsubsets"}.
}
}
}
\examples{
# "all_subsets"
modelset  = create_modelset("all_subsets", p = 5)

# "nested"
modelset  = create_modelset("nested", p = 5)

# "part_subset"
modelset  = create_modelset("part_subset", p = 5,
                             common = c(1:3))

}
