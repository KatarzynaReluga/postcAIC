% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_constraints.R
\name{construct_constraints}
\alias{construct_constraints}
\title{Construct constraints}
\usage{
construct_constraints(degcAIC_models, cAIC_min, var_cov_terms, p)
}
\arguments{
\item{degcAIC_models}{Penalty for all considered models}

\item{cAIC_min}{Index of the selected model among models in the \code{modelset}}

\item{var_cov_terms}{Variance and covariance terms in each
model (appropriate elements from matrix Sigma)}

\item{p}{Number of all fixed parameters under consideration (intercept included)}
}
\value{
\item{list_constraints}{List with constraints}
}
\description{
Function \code{construct_constraints} provides a list of constraints
for multivariate normal distributions
}
\keyword{internal}
