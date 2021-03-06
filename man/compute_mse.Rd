% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_mse.R
\name{compute_mse}
\alias{compute_mse}
\title{Compute MSE}
\usage{
compute_mse(C_cluster, X, sig_u, sig_e, clusterID, model = "NERM")
}
\arguments{
\item{C_cluster}{Cluster-level covariates for fixed and random parameters}

\item{X}{Matrix with covariates for fixed effects}

\item{sig_u}{Variance parameter of random effects}

\item{sig_e}{Variance parameter of errors}

\item{clusterID}{Vector with cluster labels}

\item{model}{Model we want to fit. Only NERM is supported for now.}
}
\value{
\item{mse}{MSE of mixed effects}
}
\description{
Function \code{compute_mse} provides first-order correct MSE for mixed effects
}
\examples{
n = 10
m_i = 5
m_total = 50

clusterID = rep(1:n, m_i)
p = 10
beta = rep(2, p)
u_i = rnorm(n, 0, 2)
u_i_aug = rep(u_i, each = m_i)
X = matrix(rnorm(m_total * p), m_total, p)
y = X\%*\%beta + u_i_aug + rnorm(m_total, 0, 1)
fit_nerm <- estimate_NERM(X = X, y = y,
                          clusterID = clusterID,
                          X_cluster = NULL)

C_cluster = cbind(X[1:10, ], diag(n))

mse_first = compute_mse(C_cluster, X,
                        sig_u = fit_nerm$sig_u,
                        sig_e = fit_nerm$sig_e,
                        clusterID = clusterID)

}
