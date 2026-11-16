#' Fit NERM at given variance parameters
#'
#' Function \code{fit_NERM_given_theta} computes the generalized least squares
#' estimator of the fixed effects and the empirical best linear unbiased
#' predictors of the random and mixed effects under NERM, with the variance
#' parameters held fixed at the given values. It is used to fit every
#' candidate model at the estimate of the variance parameters obtained
#' from the full model.
#'
#' @inheritParams estimate_NERM
#' @param sig_u Variance parameter of random effects
#' @param sig_e Variance parameter of errors
#'
#' @return List with the same elements as \code{estimate_NERM}:
#' \item{fit_model_fixed}{Fitted values using fixed effects}
#' \item{fit_model_mixed}{Fitted values using fixed and random effects}
#' \item{sig_u}{Variance parameter of random effects (as given)}
#' \item{sig_e}{Variance parameter of errors (as given)}
#' \item{beta}{Fixed effects (regression parameters)}
#' \item{u}{Random effects}
#' \item{mu}{Mixed effects}
#'
#' @details
#' For cluster \eqn{i} with \eqn{m_i} units, write
#' \eqn{\gamma_i = m_i \sigma^2_u / (\sigma^2_e + m_i \sigma^2_u)}. Then
#' \eqn{V_i^{-1} = (I - \gamma_i m_i^{-1} J)/\sigma^2_e}, so that
#' \eqn{X^T V^{-1} X = \{X^T X - \sum_i \gamma_i m_i \bar{x}_i \bar{x}_i^T\}/\sigma^2_e}
#' and \eqn{\hat{u}_i = \gamma_i (\bar{y}_i - \bar{x}_i^T \hat{\beta})}.
#' At the variance estimates of a model fitted by \code{estimate_NERM},
#' this reproduces the fixed effects and random effects of that fit.
#' Clusters are ordered as the levels of \code{clusterID}.
#'
#' @keywords internal
#'

fit_NERM_given_theta <- function(X,
                                 y,
                                 clusterID,
                                 sig_u,
                                 sig_e,
                                 X_cluster = NULL) {
  X = as.matrix(X)
  y = as.vector(y)
  clusterID = droplevels(validate_observations(clusterID, X, cluster = TRUE))

  # Cluster sizes, shrinkage factors and cluster means ---------------------
  m_i = as.vector(table(clusterID))
  gamma = m_i * sig_u / (sig_e + m_i * sig_u)

  X_bar = rowsum(X, clusterID) / m_i
  y_bar = as.vector(rowsum(y, clusterID)) / m_i

  # GLS estimator at (sig_u, sig_e) ----------------------------------------
  XtVinvX = (crossprod(X) - crossprod(X_bar * sqrt(gamma * m_i))) / sig_e
  XtVinvy = (crossprod(X, y) - crossprod(X_bar, gamma * m_i * y_bar)) / sig_e
  beta = as.vector(solve(XtVinvX, XtVinvy))
  names(beta) = colnames(X)

  # BLUP of random effects and fitted values -------------------------------
  u = gamma * (y_bar - as.vector(X_bar %*% beta))

  fit_model_fixed = as.vector(X %*% beta)
  fit_model_mixed = fit_model_fixed + u[as.integer(clusterID)]

  # Mixed effects --------------------------------------------------------
  if (is.null(X_cluster)) {
    X_cluster = X_bar
  } else {
    X_cluster = as.matrix(X_cluster)
    stopifnot(
      "Number of columns in 'X_cluster' has to be the same as p" = NCOL(X_cluster) == length(beta)
    )
    stopifnot(
      "Number of rows in 'X_cluster' has to be the same as number of clusters" = NROW(X_cluster) == length(u)
    )
  }
  mu = X_cluster %*% beta + u

  output <- list(
    fit_model_fixed = fit_model_fixed,
    fit_model_mixed = fit_model_mixed,
    sig_u = sig_u,
    sig_e = sig_e,
    beta = beta,
    u = u,
    mu = mu
  )

  output
}
