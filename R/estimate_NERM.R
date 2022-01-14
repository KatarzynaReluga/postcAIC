#' Estimate nested error regression model (NERM)
#'
#' Estimate NERM, store fixed and random parameters and variances
#'
#' @param X Matrix with covariates for fixed effects
#' @param y Vector of responses
#' @param clusterID Vector with cluster labels
#' @param X_cluster Matrix with cluster level covariates for fixed effects.
#' Default: \code{NULL}.
#'
#' @return List with parameters:
#' * \code{fit_model_fixed} - estimated model using fixed effects
#' * \code{fit_model_mixed} - estimated model using fixed and random effects
#' * \code{sig_u} - variance parameter of random effects
#' * \code{sig_e} - variance parameter of errors
#' * \code{beta} - fixed effects (regression parameters)
#' * \code{u} - random effects
#' * \code{mu} - mixed effects
#'
#' @importFrom nlme lme VarCorr
#' @importFrom stats aggregate
#'
#' @examples
#' n = 10
#' m_i = 5
#' m_total = 50
#'
#' clusterID = rep(1:5, n)
#' p = 10
#' beta = rep(2, p)
#' u_i = rnorm(n, 0, 2)
#' u_i_aug = rep(u_i, each = m_i)
#' X = matrix(rnorm(m_total * p), m_total, p)
#' y = X%*%beta + u_i_aug + rnorm(m_total, 0, 1)
#' fit_nerm <- estimate_NERM(X = X, y = y,
#'                           clusterID = clusterID,
#'                           X_cluster = NULL)
#'
#' @export
#'

estimate_NERM <- function(X, y, clusterID,
                          X_cluster = NULL) {
  # Data pre-processing ---------------------------------------------------
  X = format_data_matrix(X)
  m_total = length(y)
  dataset = cbind(y, X, clusterID)
  
  #Fit NERM ------------------------------------------------------------
  fit_model = lme(y ~ X - 1, random = ~ 1 | clusterID)
  
  # Fitted model with and without random effects ------------------------
  fit_model_fixed = fit_model$fitted[, 1]
  fit_model_mixed = fit_model$fitted[, 2]
  
  # Fixed effects and random effects ------------------------------------
  beta = unlist(fit_model$coefficients$fixed)
  p = length(beta)
  u = unlist(fit_model$coefficients$random)
  n = length(u)
  
  # Variance parameters for NERM ----------------------------------------
  sig_u = as.numeric(VarCorr(fit_model)[1, 1])
  sig_e = (summary(fit_model)$sigma) ^ 2
  
  #Cluster-level parameter -------------------------------------------
  if (is.null(X_cluster)) {
    dataset_cluster = data.frame(X = X, clusterID)
    X_cluster = aggregate(X ~ clusterID, data = dataset_cluster, FUN = "mean")[,-1]
    t_X_cluster = t(X_cluster)
    mu = crossprod(t_X_cluster, beta) + u
  } else {
    stopifnot("Number of columns in 'X_cluster' has to be the same as p" = NCOL(X_cluster) == length(beta))
    
    stopifnot("Number of rows in 'X_cluster' has to be the same as number of clusters" = NROW(X_cluster) == n)
    t_X_cluster = t(X_cluster)
    mu = crossprod(t_X_cluster, beta) + u
    
  }
  
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
