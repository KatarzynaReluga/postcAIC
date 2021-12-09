#' Estimate NERM
#'
#' Estimate NERM, store fixed and random parameters and variances
#'
#' @param X Matrix with fixed effects covariates
#' @param y Vector of responses
#' @param clusterID Vector with cluster labels
#' @param X_cluster Matrix with cluster level covariates for fixed effects 
#'
#' @return List with parameters
#' * \code{fit_model_fixed} - estimated model using fixed effects 
#' * \code{fit_model_mixed} - estimated model using fixed and random effects 
#' * \code{sig_u} - variance parameter of random effects
#' * \code{sig_e} - variance parameter of errors
#' * \code{R} - covariance matrix of errors
#' * \code{invR} - inverse of covariance matrix of errors
#' * \code{detR} - determinant of covariance matrix of errors
#' * \code{G} - covariance matrix of random effects
#' * \code{V} - covariance matrix of response
#' * \code{invV} - inverse of covariance matrix of response
#' * \code{detV} - determinant of covariance matrix of response
#' * \code{beta} - fixed effects (regression parameters)
#' * \code{u} - random effects
#' * \code{mu} - mixed effects
#'
#'
#' @importFrom nlme lme VarCorr
#' @importFrom data.table data.table
#' @importFrom Matrix bdiag det Matrix
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
#' fit_nerm <- estimate_NERM(X = X, y = y, clusterID = clusterID, X_cluster = NULL)
#'
#' @export

estimate_NERM <- function(X = NULL, y = NULL, clusterID = NULL,
                         X_cluster = NULL) {
  # Fromat X
  X = format_data_matrix(X)

  # Validate and format data matrix X, y and clusterID
  m_total = length(y)

  # Create a dataset
  dataset = cbind(y, X, clusterID)

  #Fit the model

  fit_model = lme(y ~ X -1, random = ~ 1|clusterID)

  # Fitted model with and without random effects ------------------------------
  fit_model_fixed = fit_model$fitted[, 1]
  fit_model_mixed = fit_model$fitted[, 2]

  # Fixed effects and random effects ------------------------------------
  beta = unlist(fit_model$coefficients$fixed)
  p = length(beta)
  u = unlist(fit_model$coefficients$random)
  n = length(u)

  # Variance parameters for NERM -----------------------------------
  sig_u = as.numeric(VarCorr(fit_model)[1, 1])
  sig_e = (summary(fit_model)$sigma) ^ 2
  R = Matrix((sig_e * diag(m_total)), sparse = T)
  invR = solve(R)
  detR = max(det(R),(0.1)^8)

  G = Matrix((sig_u * diag(n)), sparse = T)

  n_cluster_units = as.data.frame(table(clusterID))$Freq
  G_big <- list()
  for (i in 1:n) {
    G_big[[i]] <- matrix(1, nrow = n_cluster_units[i],
                         ncol = n_cluster_units[i])
  }

  V = R + sig_u * bdiag(G_big)
  invV = solve(V)
  detV = max(det(V), (0.1)^8)


  #Cluster-level parameter -------------------------------------------
  if (is.null(X_cluster)) {
    dataset_cluster = data.frame(X = X, clusterID)
    X_cluster = aggregate(X ~ clusterID, data = dataset_cluster, FUN = "mean")[,-1]
    t_X_cluster = t(X_cluster)
    mu = crossprod(t_X_cluster, beta) + u
  } else {
    stopifnot(
      "Number of columns in 'X_cluster' has to be the same as p" = NCOL(X_cluster) == length(beta)
    )

    stopifnot(
      "Number of rows in 'X_cluster' has to be the same as number of clusters" = NROW(X_cluster) == n
    )
    t_X_cluster = t(X_cluster)
    mu = crossprod(t_X_cluster, beta) + u

  }

  output <- list(fit_model_fixed = fit_model_fixed,
                 fit_model_mixed = fit_model_mixed,
                 sig_u = sig_u, sig_e = sig_e,
                 R = R, invR = invR, detR = detR,
                 G = G,
                 V = V, invV = invV, detV = detV,
                 beta = beta, u = u,
                 mu = mu)

  output
}
