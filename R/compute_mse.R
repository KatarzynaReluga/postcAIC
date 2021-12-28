#' Compute MSE
#'
#' Function \code{compute_mse} provides first order correct MSE for mixed effects
#'
#' @inheritParams estimate_invK
#' @param C_cluster Cluster-level covariates for fixed and random parameters
#'
#' @return mse MSE of mixed effects
#'
#' @examples
#' n = 10
#' m_i = 5
#' m_total = 50
#'
#' clusterID = rep(1:n, m_i)
#' p = 10
#' beta = rep(2, p)
#' u_i = rnorm(n, 0, 2)
#' u_i_aug = rep(u_i, each = m_i)
#' X = matrix(rnorm(m_total * p), m_total, p)
#' y = X%*%beta + u_i_aug + rnorm(m_total, 0, 1)
#' fit_nerm <- estimate_NERM(X = X, y = y, clusterID = clusterID, X_cluster = NULL)
#' C_cluster = cbind(X[1:10, ], diag(n))
#' mse_first = compute_mse(C_cluster, X,
#'                         sig_u = fit_nerm$sig_u,
#'                         sig_e = fit_nerm$sig_e,
#'                         clusterID = clusterID)
#'
#' @export

compute_mse <- function(C_cluster, X, sig_u, sig_e,
                        clusterID, model = "NERM") {

  K_inv = estimate_invK(X, sig_u, sig_e, clusterID = clusterID, model)

  stopifnot("ncol(C_cluster) must be the same as nrow(K_inv)" = ncol(C_cluster) == nrow(K_inv))
  mse = diag(C_cluster %*% K_inv %*% t(C_cluster))
  mse
}
