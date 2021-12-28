#' Estimate inverse of matrix K
#'
#' @param X Matrix with covariates for fixed effects
#' @param sig_u Variance parameter of random effects
#' @param sig_e Variance parameter of errors
#' @param model Model we want to fit. Only NERM supported for now.
#' @param clusterID Vector with cluster labels
#'
#' @return K_inv Inverse of Matrix K
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
#' invK = estimate_invK(X, sig_u = fit_nerm$sig_u,
#'                      sig_e = fit_nerm$sig_e,
#'                      clusterID = clusterID)
#'
#' @export

estimate_invK <- function(X, sig_u, sig_e,
                          model = "NERM", clusterID) {

  Z = create_Z("NERM", clusterID)
  n = ncol(Z)
  m_total = nrow(Z)
  C = cbind(X, Z)
  p = ncol(X)

  invG = 1/sig_u * diag(n)
  invR = 1/sig_e * diag(m_total)

  G_plus_inv = rbind(matrix(0, nrow = p,
                            ncol = p + n),
                     cbind(matrix(0, nrow = n, ncol = p),
                           invG))
  K_inv = solve(crossprod(t(crossprod(C, t(invR))), C) + G_plus_inv)
  K_inv

}

