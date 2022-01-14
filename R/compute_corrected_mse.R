#' Compute second-order correct MSE
#'
#' Function \code{compute_mse} provides first-and
#' second-order MSE estimates for mixed parameter
#'
#' @inheritParams compute_mse
#'
#' @return List with parameters:
#' * \code{mse} - first-order correct MSE of mixed effects
#' * \code{mse_corrected} - second-order correct MSE of mixed effects
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
#'
#' fit_nerm <- estimate_NERM(X = X, y = y,
#'                           clusterID = clusterID,
#'                           X_cluster = NULL)
#' C_cluster = cbind(X[1:10, ], diag(n))
#'
#' mse_second = compute_corrected_mse(C_cluster, X, sig_u = fit_nerm$sig_u,
#'                                    sig_e = fit_nerm$sig_e,
#'                                    clusterID = clusterID)
#'
#' @export
#'


compute_corrected_mse <- function(C_cluster,
                                  X,
                                  sig_u,
                                  sig_e,
                                  clusterID,
                                  model = "NERM") {
  m_i = as.data.frame(table(clusterID))$Freq
  
  g1g2 = compute_mse(C_cluster, X, sig_u, sig_e,
                     clusterID = clusterID, model)
  
  temp3 = m_i * sig_u + sig_e
  temp4 = sum(m_i ^ 2 * temp3 ^ (-2)) * sum(((m_i - 1) * sig_e ^ (-2) +
                                               temp3 ^ (-2))) - (sum(m_i * temp3 ^ (-2))) ^ 2
  temp5 = 2 / temp4 * sum(((m_i - 1) * sig_e ^ (-2) + temp3 ^ (-2)))
  temp6 = 2 / temp4 * sum(m_i ^ 2 * temp3 ^ (-2))
  temp7 = -2 / temp4 * sum(m_i * temp3 ^ (-2))
  temp8 = sig_e ^ 2 * temp5 + sig_u ^ 2 * temp6 - 2 * sig_e * sig_u * temp7
  g3 = m_i ^ (-2) * (sig_u + sig_e / m_i) ^ (-3) * temp8
  
  
  mse_corrected = g1g2 + 2 * g3
  
  output <- list(mse = g1g2, mse_corrected  = mse_corrected)
  output
  
}
