#' Compute second-order correct MSE
#'
#' Function \code{compute_mse} provides first and second order MSE estimates for
#' mixed parameter
#' @inheritParams compute_mse
#' @param clusterID Cluster labels
#' @param sig_u Variance parameter of random effects
#' @param sig_e Variance parameter of errors
#'
#' @return List with parameters
#' * \code{mse} First order correct MSE of mixed effects
#' * \code{mse_corrected} First order correct MSE of mixed effects
#' @export
#'


compute_corrected_mse <- function(C_cluster, clusterID,
                                  K_inv, sig_u, sig_e) {

  m_i = as.data.frame(table(clusterID))$Freq

  g1g2 = compute_mse(C_cluster, K_inv)

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
