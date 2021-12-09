#' Compute information criteria
#'
#' Function \code{compute_infromation_criteria} provides mAIC, cAIC and mBIC for NERM
#'
#' @inheritParams estimate_NERM
#' @param Z Z matrix of covariates for random effects
#' @param detV Determinant of covariance matrix of response
#' @param invV Inverse of covariance matrix of response
#' @param detR Determinant of covariance matrix of errors
#' @param invR Inverse of covariance matrix of errors
#' @param sig_u Variance parameter of random effects
#' @param sig_e Variance parameter of errors
#' @param G Covariance matrix of random effects
#' @param fit_model_fixed Estimated model using fixed effects
#' @param fit_model_mixed Estimated model using fixed and random effects
#' @param size_fixed_params Size of fixed parameters (p + variance parameters)
#'
#'
#'
#' @return List with information criteria
#'
#' * \code{mAIC} - marginal AIC
#' * \code{mBIC} - marginal BIC
#' * \code{cAIC} - conditional AIC
#' * \code{deg_cAIC} - penalty of conditional AIC
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
#' fit <- estimate_NERM(X, y, clusterID)
#'
#' Z = create_Z("NERM", clusterID)
#'
#' IC <- compute_information_criteria(X = X, y = y, Z = Z,
#'  detV = fit$detV, invV = fit$invV,
#'  detR = fit$detR, invR = fit$invR,
#'  G = fit$G,
#'  sig_u = fit$sig_u,
#'  sig_e = fit$sig_e,
#'  fit_model_fixed = fit$fit_model_fixed,
#'  fit_model_mixed = fit$fit_model_mixed,
#'  size_fixed_params = fit$size_fixed_params)
#'
#' @export


compute_information_criteria <- function(X,
                                         y,
                                         Z,
                                         detV,
                                         invV,
                                         detR,
                                         invR,
                                         G,
                                         sig_u,
                                         sig_e,
                                         fit_model_fixed,
                                         fit_model_mixed,
                                         size_fixed_params) {
  m_total = length(y)
  loglik_marginal = m_total * log(2 * pi) + log(detV) +
    (t(y - fit_model_fixed) %*% invV %*% (y - fit_model_fixed))

  #Mariginal AIC
  mAIC = as.vector(loglik_marginal + 2 * size_fixed_params)

  #Mariginal BIC
  mBIC = as.vector(loglik_marginal + log(m_total) * size_fixed_params)

  #Conditional AIC
  t_X  = t(X)
  t_Z = t(Z)
  trace1 = solve(rbind(cbind(t_X %*% X, t_X %*% Z),
                       cbind(t_Z %*% X, t_Z %*% Z + solve(G))))
  trace2 = rbind(cbind(t_X %*% X, t_X %*% Z), cbind(t_Z %*% X, t_Z %*% Z))
  trace_model = sum(diag(trace1 %*% trace2))

  #Corrections depends on the model: for the moment NERM is supported

  b_c = 1 + 2 / (m_total - 1) + 2 / (1 + m_total * sig_u / sig_e)


  deg_cAIC = trace_model + b_c

  loglik_conditional = m_total * log(2 * pi) + log(detR) +
    (t(y - fit_model_mixed) %*% invR %*% (y - fit_model_mixed))

  cAIC = as.vector(loglik_conditional + 2 * deg_cAIC)

  output  = list(
    mAIC = mAIC,
    mBIC = mBIC,
    cAIC = cAIC,
    deg_cAIC = deg_cAIC
  )
  output

}
