#' Compute information criteria
#'
#' Function \code{compute_infromation_criteria} provides
#' mAIC, cAIC and mBIC for NERM
#'
#' @inheritParams estimate_NERM
#' @param model Type of mixed model: NERM, FHM, RIRS (random slopes and random intercepts)
#' @param sig_u Variance parameter of random effects
#' @param sig_e Variance parameter of errors
#' @param fit_model_fixed Estimated model using fixed effects
#' @param fit_model_mixed Estimated model using fixed and random effects
#'
#' @return List with information criteria:
#' \item{mAIC}{Marginal AIC}
#' \item{mBIC}{Marginal BIC}
#' \item{cAIC}{Conditional AIC}
#' \item{deg_cAIC}{Penalty of conditional AIC}
#'
#' @details
#' Penalty term in cAIC depends on the model selected.
#' Function \code{compute_information_criteria} is simplified
#' because for now only NERM is supported.
#'
#' @importFrom Matrix Matrix det
#'
#' @examples
#'
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
#' fit <- estimate_NERM(X, y, clusterID)
#'
#' IC <- compute_information_criteria(X = X, y = y,
#'                                    clusterID = clusterID,
#'                                    model = "NERM",
#'                                    sig_u = fit$sig_u,
#'                                    sig_e = fit$sig_e,
#'                                    fit_model_fixed = fit$fit_model_fixed,
#'                                    fit_model_mixed = fit$fit_model_mixed)
#'
#' @export
#'

compute_information_criteria <- function(X,
                                         y,
                                         clusterID,
                                         model,
                                         sig_u,
                                         sig_e,
                                         fit_model_fixed,
                                         fit_model_mixed) {
  # Recover parameters -----------------------------------
  if (is.factor(clusterID) == TRUE) {
    clusterID
  } else {
    clusterID  = sort(factor(clusterID))
  }
  n = nlevels(clusterID)
  Z = create_Z(model = model, clusterID)
  m_total = nrow(X)
  size_fixed_params = ncol(X) + 2
  
  # Estimate matrices and inverses --------------------------
  R = sig_e * diag(m_total)
  invR = 1 / sig_e * diag(m_total)
  detR = max(det(Matrix(R, sparse = T)), (0.1) ^ 8)
  invG = 1 / sig_u * diag(n)
  n_cluster_units = as.data.frame(table(clusterID))$Freq
  
  V_list <- list()
  invV_list <- list()
  
  for (i in 1:n) {
    V_list[[i]] <- sig_e * diag(n_cluster_units[i]) +
      sig_u * matrix(1, nrow = n_cluster_units[i], ncol = n_cluster_units[i])
    invV_list[[i]] <- solve(V_list[[i]])
  }
  V = bdiag(V_list)
  invV = bdiag(invV_list)
  detV = max(det(Matrix(V, sparse = T)), (0.1) ^ 8)
  
  
  loglik_marginal = m_total * log(2 * pi) + log(detV) +
    (t(y - fit_model_fixed) %*% invV %*% (y - fit_model_fixed))
  
  # Mariginal AIC ------------------------------------------
  mAIC = as.vector(loglik_marginal + 2 * size_fixed_params)
  
  # Mariginal BIC ------------------------------------------
  mBIC = as.vector(loglik_marginal + log(m_total) * size_fixed_params)
  
  # Conditional AIC --------------------------------------
  t_X  = t(X)
  t_Z = t(Z)
  trace1 = solve(rbind(cbind(t_X %*% X, t_X %*% Z),
                       cbind(t_Z %*% X, t_Z %*% Z + invG)))
  trace2 = rbind(cbind(t_X %*% X, t_X %*% Z), cbind(t_Z %*% X, t_Z %*% Z))
  trace_model = sum(diag(trace1 %*% trace2))
  
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
