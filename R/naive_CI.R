#' Naive confidence intervals (CI)
#'
#' Function \code{naive_CI} provides naive CI for fixed and mixed effects
#' after cAIC model selection, that is the usual CI for the selected model,
#' which ignore the selection
#'
#' @inheritParams postcAIC_CI
#' @param sig_u_sel Variance parameter of random effects used for the selected
#' model (output \code{sig_u_sel} of \code{compute_cAIC_for_model_set}: the
#' full-model estimate if \code{theta = "full"}, the estimate under the
#' selected model if \code{theta = "refit"})
#' @param sig_e_sel Variance parameter of errors used for the selected
#' model (output \code{sig_e_sel} of \code{compute_cAIC_for_model_set})
#' @param sig_u_full Not used; kept so that existing calls keep working
#' @param sig_e_full Not used; kept so that existing calls keep working
#' @param C_cluster_sel Matrix with cluster level covariates for fixed and random effects
#' @param clusterID  Vector with cluster labels
#' @param indices_sel Indices of the selected covariates among full covariate set
#' @param type_MSE_mixed Type of CI (using first order, second order or both MSE estimators)
#' @param alpha Construct 1 - alpha confidence intervals. Default: \code{alpha = 0.05}
#'
#' @return List with elements:
#' \item{beta_naive_CI_up}{Upper boundary of naive CI for fixed effects}
#' \item{beta_naive_CI_do}{Lower boundary of naive CI for fixed effects}
#' \item{mixed_naive_CI_corrected_up}{Upper boundary of naive CI for mixed effects with the second order MSE}
#' \item{mixed_naive_CI_corrected_do}{Lower boundary of naive CI for mixed effects with the second order MSE}
#' \item{mixed_naive_CI_up}{Upper boundary of naive CI for mixed effects with the first order MSE}
#' \item{mixed_naive_CI_do}{Lower boundary of naive CI for mixed effects with the first order MSE}
#' \item{beta_x_naive_CI_up}{Upper boundary of naive CI for linear combinations of fixed effects}
#' \item{beta_x_naive_CI_do}{Lower boundary of naive CI for linear combinations of fixed effects}
#'
#'
#' @details
#' The output changes slightly depending on the value
#' of the parameter \code{type}. If \code{type = "regular"},
#' naive second-order correct CIs are not present.
#' If \code{type = "corrected"}, naive first-order correct CIS are not present.
#' If \code{type = "both"}, naive first- and second-order correct CIs are present.
#'
#' All intervals use the variance parameters \code{sig_u_sel} and \code{sig_e_sel}.
#' The CI for fixed effects use the covariance matrix
#' \eqn{(X_{M_*}^T V^{-1} X_{M_*})^{-1}} of the selected model.
#'
#'
#' @importFrom Matrix bdiag
#' @importFrom stats qnorm
#'
#' @examples
#' n = 10
#' m_i = 5
#' m_total = 50
#'
#' clusterID = rep(1:n, each = m_i)
#' p = 10
#' beta = rep(2, p)
#' u_i = rnorm(n, 0, 2)
#' u_i_aug = rep(u_i, each = m_i)
#' X = matrix(rnorm(m_total * p), m_total, p)
#' y = X%*%beta + u_i_aug + rnorm(m_total, 0, 1)
#'
#' cAIC_model_set =
#' compute_cAIC_for_model_set(X, y, clusterID,
#'                            model = "NERM",
#'                            covariate_selection_matrix = NULL,
#'                            modelset  = "part_subset",
#'                            common = c(1:8),
#'                            intercept = FALSE)
#'
#'
#' cAIC_min = cAIC_model_set$cAIC_min
#' degcAIC_models = cAIC_model_set$degcAIC_models
#'
#' X_full = cAIC_model_set$X_full
#' X_cluster_full = cAIC_model_set$X_cluster_full
#'
#' sig_u_full = cAIC_model_set$sig_u_full
#' sig_e_full = cAIC_model_set$sig_e_full
#'
#' beta_sel = cAIC_model_set$beta_sel
#' mu_sel = cAIC_model_set$mu_sel
#'
#' sig_u_sel = cAIC_model_set$sig_u_sel
#' sig_e_sel = cAIC_model_set$sig_e_sel
#' indices_sel = cAIC_model_set$indices_sel
#' X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
#' C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(n))
#'
#'
#' x_beta_lin_com = cAIC_model_set$X_cluster_full
#'
#' naive_CI_results  = naive_CI(
#'   beta_sel,
#'   mu_sel,
#'   sig_u_sel,
#'   sig_e_sel,
#'   sig_u_full,
#'   sig_e_full,
#'   X_full,
#'   C_cluster_sel,
#'   clusterID,
#'   indices_sel,
#'   type_MSE_mixed = "regular",
#'   x_beta_lin_com)
#'
#'   plot(naive_CI_results, type = "regular")
#'
#' @export
#'

naive_CI  = function(beta_sel,
                     mu_sel,
                     sig_u_sel,
                     sig_e_sel,
                     sig_u_full,
                     sig_e_full,
                     
                     X_full,
                     C_cluster_sel,
                     clusterID,
                     indices_sel = NULL,
                     type_MSE_mixed = c("regular", "corrected", "both"),
                     x_beta_lin_com = NULL,
                     alpha = 0.05) {
  # Recover some parameters -----------------------------------------------
  p_full = ncol(X_full)
  clusterID = validate_observations(clusterID, X_full, cluster = TRUE)
  stopifnot("Observations must be sorted by cluster ('clusterID')" =
              !is.unsorted(as.integer(clusterID)))
  n = nlevels(clusterID)
  n_cluster_units = as.data.frame(table(clusterID))$Freq
  
  # Covariance matrix of beta_hat under the selected model ------------------
  X_sel = as.matrix(X_full[, indices_sel, drop = FALSE])
  
  V_sel_list <- list()
  invV_sel_list <- list()
  
  for (i in 1:n) {
    V_sel_list[[i]] <- sig_e_sel * diag(n_cluster_units[i]) +
      sig_u_sel * matrix(1, nrow = n_cluster_units[i], ncol = n_cluster_units[i])
    invV_sel_list[[i]] <- solve(V_sel_list[[i]])
  }
  
  invV_sel = bdiag(invV_sel_list)
  
  invxV_selx = as.matrix(solve(t(X_sel) %*% invV_sel %*% X_sel))
  
  var_sel = numeric(p_full)
  var_sel[indices_sel] <- diag(invxV_selx)
  
  beta_sel_full = numeric(p_full)
  beta_sel_full[indices_sel] <- beta_sel
  
  # Naive CI for fixed parameter  ------------------------------------------
  q_interval = qnorm(p = 1 - alpha / 2)
  
  beta_naive_CI_up = beta_sel_full + q_interval * sqrt(var_sel)
  beta_naive_CI_do = beta_sel_full - q_interval * sqrt(var_sel)
  
  # Naive CI for a linear combination of fixed parameters ------------------
  if (!is.null(x_beta_lin_com)) {
    if (is.vector(x_beta_lin_com)) {
      x_beta_lin_com = matrix(x_beta_lin_com,
                              nrow = 1,
                              ncol = length(x_beta_lin_com))
    }
    x_beta_lin_com = as.matrix(x_beta_lin_com)
    stopifnot(
      "Number of columns in 'x_beta_lin_com'
        has to be the same as number of covariates in a full model" = NCOL(x_beta_lin_com) == length(beta_sel_full)
    )
    t_x_beta_lin_com = t(x_beta_lin_com)
    
    # Variance of x^T beta_hat for every row x of x_beta_lin_com
    x_beta_selected = x_beta_lin_com[, indices_sel, drop = FALSE]
    x_beta_invxvx = rowSums((x_beta_selected %*% invxV_selx) * x_beta_selected)
    
    beta_x_full = crossprod(t_x_beta_lin_com, beta_sel_full)
    
    beta_x_naive_CI_up = c(beta_x_full + sqrt(x_beta_invxvx) * q_interval)
    beta_x_naive_CI_do = c(beta_x_full - sqrt(x_beta_invxvx) * q_interval)
    
  } else {
    beta_x_naive_CI_up = "CI for a linear combination of fixed effects not requested"
    beta_x_naive_CI_do = "CI for a linear combination of fixed effects not requested"
  }
  
  #
  # Naive CI for mixed parameter --------------------------------------------------
  #
  type_MSE_mixed = match.arg(type_MSE_mixed)
  
  if (type_MSE_mixed == "regular") {
    mse = compute_mse(
      C_cluster = C_cluster_sel,
      X = X_full[, indices_sel, drop = FALSE],
      sig_u = sig_u_sel,
      sig_e = sig_e_sel,
      clusterID,
      model = "NERM"
    )
    
    mixed_naive_CI_up = c(mu_sel + q_interval * sqrt(mse))
    mixed_naive_CI_do = c(mu_sel - q_interval * sqrt(mse))
    mixed_naive_CI_corrected_up = "Corrected CI for mixed effects not requested"
    mixed_naive_CI_corrected_do = "Corrected CI for mixed effects not requested"
    
  } else if (type_MSE_mixed == "corrected") {
    mse_results = compute_corrected_mse(
      C_cluster = C_cluster_sel,
      X = X_full[, indices_sel, drop = FALSE],
      sig_u = sig_u_sel,
      sig_e = sig_e_sel,
      clusterID,
      model = "NERM"
    )
    
    mse_corrected = mse_results$mse_corrected
    
    mixed_naive_CI_up = "Regular CI for mixed effects not requested"
    mixed_naive_CI_do = "Regular CI for mixed effects not requested"
    mixed_naive_CI_corrected_up = c(mu_sel + q_interval * sqrt(mse_corrected))
    mixed_naive_CI_corrected_do = c(mu_sel - q_interval * sqrt(mse_corrected))
    
  } else {
    mse_results = compute_corrected_mse(
      C_cluster = C_cluster_sel,
      X = X_full[, indices_sel, drop = FALSE],
      sig_u = sig_u_sel,
      sig_e = sig_e_sel,
      clusterID,
      model = "NERM"
    )
    
    mse = mse_results$mse
    mse_corrected = mse_results$mse_corrected
    
    mixed_naive_CI_corrected_up = c(mu_sel + q_interval * sqrt(mse_corrected))
    mixed_naive_CI_corrected_do = c(mu_sel - q_interval * sqrt(mse_corrected))
    
    mixed_naive_CI_up = c(mu_sel + q_interval * sqrt(mse))
    mixed_naive_CI_do = c(mu_sel - q_interval * sqrt(mse))
    
  }
  
  output = list(
    beta_naive_CI_up = beta_naive_CI_up,
    beta_naive_CI_do = beta_naive_CI_do,
    mixed_naive_CI_corrected_up = mixed_naive_CI_corrected_up,
    mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do,
    mixed_naive_CI_up = mixed_naive_CI_up,
    mixed_naive_CI_do = mixed_naive_CI_do,
    beta_x_naive_CI_up = beta_x_naive_CI_up,
    beta_x_naive_CI_do = beta_x_naive_CI_do
  )
  class(output) <- "naive_CI"
  output
  
}
