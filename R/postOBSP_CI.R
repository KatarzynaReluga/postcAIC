#' Post-OBSP confidence intervals (CI)
#'
#' Function \code{postOBSP_CI} provides post-OBSP confidence intervals
#' for mixed parameters
#'
#' @inheritParams compute_cAIC_for_model_set
#' @param alpha Construct 1 - alpha confidence intervals.
#' Default: \code{alpha = 0.05}
#' @param boot Number of bootstrap samples.
#' Default: \code{boot = 1000}
#'
#' @details
#' Parameter \code{boot} is needed for the calculation of the bootstrap
#' post-OBSP MSE of mixed effects.
#'
#' @return List with elements:
#' * \code{OBSP_min}  - index of the selected model
#' * \code{OBSP_models} - cAIC for all considered parameters
#' * \code{postOBSP_up} - upper boundary of CI for mixed effects
#' * \code{postOBSP_do} - lower boundary of CI for mixed effects
#' * \code{mu_hat_sel} - mixed effects of the selected model
#'
#' @importFrom stats aggregate qnorm rnorm
#'
#'
#' @examples
#' # Define basic parameters -------------------------------------------------
#' n = 15
#' m_i = 5
#' m_total = n * m_i
#'
#' beta = c(2.25, -1.1, 2.43, rep(0, 2))
#' sig_e = 1
#' sig_u = 1
#'
#' X = simulations_n15_mi5
#' X_intercept = cbind(rep(1, m_total), X)
#'
#' clusterID = rep(1:n, each = m_i)
#'
#' # Create responses, errors and random effects  -------------------
#' e_ij = rnorm(m_total, 0, sig_e)
#'
#' u_i = rnorm(n, 0, sig_u)
#' u_i_aug = rep(u_i, each = m_i)
#'
#' y = X_intercept%*% beta + u_i_aug + e_ij
#'
#' # Post-OBSP inference ----------------------------------------
#'
#' postOBSP_CI_results = postOBSP_CI(X, y,
#'                                   clusterID,
#'                                   X_cluster_full = NULL,
#'                                   model = "NERM",
#'                                   covariate_selection_matrix = NULL,
#'                                   modelset  = "part_subset",
#'                                   intercept = FALSE,
#'                                   common = c(1:2),
#'                                   boot = 250)
#'                                   
#' plot(postOBSP_CI_results)                                   
#'
#' @export
#'

postOBSP_CI <- function(X,
                        y,
                        clusterID,
                        X_cluster_full = NULL,
                        model = "NERM",
                        covariate_selection_matrix = NULL,
                        modelset  = "all_subsets",
                        intercept = FALSE,
                        common = NULL,
                        boot = 1000,
                        alpha = 0.05) {
  # Validate and format data matrix X, y, clusterID, Z ----------
  if (identical(intercept, FALSE)) {
    X = cbind(rep(1, nrow(X)), X)
  }
  
  X_full = format_data_matrix(validate_matrix(X), name_col = "X")
  t_X_full = t(X_full)
  
  if (is.null(X_cluster_full)) {
    X_cluster_full = aggregate(X_full ~ clusterID, FUN = "mean")[,-1]
  } else {
    X_cluster_full = format_data_matrix(validate_matrix(X_cluster_full),
                                        name_col = "X")
    
  }
  
  t_X_cluster_full = t(X_cluster_full)
  
  clusterID = validate_observations(clusterID, X_full, cluster = TRUE)
  y = validate_observations(y, X_full)
  
  # Recover necessary parameters ------------------------------------------------
  p_full = ncol(X_full)
  n = nlevels(clusterID)
  Z = create_Z(model, clusterID)
  m_total = nrow(X)
  m_i = as.data.frame(table(clusterID))$Freq
  
  # Create model sets -----------------------------------------------------------
  
  if (!is.null(modelset)) {
    modelset_matrix  = create_modelset(modelset, p = p_full, common)
  } else {
    modelset_matrix = covariate_selection_matrix
  }
  
  
  # Estimate parameters in the biggest model ---------------------------------
  params_full = estimate_NERM(X = X_full,
                              y,
                              clusterID = clusterID,
                              X_cluster = X_cluster_full)
  
  sig_e_full = params_full$sig_e
  sig_u_full = params_full$sig_u
  
  
  R_full = sig_e_full * diag(m_total)
  invR_full = 1 / sig_e_full * diag(m_total)
  G_full = sig_u_full * diag(n)
  n_cluster_units = as.data.frame(table(clusterID))$Freq
  
  invV_full_list <- list()
  Ga_list <- list()
  Ga_sq_list <- list()
  
  for (i in 1:n) {
    invV_full_list[[i]] <- solve(
      sig_e_full * diag(n_cluster_units[i]) +
        sig_u_full * matrix(1, nrow = n_cluster_units[i],
                            ncol = n_cluster_units[i])
    )
    Ga_list[[i]] <- sig_u_full * invV_full_list[[i]]
    Ga_sq_list[[i]] <- crossprod(Ga_list[[i]])
  }
  
  invV_full = bdiag(invV_full_list)
  Ga = as.matrix(bdiag(Ga_list))
  Ga_sq = as.matrix(bdiag(Ga_sq_list))
  
  # Prepare for the computation of OBSP for the set of models ------------------
  gamma_full = sig_u_full / (sig_u_full + sig_e_full / m_i)
  g1_full = (1 - gamma_full) * sig_u_full
  y_cluster_mean = aggregate(y ~ clusterID, FUN = "mean")[,-1]
  
  beta_model_matrix = matrix(0, nrow = nrow(modelset_matrix),
                             ncol = p_full)
  
  OBSP_models = numeric(nrow(modelset_matrix))
  
  # Compute OBSP for the set of models -----------------------------------------
  mu_hat_model = list()
  for (k in 1:nrow(modelset_matrix)) {
    covariates = modelset_matrix[k, ]
    indices0 <- (1:ncol(X_full)) * covariates
    indices = indices0[indices0 != 0]
    X = format_data_matrix(as.matrix(X_full[, indices]))
    t_X = t(X)
    X_cluster <-
      format_data_matrix(as.matrix(X_cluster_full[, indices]))
    
    temp = as.matrix(solve(crossprod(X, crossprod(Ga_sq, X))))
    beta_model = temp %*% t_X %*% Ga_sq %*% y
    y_Xbeta_dif = y - crossprod(t_X, beta_model)
    P_model = Ga %*% X %*% temp %*% t_X %*% Ga
    penalty = sum(diag(P_model %*% Ga %*% as.matrix(R_full)))
    OBSP_models[k] = t(y_Xbeta_dif) %*% Ga_sq %*% (y_Xbeta_dif) +
      2 * penalty
    
    u_model = gamma_full * (y_cluster_mean - X_cluster %*% beta_model)
    mu_hat_model[[k]] = X_cluster %*% beta_model + u_model
    
    beta_model_matrix[k, indices] <- beta_model
  }
  OBSP_min = which.min(OBSP_models)
  
  covariates_sel = modelset_matrix[OBSP_min, ]
  indices0_sel <- (1:ncol(X_full)) * covariates_sel
  indices_sel = indices0_sel[indices0_sel != 0]
  
  # Compute bootstrap MSE for the mixed effect in the selected model -------------
  
  u_boot_true = matrix(0, nrow = boot, ncol = n)
  mu_boot_true = matrix(0, nrow = boot, ncol = n)
  
  gamma_full_boot = matrix(0, nrow = boot, ncol = n)
  
  u_boot_hat = matrix(0, nrow = boot, ncol = n)
  mu_boot_hat = matrix(0, nrow = boot, ncol = n)
  
  sig_u_full_boot = numeric(boot)
  sig_e_full_boot = numeric(boot)
  
  
  for (b in 1:boot)  {
    e_boot = rnorm(m_total, 0, sqrt(sig_e_full))
    u_boot = rnorm(n, 0, sqrt(sig_u_full))
    beta_select = beta_model_matrix[OBSP_min,]
    y_boot = X_full %*% beta_select + rep(u_boot, m_i) + e_boot
    y_cluster_mean_boot = aggregate(y_boot ~ clusterID, FUN = "mean")[, -1]
    
    # Boot true mu  --------------------------------------------------------------
    
    u_boot_true[b, ] = gamma_full * (y_cluster_mean_boot - as.matrix(X_cluster_full) %*%
                                       beta_select)
    mu_boot_true[b, ] = as.matrix(X_cluster_full) %*% beta_select + u_boot_true[b, ]
    
    # Estimate boostrap data -------------------------------------------------------
    
    params = estimate_NERM(
      X = X_full[, indices_sel],
      y = y_boot,
      clusterID = clusterID,
      X_cluster = X_cluster_full[, indices_sel]
    )
    
    sig_u_full_boot[b] = params$sig_u
    sig_e_full_boot[b] = params$sig_e
    
    Ga_boot_list <- list()
    Ga_sq_boot_list <- list()
    
    for (i in 1:n) {
      Ga_boot_list[[i]] <-
        sig_u_full_boot[b] * solve(
          sig_e_full_boot[b] * diag(n_cluster_units[i]) +
            sig_u_full_boot[b] * matrix(1, nrow = n_cluster_units[i],
                                        ncol = n_cluster_units[i])
        )
      Ga_sq_boot_list[[i]] <- crossprod(Ga_boot_list[[i]])
    }
    
    Ga_boot = as.matrix(bdiag(Ga_boot_list))
    Ga_sq_boot = as.matrix(bdiag(Ga_sq_boot_list))
    
    temp_boot = as.matrix(solve(crossprod(
      X_full[, indices],
      crossprod(Ga_sq_boot, X_full[, indices])
    )))
    beta_model_temp = temp_boot %*% t(X_full[, indices]) %*% Ga_sq_boot %*%
      y_boot
    
    gamma_full_boot[b, ] = sig_u_full_boot[b] / (sig_u_full_boot[b] + sig_e_full_boot[b] /
                                                   m_i)
    u_boot_hat[b, ] = gamma_full_boot[b,] * (y_cluster_mean_boot - as.matrix(X_cluster_full) %*%
                                               beta_select)
    
    mu_boot_hat[b,] = as.matrix(X_cluster_full) %*% beta_select + u_boot_hat[b, ]
    
  }
  
  g1_boot = (1 - gamma_full_boot) * sig_u_full_boot
  mse_b = 2 * g1_full - mean(g1_boot) + colMeans((mu_boot_hat - mu_boot_true) ^ 2)
  rmse_b = sqrt(mse_b)
  
  # Construct confidence intervals ---------------------------------------------
  q_interval = qnorm(p = 1 - alpha / 2)
  mu_hat_sel = c(mu_hat_model[[OBSP_min]])
  
  postOBSP_up = c(mu_hat_sel + q_interval * rmse_b)
  postOBSP_do = c(mu_hat_sel - q_interval * rmse_b)
  
  output = list(
    postOBSP_up = postOBSP_up,
    postOBSP_do = postOBSP_do,
    mu_hat_sel = mu_hat_sel,
    OBSP_min = OBSP_min,
    OBSP_models = OBSP_models
  )
  
  class(output) <- "postOBSP_CI"
  output
  
}
