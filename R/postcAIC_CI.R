#' Post-cAIC confidence intervals
#'
#' Function \code{postcAIC_CI} provides post-cAIC confidence intervals for
#' mixed and fixed effects under NERM
#'
#' @inheritParams create_modelset
#' @inheritParams estimate_invK
#' @param degcAIC_models Penalty for all considered models
#' @param cAIC_min Index of the selected model among models in the modelset
#' @param X_full Matrix with a full set of covariates
#' @param X_cluster_full Matrix with cluster level covariates for fixed effects of the full model
#' @param R_full Covariance matrix of errors of the full model
#' @param G_full Covariance matrix of random effects of the full model
#' @param V_full Covariance matrix of response of the full model
#' @param beta_sel Fixed effects (regression parameters) of the selected model
#' @param mu_sel Mixed effects of the selected model
#' @param x_beta_lin_com Vector or matrix to create linear combinations with
#' fixed parameters. Default: 'x_beta_lin_com = NULL'
#' @param modelset_matrix Matrix composed of zeros and ones.
#' @param n_starting_points Number of initial starting points for sampling from truncated distribution
#' @param scale_mvrnorm Scale parameter for multivariate normal distribution to sample
#'
#' @return List with parameters
#' * \code{cAIC_min}  - Index of selected model
#' * \code{degcAIC_models} - Penalty for all considered models
#' * \code{beta_sel} - fixed effects (regression parameters) of the selected model
#' * \code{mu_sel} - mixed effects of the selected model
#' * \code{V_sel} - covariance matrix of response of the selected model
#' * \code{G_sel} - covariance matrix of random effects of the selected model
#' * \code{R_sel} - covariance matrix of response of the selected model
#' * \code{X_sel} - matrix with fixed effects covariates of the selected model
#' * \code{indices_sel} - indices of the selected covaraites among full covariate set
#' * \code{sig_u_sel} - variance parameter of random effects of the selected model
#' * \code{sig_e_sel} - variance parameter of errors of the selected model
#' * \code{V_sel} -  covariance matrix of response of the selected model
#' * \code{invV_full} - inverse of covariance matrix of response of the full model
#' * \code{G_full} - covariance matrix of random effects of the selected model
#' * \code{R_full} - covariance matrix of response of the full model
#' * \code{X_full} - matrix with fixed effects covariates of the full model
#' * \code{X_cluster_full} - matrix with cluster level covariates for fixed effects of the full model
#' * \code{Z} - matrix of covariates for random effects
#' * \code{modelset_matrix} -  matrix composed of zeros and ones. Ones correspond to
#' parameters in a model which is represented in nth row.
#'
#' @return List with parameters
#' * \code{beta_PoSI_CI_up}  - upper boundary of CI for fixed effects
#' * \code{beta_PoSI_CI_do} - lower boundary of CI for fixed effects
#' * \code{mixed_PoSI_CI_up} - upper boundary of CI for mixed effects
#' * \code{mixed_PoSI_CI_do} - lower boundary of CI for mixed effects
#' * \code{beta_x_PoSI_CI_up} - upper boundary of CI for linear combinations of fixed effects
#' * \code{beta_x_PoSI_CI_do} - lower boundary of CI for linear combinations of fixed effects
#'
#'
#'
#' @importFrom stats quantile
#'
#' @export
#'
#'

postcAIC_CI  = function(cAIC_min,
                        degcAIC_models,
                        Z,
                        X_full,
                        X_cluster_full,
                        G_full,
                        R_full,
                        V_full,

                        beta_sel,
                        mu_sel,

                        modelset = "all_subsets",
                        common = NULL,
                        modelset_matrix,
                        x_beta_lin_com = NULL,
                        n_starting_points = 5, scale_mvrnorm = 1) {
  # Parameters to recover -------------------------------------------------
  p_full = ncol(X_full)
  n = ncol(Z)
  C_cluster_full = cbind(X_cluster_full, diag(n))
  invR_full = solve(R_full)
  invV_full = solve(V_full)

  # Compute matrix Sigma for a full model ---------------------------------

  compute_Sigma_results  = compute_Sigma(X = X_full, invR = invR_full,
                                         invV = invV_full)

  Sigma_full = as.matrix(compute_Sigma_results$Sigma)
  sqrt_invxVx_full = compute_Sigma_results$sqrt_invxVx

  # Compute selection matrix upsilon  -------------------------------------
  upsilon_output  = compute_upsilon(modelset, p  = p_full, common)

  upsilon = upsilon_output$upsilon
  upsilon_cAIC = upsilon[cAIC_min,]
  upsilon_cAIC_extended = matrix(
    upsilon_cAIC,
    nrow = nrow(modelset_matrix) - 1,
    ncol = ncol(upsilon),
    byrow = TRUE
  )
  upsilon_not_cAIC = upsilon[-cAIC_min,]

  upsilon_cAIC_not_cAIC = upsilon_cAIC_extended - upsilon_not_cAIC


  # Vectorize terms in matrix Sigma ---------------------------------------
  index_cov_terms = upsilon_output$cov_terms_index0
  index_full_model = upsilon_output$index_full_model
  Sigma_ordered_output  = vectorize_Sigma(Sigma = Sigma_full,
                                          index_cov_terms,
                                          p = p_full,
                                          index_full_model)
  Sigma_ordered = Sigma_ordered_output$Sigma_ordered
  Sigma_order_index = Sigma_ordered_output$Sigma_order_index


  # Calculate invK, its inverse and square root-- ------------------------
  invK = estimate_invK(X = X_full,
                       Z = Z,
                       G = G_full,
                       R = R_full)
  invK_vec = eigen(invK)$vectors
  invK_val = sqrt(eigen(invK)$values)
  solinvK_vec = solve(invK_vec)
  invK_sqrt = invK_vec %*% diag(invK_val) %*% solinvK_vec

  # Select terms in Sigma according to cAIC ------------------------------
  var_cov_terms = matrix(0,
                         nrow = nrow(upsilon_not_cAIC),
                         ncol = ncol(upsilon_not_cAIC))

  for (k in 1:nrow(upsilon_not_cAIC)) {
    var_cov_terms[k,] = Sigma_ordered * upsilon_cAIC_not_cAIC[k, Sigma_order_index]
  }

  # Sample from constrained distribution ------------------------------
  list_constraints  = construct_constraints(degcAIC_models, cAIC_min,
                                            var_cov_terms, p = p_full)

  sample_constraints_results  = sample_with_constraints(
    n_starting_points,
    p = p_full,
    n = n,
    n_models_to_compare = nrow(modelset_matrix) -
      1,
    list_constraints = list_constraints, scale_mvrnorm = scale_mvrnorm
  )
  # Post-cAIC CI ------------------------------------------------

  covariates_selected = modelset_matrix[cAIC_min,]
  indices0_selected = c(1:p_full) * covariates_selected
  indices_selected = indices0_selected[indices0_selected != 0]

  indices0_not_selected = c(1:p_full) * !covariates_selected
  indices_not_selected  = indices0_not_selected[indices0_not_selected != 0]

  sample_mix_sel = sample_constraints_results$sample_mix_full[,-indices_not_selected]
  sample_fixed_sel = sample_constraints_results$sample_fixed_full[, c(indices_selected)]

  # Post-cAIC CI for beta ----------------------------------------------------
  temp_beta_invxVx = tcrossprod(sqrt_invxVx_full[c(indices_selected),
                                                 c(indices_selected)],
                                sample_fixed_sel)
  q_beta_u = apply(temp_beta_invxVx,
                   1,
                   quantile,
                   prob = 0.975,
                   type = 8)
  q_beta_d = apply(temp_beta_invxVx,
                   1,
                   quantile,
                   prob = 0.025,
                   type = 8)

  q_beta_u_full  = numeric(p_full)
  q_beta_u_full[indices_selected] <- q_beta_u

  q_beta_d_full  = numeric(p_full)
  q_beta_d_full[indices_selected] <- q_beta_d

  full_beta_hat = numeric(p_full)
  full_beta_hat[indices_selected] <- beta_sel

  beta_PoSI_CI_up = full_beta_hat + q_beta_u_full
  beta_PoSI_CI_do = full_beta_hat + q_beta_d_full

  #Post-cAIC for  linear combination of betas and covaraites ---------------
  if (!is.null(x_beta_lin_com)) {
    stopifnot(
      "Number of columns in 'x_beta_lin_com'
        has to be the same as number of covariates in a full model" =
        NCOL(x_beta_lin_com) == length(full_beta_hat)
    )

    t_x_beta_lin_com = t(x_beta_lin_com)
    sample_beta_pred = crossprod(t_x_beta_lin_com[indices_selected,],
                                 temp_beta_invxVx)
    q_betax_u = apply(sample_beta_pred, 1,
                      function(x)
                        quantile(x, 0.975))
    q_betax_d = apply(sample_beta_pred, 1,
                      function(x)
                        quantile(x, 0.025))


    beta_x_full = crossprod(t_x_beta_lin_com, full_beta_hat)

    #PoSI
    beta_x_PoSI_CI_up = beta_x_full + q_betax_u
    beta_x_PoSI_CI_do = beta_x_full + q_betax_d
  }


  # Post-cAIC CI for mixed effects-------------------------------------------
  invK_sel = invK[-indices_not_selected,-indices_not_selected]
  invK_sqrt_sel = invK_sqrt[-indices_not_selected,-indices_not_selected]
  temp0_mixed_invK = tcrossprod(invK_sqrt_sel, sample_mix_sel)
  C_cluster_sel = C_cluster_full[,-indices_not_selected]
  temp_mixed_invK = crossprod(t(C_cluster_sel), temp0_mixed_invK)

  q_mixed_u = apply(temp_mixed_invK,
                    1,
                    quantile,
                    prob = 0.975,
                    type = 8)
  q_mixed_d = apply(temp_mixed_invK,
                    1,
                    quantile,
                    prob = 0.025,
                    type = 8)

  mixed_PoSI_CI_up = mu_sel + q_mixed_u
  mixed_PoSI_CI_do = mu_sel + q_mixed_d

  if (is.null(x_beta_lin_com)) {
    output = list(
      beta_PoSI_CI_up = beta_PoSI_CI_up,
      beta_PoSI_CI_do = beta_PoSI_CI_do,
      mixed_PoSI_CI_up = mixed_PoSI_CI_up,
      mixed_PoSI_CI_do = mixed_PoSI_CI_do
    )

  } else {
    output = list(
      beta_PoSI_CI_up = beta_PoSI_CI_up,
      beta_PoSI_CI_do = beta_PoSI_CI_do,
      mixed_PoSI_CI_up = mixed_PoSI_CI_up,
      mixed_PoSI_CI_do = mixed_PoSI_CI_do,
      beta_x_PoSI_CI_up = beta_x_PoSI_CI_up,
      beta_x_PoSI_CI_do = beta_x_PoSI_CI_do
    )
  }

  output
}
