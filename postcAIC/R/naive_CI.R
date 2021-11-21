#' Naive CI
#'
#' Function \code{naive_CI} provides naive CI for fixed and mixed effects after model selection
#'
#' @inheritParams postcAIC_CI
#' @param G_sel Covariance matrix of random effects from the selected model
#' @param R_sel Covariance matrix of errors from the selected model
#' @param V_full Covariance matrix of y from the full model
#' @param sig_u_sel Variance parameter of random effects of the selected model
#' @param sig_e_sel Variance parameter of errors of the selected model
#' @param Z Matrix with covariates for random effects
#' @param C_cluster_sel Matrix with cluster level covariates for fixed and random effects
#' @param clusterID  Vector with cluster labels
#' @param indices_sel Indices of the selected covaraites among full covariate set
#' @param type_MSE_mixed Type of CI (using first order, second order or both MSE estimators)
#'
#' @return List with parameters
#' * \code{beta_naive_CI_up}  - upper boundary of naive CI for fixed effects
#' * \code{beta_naive_CI_do} - lower boundary of naive CI for fixed effects
#' * \code{mixed_naive_CI_corrected_up} - upper boundary of naive CI for mixed effects with the second order MSE
#' * \code{mixed_naive_CI_corrected_do} - lower boundary of naive CI for mixed effects with the second order MSE
#' * \code{mixed_naive_CI_up} - upper boundary of naive CI for mixed effects with the first order MSE
#' * \code{mixed_naive_CI_do} - lower boundary of naive CI for mixed effects with the first order MSE
#' * \code{beta_x_naive_CI_up} - upper boundary of naive CI for linear combinations of fixed effects
#' * \code{beta_x_naive_CI_do} - lower boundary of naive CI for linear combinations of fixed effects
#'
#'
#' @export
#'
#'
#'
naive_CI  = function(beta_sel,
                     mu_sel,
                     G_sel,
                     R_sel,
                     V_full,
                     sig_u_sel,
                     sig_e_sel,
                     X_full,
                     Z,
                     C_cluster_sel,
                     clusterID,
                     indices_sel = NULL,
                     type_MSE_mixed = c("regular", "corrected", "both"),
                     x_beta_lin_com = NULL) {

  # Recover some parameters -----------------------------------------------
  p_full = ncol(X_full)

  invxV_fullx = solve(t(X_full) %*% solve(V_full) %*% X_full)

  var_beta_full = diag(invxV_fullx)
  var_sel = numeric(p_full)
  var_sel[indices_sel] <- var_beta_full[indices_sel]

  beta_sel_full = numeric(p_full)
  beta_sel_full[indices_sel] <- beta_sel

  # Naive CI for fixed parameter  ------------------------------------------
  beta_naive_CI_up = beta_sel_full + 1.96 * sqrt(var_sel)
  beta_naive_CI_do = beta_sel_full - 1.96 * sqrt(var_sel)

  # Naive CI for a linear combination of fixed parameters ------------------
  if (!is.null(x_beta_lin_com)) {
    stopifnot(
      "Number of columns in 'x_beta_lin_com'
        has to be the same as number of covariates in a full model" = NCOL(x_beta_lin_com) == length(beta_sel_full)
    )

    if (is.vector(x_beta_lin_com)) {
      x_beta_lin_com = matrix(x_beta_lin_com,
                              nrow = 1,
                              ncol = length(x_beta_lin_com))
    }
    t_x_beta_lin_com = t(x_beta_lin_com)

    x_beta_invxvx_temp1 = crossprod(t_x_beta_lin_com[indices_sel,],
                                    invxV_fullx[indices_sel, indices_sel])

    if (nrow(x_beta_lin_com) == 1) {
      x_beta_invxvx = x_beta_invxvx_temp1 %*% t_x_beta_lin_com
    } else {
      k = nrow(x_beta_lin_com)
      x_beta_invxvx = numeric(k)

      for (i in 1:k) {
        x_beta_invxvx[i] = t_x_beta_lin_com[indices_sel, i] %*%
          x_beta_invxvx_temp1[i,]
      }
    }

    beta_x_full = crossprod(t_x_beta_lin_com, beta_sel_full)

    beta_x_naive_CI_up = beta_x_full + sqrt(x_beta_invxvx) * 1.96
    beta_x_naive_CI_do = beta_x_full - sqrt(x_beta_invxvx) * 1.96

  }

  #
  # Naive CI for mixed parameter --------------------------------------------------
  #
  type_MSE_mixed = match.arg(type_MSE_mixed)
  K_inv_sel = estimate_invK(X = X_full[, indices_sel], Z, G = G_sel, R = R_sel)

  if (type_MSE_mixed == "regular") {
    # Naive CI for mixed parameter with first-order correct MSE
    mse = compute_mse(C_cluster = C_cluster_sel, K_inv = K_inv_sel)

    mixed_naive_CI_up = mu_sel + 1.96 * sqrt(mse)
    mixed_naive_CI_do = mu_sel - 1.96 * sqrt(mse)
  } else if (type_MSE_mixed == "corrected") {
    mse_results = compute_corrected_mse(
      C_cluster = C_cluster_sel,
      clusterID,
      K_inv = K_inv_sel,
      sig_u = sig_u_sel,
      sig_e = sig_e_sel
    )
    mse_corrected = mse_results$mse_corrected

    mixed_naive_CI_corrected_up = mu_sel + 1.96 * sqrt(mse_corrected)
    mixed_naive_CI_corrected_do = mu_sel - 1.96 * sqrt(mse_corrected)

  } else {
    mse_results = compute_corrected_mse(
      C_cluster = C_cluster_sel,
      clusterID,
      K_inv = K_inv_sel,
      sig_u = sig_u_sel,
      sig_e = sig_e_sel
    )

    mse = mse_results$mse
    mse_corrected = mse_results$mse_corrected

    mixed_naive_CI_corrected_up = mu_sel + 1.96 * sqrt(mse_corrected)
    mixed_naive_CI_corrected_do = mu_sel - 1.96 * sqrt(mse_corrected)

    mixed_naive_CI_up = mu_sel + 1.96 * sqrt(mse)
    mixed_naive_CI_do = mu_sel - 1.96 * sqrt(mse)

  }

  if (is.null(x_beta_lin_com)) {
    if (type_MSE_mixed == "regular") {
      output = list(
        beta_naive_CI_up = beta_naive_CI_up,
        beta_naive_CI_do = beta_naive_CI_do,
        mixed_naive_CI_up = mixed_naive_CI_up,
        mixed_naive_CI_do = mixed_naive_CI_do
      )

    } else if (type_MSE_mixed == "corrected") {
      output = list(
        beta_naive_CI_up = beta_naive_CI_up,
        beta_naive_CI_do = beta_naive_CI_do,
        mixed_naive_CI_corrected_up = mixed_naive_CI_corrected_up,
        mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do
      )
    } else {
      output = list(
        beta_naive_CI_up = beta_naive_CI_up,
        beta_naive_CI_do = beta_naive_CI_do,
        mixed_naive_CI_corrected_up = mixed_naive_CI_corrected_up,
        mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do,
        mixed_naive_CI_up = mixed_naive_CI_up,
        mixed_naive_CI_do = mixed_naive_CI_do
      )
    }

  } else {
    if (type_MSE_mixed == "regular") {
      output = list(
        beta_naive_CI_up = beta_naive_CI_up,
        beta_naive_CI_do = beta_naive_CI_do,
        mixed_naive_CI_up = mixed_naive_CI_up,
        mixed_naive_CI_do = mixed_naive_CI_do,
        beta_x_naive_CI_up = beta_x_naive_CI_up,
        beta_x_naive_CI_do = beta_x_naive_CI_do
      )

    } else if (type_MSE_mixed == "corrected") {
      output = list(
        beta_naive_CI_up = beta_naive_CI_up,
        beta_naive_CI_do = beta_naive_CI_do,
        mixed_naive_CI_corrected_up = mixed_naive_CI_corrected_up,
        mixed_naive_CI_corrected_do = mixed_naive_CI_corrected_do,
        beta_x_naive_CI_up = beta_x_naive_CI_up,
        beta_x_naive_CI_do = beta_x_naive_CI_do
      )
    } else {
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
    }

  }

  output

}
