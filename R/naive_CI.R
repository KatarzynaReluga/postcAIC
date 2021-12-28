#' Naive CI
#'
#' Function \code{naive_CI} provides naive CI for fixed and mixed effects after model selection
#'
#' @inheritParams postcAIC_CI
#' @param sig_u_sel Variance parameter of random effects of the selected model
#' @param sig_e_sel Variance parameter of errors of the selected model
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
#' @importFrom Matrix bdiag
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
#' sig_e_full = cAIC_model_set$sig_u_full
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
                     x_beta_lin_com = NULL) {

  # Recover some parameters -----------------------------------------------
  p_full = ncol(X_full)
  clusterID = validate_observations(clusterID, X_full, cluster = TRUE)
  n = nlevels(clusterID)
  n_cluster_units = as.data.frame(table(clusterID))$Freq

  V_full_list <- list()
  invV_full_list <- list()

  for (i in 1:n) {
    V_full_list[[i]] <- sig_e_full * diag(n_cluster_units[i]) +
                        sig_u_full * matrix(1, nrow = n_cluster_units[i], ncol = n_cluster_units[i])
    invV_full_list[[i]] <- solve(V_full_list[[i]])
  }

  invV_full = bdiag(invV_full_list)

  invxV_fullx = solve(t(X_full) %*% invV_full %*% X_full)

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

    beta_x_naive_CI_up = c(beta_x_full + sqrt(x_beta_invxvx) * 1.96)
    beta_x_naive_CI_do = c(beta_x_full - sqrt(x_beta_invxvx) * 1.96)

  } else {
    beta_x_PoSI_CI_up = "CI for a linear combination of fixed effects not requested"
    beta_x_PoSI_CI_do = "CI for a linear combination of fixed effects not requested"
  }

  #
  # Naive CI for mixed parameter --------------------------------------------------
  #
  type_MSE_mixed = match.arg(type_MSE_mixed)

  if (type_MSE_mixed == "regular") {
    mse = compute_mse(C_cluster = C_cluster_sel,
                      X = X_full[, indices_sel],
                      sig_u = sig_e_sel,
                      sig_e = sig_e_sel,
                      clusterID, model = "NERM")

    mixed_naive_CI_up = c(mu_sel + 1.96 * sqrt(mse))
    mixed_naive_CI_do = c(mu_sel - 1.96 * sqrt(mse))
    mixed_naive_CI_corrected_up = "Corrected CI for mixed effects not requested"
    mixed_naive_CI_corrected_do = "Corrected CI for mixed effects not requested"

  } else if (type_MSE_mixed == "corrected") {
    mse_results = compute_corrected_mse(
      C_cluster = C_cluster_sel,
      X = X_full[, indices_sel],
      sig_u = sig_u_sel,
      sig_e = sig_e_sel,
      clusterID,
      model = "NERM"
    )

    mse_corrected = mse_results$mse_corrected

    mixed_naive_CI_up = "Regular CI for mixed effects not requested"
    mixed_naive_CI_do = "Regular CI for mixed effects not requested"
    mixed_naive_CI_corrected_up = c(mu_sel + 1.96 * sqrt(mse_corrected))
    mixed_naive_CI_corrected_do = c(mu_sel - 1.96 * sqrt(mse_corrected))

  } else {
    mse_results = compute_corrected_mse(
      C_cluster = C_cluster_sel,
      X = X_full[, indices_sel],
      sig_u = sig_u_sel,
      sig_e = sig_e_sel,
      clusterID,
      model = "NERM"
    )

    mse = mse_results$mse
    mse_corrected = mse_results$mse_corrected

    mixed_naive_CI_corrected_up = c(mu_sel + 1.96 * sqrt(mse_corrected))
    mixed_naive_CI_corrected_do = c(mu_sel - 1.96 * sqrt(mse_corrected))

    mixed_naive_CI_up = c(mu_sel + 1.96 * sqrt(mse))
    mixed_naive_CI_do = c(mu_sel - 1.96 * sqrt(mse))

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

  output

}
