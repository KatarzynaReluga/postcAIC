#' Post OBSP confidence intervals
#'
#' Function \code{postOBSP_CI} provides post-OBSP confidence intervals for mixed parameters
#'
#' @inheritParams estimate_NERM
#' @inheritParams postcAIC_CI
#' @param sig_u_full Variance parameter of random effects from the full model
#' @param sig_e_full Variance parameter of errors from the full model
#' @param boot Number of boostrap samples
#'
#' @details
#' * The parameter \code{boot} is needed for the calculation of the boostrap post-OBSP MSE of mixed effects
#'
#'
#' @importFrom stats aggregate
#' @importFrom stats rnorm
#'
#' @export
#'

postOBSP_CI <- function(X_full, y, clusterID, Z, X_cluster_full,
                        G_full, V_full, R_full,
                        sig_u_full, sig_e_full,
                        modelset_matrix,
                        boot = 1000) {

  # Recover necessary parameters -------------------------------------------------------------------

  p_full = ncol(X_full)
  m_total = nrow(Z)
  n = nrow(G_full)
  m_i = as.data.frame(table(clusterID))$Freq

  # Prepare for the computation of OBSP for the set of models ---------------------------------------------

  t_Z = t(Z)
  invV_full = solve(V_full)
  Ga = as.matrix(diag(m_total) - Z%*%G_full%*%t_Z%*%invV_full)
  Ga_sq = crossprod(Ga)
  gamma_full = sig_u_full/(sig_u_full+sig_e_full/m_i)
  g1_full = (1 - gamma_full) * sig_u_full
  y_cluster_mean = aggregate(y ~ clusterID, FUN = "mean")[,-1]

  beta_model_matrix = matrix(0, nrow = nrow(modelset_matrix),
                          ncol = p_full)

  OBSP_models = numeric(nrow(modelset_matrix))

  # Compute OBSP for the set of models ---------------------------------------------------------------------
  #X_list = list()
  mu_hat_model = list()
  #Ga = diag(m_total)-Z%*%G_m5%*%t.Z%*%solV_m5
  for (k in 1 : nrow(modelset_matrix)) {
    covariates = modelset_matrix[k, ]
    indices0 <- (1:ncol(X_full)) * covariates
    indices = indices0[indices0!=0]
    X = format_data_matrix(as.matrix(X_full[, indices]))
    #  X_list[[k]] <- X
    t_X = t(X)
    X_cluster <- format_data_matrix(as.matrix(X_cluster_full[, indices]))

    temp = as.matrix(solve(crossprod(X, crossprod(Ga_sq, X))))
    beta_model = temp%*%t_X%*%Ga_sq%*%y
    #y_Xbeta_dif = y - X%*%beta_temp
    y_Xbeta_dif = y - crossprod(t_X, beta_model)
    P_model = Ga%*%X%*%temp%*%t_X%*%Ga
    penalty = sum(diag(P_model%*%Ga%*%as.matrix(R_full)))
    OBSP_models[k] = t(y_Xbeta_dif)%*%Ga_sq%*%(y_Xbeta_dif) +
      2 * penalty

    u_model = gamma_full*(y_cluster_mean - X_cluster%*%beta_model)
    mu_hat_model[[k]] = X_cluster%*%beta_model + u_model

    beta_model_matrix[k, indices] <- beta_model

  }

  OBSP_min = which.min(OBSP_models)

  covariates_sel = modelset_matrix[OBSP_min, ]
  indices0_sel <- (1:ncol(X_full)) * covariates_sel
  indices_sel = indices0_sel[indices0_sel != 0]

  # Compute bootstrap MSE for the mixed effect in the selected model ----------------------------------------------------------

  u_boot_true = matrix(0, nrow = boot, ncol = n)
  mu_boot_true = matrix(0, nrow = boot, ncol = n)

  #y_boot=matrix(0,nrow=boot,ncol=m_total)

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


    # Boot true mu  ------------------------------------------------------------------------------


    u_boot_true[b, ] = gamma_full * (y_cluster_mean_boot - as.matrix(X_cluster_full) %*%
                                       beta_select)
    mu_boot_true[b, ] = as.matrix(X_cluster_full) %*% beta_select + u_boot_true[b, ]


    # Estimate boostrap data ----------------------------------------------------------------------------------

    params = estimate_NERM(
      X = X_full[, indices_sel],
      y = y_boot,
      clusterID = clusterID,
      X_cluster = X_cluster_full[, indices_sel]
    )

    sig_u_full_boot[b] = params$sig_u
    sig_e_full_boot[b] = params$sig_e

    R_boot = params$R
    G_boot = params$G
    V_boot = params$V

    invV_boot = params$invV
    invR_boot = params$invR


    #################
    Ga_boot = as.matrix(diag(m_total) - Z %*% G_boot %*% t_Z %*% invV_boot)
    Ga_sq_boot = crossprod(Ga_boot)

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

  # Construct interval ---------------------------------------------------------------------------------
  mu_hat_sel = mu_hat_model[[OBSP_min]]
  postOBSP_up = mu_hat_sel + 1.96 * rmse_b
  postOBSP_do = mu_hat_sel - 1.96 * rmse_b

  output = list(postOBSP_up = postOBSP_up,
                postOBSP_do = postOBSP_do,
                mu_hat_sel = mu_hat_sel)
  output

}


