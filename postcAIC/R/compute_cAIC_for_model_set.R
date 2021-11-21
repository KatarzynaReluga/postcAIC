#' Construct post-cAIC confidence intervals
#'
#' Function \code{postcAIC} provides post-cAIC confidence intervals
#' for fixed and mixed parameters under LMM
#'
#' @inheritParams estimate_NERM
#' @inheritParams create_modelset
#' @inheritParams create_Z
#' @param covariate_selection_matrix Matrix composed of zeros and ones indicating fixed models in each parameter.
#' Default: intercept = TRUE

#' @return List with parameters
#' * \code{cAIC_min}  - Index of selected model
## #' * cAIC_models -
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
#'
#' @details
#' * \code{model}  - for the moments, only NERM is supported.
#' * \code{covariate_selection_matrix} - necessary only if modelset = NULL
#'
#' @importFrom stats aggregate
#'
#' @export
#'

compute_cAIC_for_model_set = function(X, y,
                                      clusterID,
                                      X_cluster = NULL,
                                      model = "NERM",
                                      covariate_selection_matrix = NULL,
                                      modelset  = "all_subsets",
                                      intercept = FALSE,
                                      common = NULL) {


  # Validate and format data matrix X, y, clusterID, Z ----------

  if (identical(intercept, FALSE)) {
    X = cbind(rep(1, nrow(X)), X)
  }

  X_full = format_data_matrix(validate_matrix(X), name_col = "X")
  t_X_full = t(X_full)

  X_cluster_full = aggregate(X_full ~ clusterID, FUN = "mean")[,-1]
  t_X_cluster_full = t(X_cluster_full)

  clusterID = validate_observations(clusterID, X_full, cluster = TRUE)
  y = validate_observations(y, X_full)

  p_full = ncol(X_full)
  n = nlevels(clusterID)

  Z = create_Z(model = model, clusterID)

  # Create model sets --------------------------------------

  if (!is.null(modelset)) {
    modelset_matrix  = create_modelset(modelset, p = p_full, common)
  } else {
    modelset_matrix = covariate_selection_matrix
  }

  # Estimate parameters in the biggest model ----------------------------------------
  params_full = estimate_NERM(X = X_full, y,
                              clusterID = clusterID,
                              X_cluster = X_cluster_full)

  invV_full = params_full$invV
  V_full = params_full$V
  invR_full = params_full$invR
  R_full = params_full$R
  G_full = params_full$G
  sig_e_full = params_full$sig_e
  sig_u_full = params_full$sig_u

  # Calculate cAIC for models in model set ------------------------

  cAIC_models = numeric(nrow(modelset_matrix))
  degcAIC_models = numeric(nrow(modelset_matrix))
  beta_models = list()
  mu_models = list()
  R_models = list()
  G_models = list()
  V_models = list()
  sig_u_models = numeric(nrow(modelset_matrix))
  sig_e_models = numeric(nrow(modelset_matrix))

  for (k in 1 : nrow(modelset_matrix)) {
    covariates = modelset_matrix[k, ]
    indices0 <- (1:ncol(X_full)) * covariates
    indices = indices0[indices0!=0]
    X <- format_data_matrix(as.matrix(X_full[, indices]))
    X_cluster <- format_data_matrix(as.matrix(X_cluster_full[, indices]))


    params = estimate_NERM(X, y,
                           clusterID = clusterID,
                           X_cluster = X_cluster)

    beta_models[[k]] = params$beta
    mu_models[[k]] = params$mu
    R_models[[k]] = params$R
    G_models[[k]] = params$G
    V_models[[k]] = params$V
    sig_u_models[k] = params$sig_u
    sig_e_models[k] = params$sig_e

    IC = compute_information_criteria(X, y, Z,
                                      detV = params$detV,
                                      invV = params$invV,
                                      detR = params$detR,
                                      invR = params$invR,
                                      G = params$G,
                                      sig_u = params$sig_u,
                                      sig_e = params$sig_e,
                                      fit_model_fixed = params$fit_model_fixed,
                                      fit_model_mixed = params$fit_model_mixed,
                                      size_fixed_params = ncol(X) + 2)
    cAIC_models[k] = IC$cAIC
    degcAIC_models[k] = IC$deg_cAIC
  }

  cAIC_min = which.min(cAIC_models)
  beta_sel  = beta_models[[cAIC_min]]
  mu_sel  = mu_models[[cAIC_min]]
  R_sel  = R_models[[cAIC_min]]
  G_sel  = G_models[[cAIC_min]]
  V_sel  = V_models[[cAIC_min]]
  sig_u_sel = sig_u_models[cAIC_min]
  sig_e_sel = sig_e_models[cAIC_min]

  covariates_sel = modelset_matrix[cAIC_min, ]
  indices0_sel <- (1:ncol(X_full)) * covariates_sel
  indices_sel = indices0_sel[indices0_sel != 0]
  X_sel <- format_data_matrix(as.matrix(X_full[, indices_sel]))


  output = list(cAIC_min = cAIC_min,
#                cAIC_models = cAIC_models,
                degcAIC_models = degcAIC_models,

                beta_sel = beta_sel,
                mu_sel = mu_sel,
                G_sel = G_sel,
                R_sel = R_sel,
                V_sel = V_sel,
                X_sel = X_sel,
                indices_sel = indices_sel,
                sig_e_sel = sig_e_sel,
                sig_u_sel = sig_u_sel,

                invV_full = invV_full,
                V_full = V_full,
                invR_full = invR_full,
                R_full = R_full,
                G_full = G_full,
                X_full = X_full,
                sig_e_full = sig_e_full,
                sig_u_full = sig_u_full,
                X_cluster_full = X_cluster_full,
                Z = Z,
                modelset_matrix = modelset_matrix)

  output
}
