#' Post-cAIC confidence intervals
#'
#' Function \code{postcAIC_CI} provides post-cAIC confidence intervals for
#' mixed and fixed effects under NERM, following Algorithm 1 of the paper.
#' The model must have been selected with
#' \code{compute_cAIC_for_model_set(..., theta = "full")}, that is with cAIC
#' computed for every candidate model at the estimate of the variance
#' parameters from the full model.
#'
#' @param degcAIC_models Penalty for all considered models
#' @param cAIC_min Index of the selected model among models in the model set
#' @param X_full Matrix with a full set of covariates
#' @param X_cluster_full Matrix with cluster level covariates for fixed effects of the full model
#' @param model Type of mixed model. For the moment, only NERM is supposted.
#' @param clusterID  Vector with cluster labels
#' @param sig_u_full Variance parameter of random effects from the full model
#' @param sig_e_full Variance parameter of errors from the full model
#' @param beta_sel Fixed effects (regression parameters) of the selected model,
#' computed at the variance parameters of the full model
#' @param mu_sel Mixed effects of the selected model, computed at the
#' variance parameters of the full model
#' @param x_beta_lin_com Vector or matrix to create linear combinations with
#' fixed parameters. Default: \code{x_beta_lin_com = NULL}
#' @param modelset_matrix Matrix composed of zeros and ones
#' @param n_starting_points Number of initial starting points for sampling from
#' a truncated normal distribution. Default: \code{n_starting_points = NULL}
#' @param scale_mvrnorm Variance of the normal distribution from which candidate
#' starting points of the Markov chain samplers are drawn (see
#' \code{find_starting_points}). Default: \code{scale_mvrnorm = 1}.
#' @param alpha Construct 1 - alpha confidence intervals. Default: \code{alpha = 0.05}
#' @param theta How the variance parameters were estimated for the selection
#' (element \code{theta} of the output of \code{compute_cAIC_for_model_set}).
#' Required, and must be \code{"full"}.
#' @param cAIC_models cAIC for all considered models (element \code{cAIC_models}
#' of the output of \code{compute_cAIC_for_model_set}). Required.
#' @param y Vector of responses. Needed only if \code{vcov = "robust"}.
#' Default: \code{y = NULL}
#' @param c_n Threshold defining the estimated set of overparametrized
#' models, \eqn{\{M: cAIC(M) - cAIC(M_*) \le c_n\}}; the selected model is
#' compared only with these models. Any \eqn{c_n \to \infty} with
#' \eqn{c_n / n \to 0} is valid. Default: \code{c_n = NULL}, which gives
#' \eqn{c_n = 2 \log n}, \eqn{n} the number of clusters.
#' \code{c_n = Inf} compares the selected model with all candidate models.
#' @param var_psi Variance of the term \eqn{\psi_i} in the intervals for mixed
#' effects: \code{"g1"} (default) for \eqn{g_{1i}}, as in the asymptotic
#' theory, or \code{"g1+2g3"} for \eqn{g_{1i} + 2 g_{3i}}, which adds the
#' second-order term of Prasad and Rao for the estimation of the variance
#' parameters
#' @param vcov Estimate of the covariance matrix \eqn{\Lambda}:
#' \code{"model"} (default) for the model-based estimate (correctly specified
#' models, Section 3 of the paper) or \code{"robust"} for the cluster-robust
#' estimate (misspecified models, Section 4 of the paper)
#' @param n_samples Number of samples from each Markov chain.
#' Default: \code{n_samples = 10000}
#' @param burn.in Number of burn-in iterations of each Markov chain.
#' Default: \code{burn.in = 1000}
#' @param sampler Method to sample from the truncated normal distribution:
#' \code{"rejection"} (default) for rejection sampling, with elliptical slice
#' sampling as a fallback when the acceptance rate is below \code{min_acceptance},
#' or \code{"tmg"} for function \code{rtmg} of package \code{tmg}, which must be
#' installed (it is archived on CRAN). See \code{sample_with_constraints}.
#' @param min_acceptance Smallest acceptance rate for which rejection sampling
#' is used. Default: \code{min_acceptance = 0.005}
#'
#'
#' @return List with parameters:
#' \item{beta_postcAIC_CI_up}{Upper boundary of CI for fixed effects (zero for
#' covariates not in the selected model)}
#' \item{beta_postcAIC_CI_do}{Lower boundary of CI for fixed effects (zero for
#' covariates not in the selected model)}
#' \item{mixed_postcAIC_CI_up}{Upper boundary of CI for mixed effects}
#' \item{mixed_postcAIC_CI_do}{Lower boundary of CI for mixed effects}
#' \item{beta_x_postcAIC_CI_up}{Upper boundary of CI for linear combinations of fixed effects}
#' \item{beta_x_postcAIC_CI_do}{Lower boundary of CI for linear combinations of fixed effects}
#' \item{models_compared}{Indices (rows of \code{modelset_matrix}) of the models
#' the selected model is compared with}
#' \item{c_n}{Threshold used to define the estimated set of overparametrized models}
#' \item{sampler_used}{Sampler actually used: \code{"rejection"},
#' \code{"elliptical slice"}, \code{"tmg"} or \code{"none"} (no constraints)}
#' \item{acceptance_rate}{Acceptance rate of rejection sampling in the pilot batch}
#'
#' @details
#' The intervals are \eqn{[\hat{\beta}_j - q_{1-\alpha/2}, \hat{\beta}_j - q_{\alpha/2}]},
#' where \eqn{q_\gamma} are empirical quantiles of draws from the post-selection
#' distribution of \eqn{\hat{\beta}(M_*) - \beta_0(M_*)}, and similarly for
#' linear combinations and mixed effects.
#'
#' The total number of draws is \code{2 * n_starting_points * n_samples}
#' for every sampler.
#'
#' With \code{sampler = "tmg"}, or when elliptical slice sampling is used and
#' the pilot batch has too few accepted draws, the running time of function
#' \code{postcAIC_CI} is
#' sensitive to the choice of scaling factor \code{scale_mvrnorm} which affects
#' the speed of finding initial starting points in function \code{find_starting_points}.
#' These starting points are necessary to sample from a constrained normal distribution
#' from which we obtain critical values to construct confidence intervals. We suggest
#' choosing a larger/smaller value if the running time of function \code{postcAIC_CI}
#' is longer than 1 minute.
#'
#' @importFrom stats quantile rnorm
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
#' e_ij = rnorm(m_total, 0, sqrt(sig_e))
#'
#' u_i = rnorm(n, 0, sqrt(sig_u))
#' u_i_aug = rep(u_i, each = m_i)
#'
#' y = X_intercept%*% beta + u_i_aug + e_ij
#'
#' # Compute cAIC for models from the set of models -----------------------
#'
#'
#' cAIC_model_set = compute_cAIC_for_model_set(
#'   X,
#'   y,
#'   clusterID,
#'   model = "NERM",
#'   covariate_selection_matrix = NULL,
#'   modelset  = "part_subset",
#'   common = c(1:3),
#'   intercept = FALSE,
#'   theta = "full"
#' )
#'
#' cAIC_min = cAIC_model_set$cAIC_min
#' degcAIC_models = cAIC_model_set$degcAIC_models
#' X_full = cAIC_model_set$X_full
#' X_cluster_full = cAIC_model_set$X_cluster_full
#'
#' sig_u_full = cAIC_model_set$sig_u_full
#' sig_e_full = cAIC_model_set$sig_e_full
#'
#' beta_sel = cAIC_model_set$beta_sel
#' mu_sel = cAIC_model_set$mu_sel
#' cAIC_models = cAIC_model_set$cAIC_models
#'
#' modelset_matrix = cAIC_model_set$modelset_matrix
#' x_beta_lin_com = cAIC_model_set$X_cluster_full
#'
#' # Post-cAIC CI for mixed and fixed parameters ------------------------------------
#'
#' postcAIC_CI_results = postcAIC_CI(
#'   cAIC_min,
#'   degcAIC_models,
#'   X_full,
#'   X_cluster_full,
#'   sig_u_full,
#'   sig_e_full,
#'   model = "NERM",
#'   clusterID,
#'   beta_sel,
#'   mu_sel,
#'   modelset_matrix,
#'   x_beta_lin_com = NULL,
#'   cAIC_models = cAIC_models,
#'   theta = cAIC_model_set$theta)
#'
#'   plot(postcAIC_CI_results)
#' @export
#'
#'

postcAIC_CI  = function(cAIC_min,
                        degcAIC_models,
                        
                        X_full,
                        X_cluster_full,
                        sig_u_full,
                        sig_e_full,
                        model = "NERM",
                        clusterID,
                        
                        beta_sel,
                        mu_sel,
                        
                        modelset_matrix,
                        x_beta_lin_com = NULL,
                        n_starting_points = 5,
                        scale_mvrnorm = 1,
                        alpha = 0.05,
                        cAIC_models,
                        theta,
                        y = NULL,
                        c_n = NULL,
                        var_psi = c("g1", "g1+2g3"),
                        vcov = c("model", "robust"),
                        n_samples = 10000,
                        burn.in = 1000,
                        sampler = c("rejection", "tmg"),
                        min_acceptance = 0.005) {
  # Check arguments -------------------------------------------------------
  stopifnot("Parameter alpha must be between 0 and 1" = 0 < alpha &
              alpha < 1)
  if (missing(cAIC_models)) {
    stop(
      "Argument 'cAIC_models' (element 'cAIC_models' of the output of ",
      "compute_cAIC_for_model_set) is needed to estimate the set of ",
      "overparametrized models."
    )
  }
  if (missing(theta)) {
    stop(
      "Argument 'theta' (element 'theta' of the output of ",
      "compute_cAIC_for_model_set) is required."
    )
  }
  if (!identical(theta, "full")) {
    stop(
      "postcAIC_CI requires the selection to be computed at the variance ",
      "estimate of the full model: use compute_cAIC_for_model_set(..., theta = \"full\")."
    )
  }
  var_psi = match.arg(var_psi)
  vcov = match.arg(vcov)
  sampler = match.arg(sampler)
  
  # Parameters to recover -------------------------------------------------
  X_full = as.matrix(X_full)
  X_cluster_full = as.matrix(X_cluster_full)
  p_full = ncol(X_full)
  clusterID = droplevels(validate_observations(clusterID, X_full, cluster = TRUE))
  n = nlevels(clusterID)
  
  indices_selected = which(modelset_matrix[cAIC_min, ] != 0)
  
  # Step 2: matrices of the limiting distribution at the full-model estimate
  Lambda_results = compute_Lambda(
    X_full = X_full,
    y = y,
    clusterID = clusterID,
    sig_u = sig_u_full,
    sig_e = sig_e_full,
    vcov = vcov
  )
  I_mat = Lambda_results$I_mat
  G_mat = Lambda_results$G_mat
  Lambda_sqrt = Lambda_results$Lambda_sqrt
  gamma = Lambda_results$gamma
  X_bar = Lambda_results$X_bar
  m_i = Lambda_results$m_i
  
  # Step 2: estimated set of overparametrized models ------------------------
  if (is.null(c_n)) {
    c_n = 2 * log(n)
  }
  models_compared = which(cAIC_models - cAIC_models[cAIC_min] <= c_n)
  models_compared = setdiff(models_compared, cAIC_min)
  
  # Step 3: sample from the truncated distribution ------------------------
  list_constraints  = construct_constraints(
    degcAIC_models = degcAIC_models,
    cAIC_min = cAIC_min,
    models_compared = models_compared,
    modelset_matrix = modelset_matrix,
    I_mat = I_mat,
    G_mat = G_mat,
    Lambda_sqrt = Lambda_sqrt
  )
  
  sample_results = sample_with_constraints(
    n_starting_points = n_starting_points,
    dim_w = 2 * p_full,
    n_models_to_compare = length(models_compared),
    list_constraints = list_constraints,
    n_samples = n_samples,
    burn.in = burn.in,
    scale_mvrnorm = scale_mvrnorm,
    sampler = sampler,
    min_acceptance = min_acceptance
  )
  sample_w = sample_results$sample_w
  
  # Step 3: draws of beta_hat(M_*) - beta_0(M_*) --------------------------
  # delta = I(M_*)^{-1} Pi_{M_*} (I_p, 0) Lambda^{1/2} w, one row per draw
  I_selected_inv = solve(I_mat[indices_selected, indices_selected, drop = FALSE])
  map_delta = I_selected_inv %*% Lambda_sqrt[indices_selected, , drop = FALSE]
  sample_delta = tcrossprod(sample_w, map_delta)
  
  # Step 4: post-cAIC CI for beta -----------------------------------------
  q_beta_u = apply(sample_delta,
                   2,
                   quantile,
                   probs = 1 - alpha / 2,
                   type = 8)
  q_beta_d = apply(sample_delta,
                   2,
                   quantile,
                   probs = alpha / 2,
                   type = 8)
  
  full_beta_hat = numeric(p_full)
  full_beta_hat[indices_selected] <- beta_sel
  
  beta_postcAIC_CI_up = numeric(p_full)
  beta_postcAIC_CI_do = numeric(p_full)
  beta_postcAIC_CI_up[indices_selected] = beta_sel - q_beta_d
  beta_postcAIC_CI_do[indices_selected] = beta_sel - q_beta_u
  
  # Step 4: post-cAIC CI for linear combinations of betas ------------------
  if (!is.null(x_beta_lin_com)) {
    if (is.vector(x_beta_lin_com)) {
      x_beta_lin_com = matrix(x_beta_lin_com,
                              nrow = 1,
                              ncol = length(x_beta_lin_com))
    }
    x_beta_lin_com = as.matrix(x_beta_lin_com)
    stopifnot(
      "Number of columns in 'x_beta_lin_com'
        has to be the same as number of covariates in a full model" =
        NCOL(x_beta_lin_com) == length(full_beta_hat)
    )
    
    x_beta_selected = x_beta_lin_com[, indices_selected, drop = FALSE]
    beta_x_full = as.vector(x_beta_lin_com %*% full_beta_hat)
    
    beta_x_postcAIC_CI_up = numeric(nrow(x_beta_lin_com))
    beta_x_postcAIC_CI_do = numeric(nrow(x_beta_lin_com))
    
    for (k in 1:nrow(x_beta_lin_com)) {
      sample_beta_x = as.vector(sample_delta %*% x_beta_selected[k, ])
      q_betax = quantile(sample_beta_x,
                         probs = c(alpha / 2, 1 - alpha / 2),
                         type = 8)
      beta_x_postcAIC_CI_up[k] = beta_x_full[k] - q_betax[1]
      beta_x_postcAIC_CI_do[k] = beta_x_full[k] - q_betax[2]
    }
  } else {
    beta_x_postcAIC_CI_up = "CI for a linear combination of fixed effects not requested"
    beta_x_postcAIC_CI_do = "CI for a linear combination of fixed effects not requested"
  }
  
  # Step 5: post-cAIC CI for mixed effects ---------------------------------
  # d_i(M_*) = k_i(M_*) - gamma_i xbar_i(M_*), with k_i the cluster-level covariates
  d_selected = X_cluster_full[, indices_selected, drop = FALSE] -
    gamma * X_bar[, indices_selected, drop = FALSE]
  
  g1 = gamma * sig_e_full / m_i
  if (var_psi == "g1") {
    var_psi_values = g1
  } else {
    var_psi_values = g1 + 2 * compute_g3_NERM(m_i, sig_u_full, sig_e_full)
  }
  
  mu_sel = as.vector(mu_sel)
  n_draws = nrow(sample_delta)
  mixed_postcAIC_CI_up = numeric(n)
  mixed_postcAIC_CI_do = numeric(n)
  
  for (i in 1:n) {
    sample_tau = rnorm(n_draws, mean = 0, sd = sqrt(var_psi_values[i])) +
      as.vector(sample_delta %*% d_selected[i, ])
    q_mixed = quantile(sample_tau,
                       probs = c(alpha / 2, 1 - alpha / 2),
                       type = 8)
    mixed_postcAIC_CI_up[i] = mu_sel[i] - q_mixed[1]
    mixed_postcAIC_CI_do[i] = mu_sel[i] - q_mixed[2]
  }
  
  output = list(
    beta_postcAIC_CI_up = beta_postcAIC_CI_up,
    beta_postcAIC_CI_do = beta_postcAIC_CI_do,
    mixed_postcAIC_CI_up = mixed_postcAIC_CI_up,
    mixed_postcAIC_CI_do = mixed_postcAIC_CI_do,
    beta_x_postcAIC_CI_up = beta_x_postcAIC_CI_up,
    beta_x_postcAIC_CI_do = beta_x_postcAIC_CI_do,
    models_compared = models_compared,
    c_n = c_n,
    sampler_used = sample_results$sampler_used,
    acceptance_rate = sample_results$acceptance_rate
  )
  
  class(output)  <- "postcAIC_CI"
  output
}
