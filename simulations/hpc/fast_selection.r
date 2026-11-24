######################################################################
# Fast version of the selection rule of the simulations, used in the #
# congruency check of the method of Ruegamer et al. (RBG).           #
#                                                                    #
# It gives the same cAIC values as                                   #
#   compute_cAIC_for_model_set(X, y, clusterID, model = "NERM",      #
#                              intercept = FALSE, theta = "full")    #
# for the candidate models in modelset_matrix:                       #
#   - the variance parameters are estimated under the full model     #
#     with the same function of the package (estimate_NERM),         #
#   - every candidate model is fitted at this estimate, and its      #
#     cAIC is computed from sums over the clusters instead of        #
#     matrices with m_total rows. The penalty is the trace of the    #
#     hat matrix, |M| + sum_i gamma_i - tr{I(M)^{-1} G(M)}, plus the #
#     term b_c of compute_information_criteria.                      #
# Observations must be sorted by cluster.                            #
#                                                                    #
# Usage:                                                             #
#   select_model_fast = make_fast_selection(X, clusterID,            #
#                                           modelset_matrix)         #
#   select_model_fast(y)$cAIC_min                                    #
# X: covariates without the intercept, as in sim_postcAIC.r.         #
######################################################################

make_fast_selection = function(X, clusterID, modelset_matrix) {
  # Quantities that do not depend on y, prepared as in
  # compute_cAIC_for_model_set
  X_full = postcAIC:::format_data_matrix(
    postcAIC:::validate_matrix(cbind(rep(1, nrow(X)), X)),
    name_col = "X"
  )
  cluster_factor = postcAIC:::validate_observations(clusterID, X_full,
                                                    cluster = TRUE)
  stopifnot("Observations must be sorted by cluster ('clusterID')" =
              !is.unsorted(as.integer(cluster_factor)))

  cluster_index = as.integer(cluster_factor)
  m_i = as.vector(table(cluster_factor))
  m_total = nrow(X_full)
  X_bar = rowsum(X_full, cluster_index) / m_i    # cluster means
  XtX = crossprod(X_full)
  model_indices = lapply(seq_len(nrow(modelset_matrix)),
                         function(k) which(modelset_matrix[k, ] != 0))

  function(y) {
    y = postcAIC:::validate_observations(y, X_full)

    # Variance parameters estimated under the full model, as in the package
    fit_full = postcAIC::estimate_NERM(X_full, y, clusterID = cluster_factor,
                                       X_cluster = X_bar)
    sig_u = fit_full$sig_u
    sig_e = fit_full$sig_e

    gamma = m_i * sig_u / (sig_e + m_i * sig_u)
    y_bar = as.vector(rowsum(y, cluster_index)) / m_i
    Xty = as.vector(crossprod(X_full, y))
    Xbar_y = as.vector(crossprod(X_bar, gamma * m_i * y_bar))
    # sum_i gamma_i m_i xbar_i xbar_i^T
    W = crossprod(X_bar * sqrt(gamma * m_i))
    # sum_i gamma_i (1 - gamma_i) m_i xbar_i xbar_i^T = sig_e * G
    K = crossprod(X_bar * sqrt(gamma * (1 - gamma) * m_i))
    b_c = 1 + 2 / (m_total - 1) + 2 / (1 + m_total * sig_u / sig_e)

    cAIC_models = sapply(model_indices, function(idx) {
      # sig_e * I(M) = X_M^T X_M - sum_i gamma_i m_i xbar_iM xbar_iM^T
      S = XtX[idx, idx, drop = FALSE] - W[idx, idx, drop = FALSE]
      beta = solve(S, Xty[idx] - Xbar_y[idx])
      u = gamma * (y_bar - as.vector(X_bar[, idx, drop = FALSE] %*% beta))
      residuals = y - as.vector(X_full[, idx, drop = FALSE] %*% beta) -
        u[cluster_index]
      penalty = length(idx) + sum(gamma) -
        sum(diag(solve(S, K[idx, idx, drop = FALSE])))
      m_total * log(2 * pi) + m_total * log(sig_e) +
        sum(residuals^2) / sig_e + 2 * (penalty + b_c)
    })

    list(cAIC_min = which.min(cAIC_models), cAIC_models = cAIC_models)
  }
}
