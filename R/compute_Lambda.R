#' Matrices of the limiting post-selection distribution under NERM
#'
#' Function \code{compute_Lambda} computes, at given variance parameters,
#' the matrices needed for the selection region and the post-selection
#' distribution: \eqn{X^T V^{-1} X}, \eqn{X^T V^{-1} Z G Z^T V^{-1} X},
#' the covariance matrix \eqn{\Lambda} of
#' \eqn{(\xi, \eta) = D^T V^{-1} (y - X \beta_0)} with
#' \eqn{D = [X, Z G Z^T V^{-1} X]}, and the symmetric square root of
#' \eqn{\Lambda}. All matrices are unscaled (not divided by the number
#' of clusters); the selection region and the draws of
#' \eqn{\hat{\beta}(M) - \beta_0(M)} do not depend on this scaling.
#'
#' @param X_full Matrix with the covariates of the full model
#' @param y Vector of responses. Needed only if \code{vcov = "robust"}
#' @param clusterID Vector with cluster labels
#' @param sig_u Variance parameter of random effects (from the full model)
#' @param sig_e Variance parameter of errors (from the full model)
#' @param vcov \code{"model"} for the model-based matrix
#' \eqn{\Lambda = D^T V^{-1} D} (correctly specified models), or
#' \code{"robust"} for the cluster-robust estimator
#' \eqn{\sum_i D_i^T V_i^{-1} \hat{\varepsilon}_i \hat{\varepsilon}_i^T V_i^{-1} D_i}
#' with residuals \eqn{\hat{\varepsilon}_i = y_i - X_i \hat{\beta}(M_F)}
#' from the full model (misspecified models)
#'
#' @return List with elements:
#' \item{I_mat}{Matrix \eqn{X^T V^{-1} X}}
#' \item{G_mat}{Matrix \eqn{X^T V^{-1} Z G Z^T V^{-1} X}}
#' \item{Lambda}{Covariance matrix used: model-based or cluster-robust}
#' \item{Lambda_sqrt}{Symmetric square root of \code{Lambda}}
#' \item{gamma}{Shrinkage factors \eqn{m_i \sigma^2_u / (\sigma^2_e + m_i \sigma^2_u)}}
#' \item{X_bar}{Cluster means of \code{X_full}}
#' \item{m_i}{Cluster sizes}
#'
#' @details
#' For cluster \eqn{i}, \eqn{Z_i G_i Z_i^T V_i^{-1} X_i = \gamma_i 1 \bar{x}_i^T},
#' so that, with \eqn{w_i = 1^T V_i^{-1} 1 = m_i / (\sigma^2_e + m_i \sigma^2_u)},
#' the blocks of \eqn{\Lambda} are \eqn{X^T V^{-1} X},
#' \eqn{\sum_i \gamma_i w_i \bar{x}_i \bar{x}_i^T} and
#' \eqn{\sum_i \gamma_i^2 w_i \bar{x}_i \bar{x}_i^T}.
#' \eqn{\Lambda} may be singular, for instance in a balanced design with
#' an intercept or with cluster-level covariates, so its square root is
#' computed from the eigendecomposition, with negative eigenvalues due to
#' rounding set to zero. Clusters are ordered as the levels of \code{clusterID}.
#'
#' @keywords internal
#'

compute_Lambda <- function(X_full,
                           y = NULL,
                           clusterID,
                           sig_u,
                           sig_e,
                           vcov = c("model", "robust")) {
  vcov = match.arg(vcov)
  X_full = as.matrix(X_full)
  clusterID = droplevels(validate_observations(clusterID, X_full, cluster = TRUE))

  # Cluster sizes, shrinkage factors and cluster means ---------------------
  m_i = as.vector(table(clusterID))
  gamma = m_i * sig_u / (sig_e + m_i * sig_u)
  w = m_i / (sig_e + m_i * sig_u)
  X_bar = rowsum(X_full, clusterID) / m_i

  # Model-based blocks of Lambda -----------------------------------------
  I_mat = (crossprod(X_full) - crossprod(X_bar * sqrt(gamma * m_i))) / sig_e
  G_mat = crossprod(X_bar * sqrt(gamma * w))
  H_mat = crossprod(X_bar * (gamma * sqrt(w)))

  if (vcov == "model") {
    Lambda = rbind(cbind(I_mat, G_mat),
                   cbind(G_mat, H_mat))
  } else {
    stopifnot("Vector 'y' is needed for vcov = 'robust'" = !is.null(y))
    y = as.vector(y)
    y_bar = as.vector(rowsum(y, clusterID)) / m_i

    # Residuals from the full model at (sig_u, sig_e) ----------------------
    XtVinvy = (crossprod(X_full, y) - crossprod(X_bar, gamma * m_i * y_bar)) / sig_e
    beta_full = solve(I_mat, XtVinvy)
    eps = as.vector(y - X_full %*% beta_full)
    eps_bar = as.vector(rowsum(eps, clusterID)) / m_i

    # Cluster scores D_i^T V_i^{-1} eps_i, one row per cluster -------------
    S_xi = (rowsum(X_full * eps, clusterID) - X_bar * (gamma * m_i * eps_bar)) / sig_e
    S_eta = X_bar * (gamma * w * eps_bar)
    Lambda = crossprod(cbind(S_xi, S_eta))
  }

  Lambda_sqrt = sqrt_psd(Lambda)

  output = list(
    I_mat = I_mat,
    G_mat = G_mat,
    Lambda = Lambda,
    Lambda_sqrt = Lambda_sqrt,
    gamma = gamma,
    X_bar = X_bar,
    m_i = m_i
  )
  output
}


#' Matrix Gamma(M) of the limiting quadratic form
#'
#' Function \code{compute_Gamma} computes the \eqn{2p \times 2p} matrix
#' \eqn{\Gamma(M)} such that, for \eqn{M} containing the true model,
#' \eqn{2\ell^c_n\{\hat{\beta}(M), \theta\} - 2\ell^c_n(\beta_0, \theta) =
#' (\xi^T, \eta^T) \Gamma(M) (\xi^T, \eta^T)^T}.
#'
#' @param indices Indices of the covariates in model \eqn{M}
#' @param I_mat Matrix \eqn{X^T V^{-1} X} of the full model
#' @param G_mat Matrix \eqn{X^T V^{-1} Z G Z^T V^{-1} X} of the full model
#'
#' @return Matrix \eqn{\Gamma(M)} with blocks
#' \eqn{\Pi_M^T I(M)^{-1}\{I(M) + G(M)\} I(M)^{-1} \Pi_M},
#' \eqn{-\Pi_M^T I(M)^{-1} \Pi_M} (off-diagonal) and \eqn{0}
#'
#' @keywords internal
#'

compute_Gamma <- function(indices, I_mat, G_mat) {
  p = ncol(I_mat)
  I_M = I_mat[indices, indices, drop = FALSE]
  G_M = G_mat[indices, indices, drop = FALSE]
  I_M_inv = solve(I_M)

  Gamma = matrix(0, nrow = 2 * p, ncol = 2 * p)
  Gamma[indices, indices] = I_M_inv %*% (I_M + G_M) %*% I_M_inv
  Gamma[indices, p + indices] = -I_M_inv
  Gamma[p + indices, indices] = -I_M_inv
  Gamma
}


#' Symmetric square root of a positive semi-definite matrix
#'
#' @param A Symmetric positive semi-definite matrix
#'
#' @return Symmetric matrix \eqn{A^{1/2}}; eigenvalues below zero
#' (rounding errors) are set to zero
#'
#' @keywords internal
#'

sqrt_psd <- function(A) {
  A = (A + t(A)) / 2
  eigen_A = eigen(A, symmetric = TRUE)
  values = pmax(eigen_A$values, 0)
  eigen_A$vectors %*% (sqrt(values) * t(eigen_A$vectors))
}
