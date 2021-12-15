#' Estimate inverse of matrix K
#'
#' @param X Matrix with covariates for fixed effects
#' @param Z Matrix with covariates for random effects covariates
#' @param G Covariance matrix of random effects
#' @param R Covariance matrix of errors
#' @param invG Inverse of covariance matrix of random effects (optional)
#' @param invR Inverse of covariance matrix of errors (opional)
#'
#'
#' @return Inverse of Matrix K
#'
#'

estimate_invK <- function(X, Z, G, R , invG = NULL, invR = NULL) {
  C = cbind(X, Z)
  p = ncol(X)

  if (is.null(invG)) {
    stopifnot("G cannot be empty if 'invG = NULL' ", !is.null(G))
    invG = solve(G)
  }

  if (is.null(invR)) {
    stopifnot("G cannot be empty if 'invG = NULL' ", !is.null(R))
    invG = solve(R)
  }

  n = nrow(G)
  G_plus_inv = rbind(matrix(0, nrow = p,
                            ncol = p + n),
                     cbind(matrix(0, nrow = n, ncol = p),
                           invG))
  K_inv2 = solve(crossprod(t(crossprod(C, t(invR))), C) + G_plus_inv)
  K_inv

}

