#' Estimate inverse of matrix K
#'
#' @param X Matrix with covariates for fixed effects 
#' @param Z Matrix with covariates for random effects covariates
#' @param G Covariance matrix of random effects
#' @param R Covariance matrix of errrors
#'
#' @return Inverse of Matrix K
#'
#'

estimate_invK <- function(X, Z, G, R) {
  C = cbind(X, Z)
  p = ncol(X)
 
  invG = solve(G)
  invR = solve(R)
  n = nrow(G)
  G_plus_inv = rbind(matrix(0, nrow = p,
                            ncol = p + n),
                     cbind(matrix(0, nrow = n, ncol = p),
                           invG))
  K_inv = solve(crossprod(C, t(invR)) %*% C + G_plus_inv)
  K_inv

}
