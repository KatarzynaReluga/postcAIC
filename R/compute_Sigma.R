#' Compute Sigma
#'
#' Function \code{compute_Sigma} computes matrix Sigma
#'
#' @param X Matrix with covariates for fixed effects
#' @param invR Inverse of covariance matrix R of errors
#' @param invV Inverse of covariance matrix V of response variable
#'
#' @return List with elements:
#' * \code{Sigma} - matrix Sigma
#' * \code{sqrt_invxVx} - square root of variance matrix of fixed effects
#'
#' @keywords internal
#'

compute_Sigma <- function(X, invR, invV) {
  t_X = t(X)
  invxVx = solve(t_X %*% invV %*% X)
  invxRx = solve(t_X %*% invR %*% X)
  
  xVx = t_X %*% invV %*% X
  xRx = t_X %*% invR %*% X
  
  Vvec = eigen(invxVx)$vectors
  invVvec = solve(Vvec)
  Vval = sqrt(eigen(invxVx)$values)
  sqrt_invxVx = Vvec %*% diag(Vval) %*% invVvec
  
  Sigma = sqrt_invxVx %*% xRx %*% sqrt_invxVx
  
  output = list(Sigma = Sigma,
                sqrt_invxVx = sqrt_invxVx)
  output
}
