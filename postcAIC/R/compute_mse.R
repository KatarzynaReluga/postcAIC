#' Compute MSE
#'
#' Function \code{compute_mse} provides first order correct MSE for mixed effects
#'
#' @param C_cluster Cluster-level covariates for fixed and random parameters
#' @param K_inv Inverse of K matrix
#'
#' @return mse MSE of mixed effects
#'
#' @export

compute_mse <- function(C_cluster, K_inv) {
  stopifnot("ncol(C_cluster) must be the same as nrow(K_inv)" = ncol(C_cluster) == nrow(K_inv))
  mse = diag(C_cluster %*% K_inv %*% t(C_cluster))
  mse
}
