#' Vectorize Sigma
#'
#' Function \code{vectorise_Sigma} vectorizes and orders
#' unique elements of variance matrix Sigma
#'
#' @param Sigma Matrix Sigma
#' @param index_cov_terms Index of covariance terms (for example,
#' 12, 34, etc)
#' @param p Number of all fixed parameters under consideration (intercept included)
#' @param index_full_model Index of a full model in the model set
#'
#' @return List with elements:
#' * \code{Sigma_ordered} - ordered elements of Sigma
#' * \code{Sigma_order_index} - indices of ordered elements of Sigma
#'
#' @keywords internal
#'
#' @details
#' Matrix \code{Sigma} is defined in Claeskens, Reluga, and Sperlich (2021)
#'
#'


vectorize_Sigma <- function(Sigma,
                            index_cov_terms,
                            p,
                            index_full_model) {
  Sigma_cov = Sigma[t(lower.tri(Sigma))]
  
  Sigma_cov_var0 = c(diag(Sigma), Sigma_cov)
  
  var_index  = strtoi(paste(c(1:p), c(1:p),
                            sep = ""))
  
  order_cov_var = sort(c(var_index,
                         index_cov_terms[[index_full_model]]),
                       index.return = T)$ix
  
  Sigma_ordered = Sigma_cov_var0[order_cov_var]
  
  output <- list(Sigma_ordered = Sigma_ordered,
                 Sigma_order_index = order_cov_var)
  
  output
}
