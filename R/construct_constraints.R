#' Construct constraints
#'
#' Function \code{construct_constraints} provides a list of constraints
#' for multivariate normal distributions
#'
#' @param degcAIC_models Penalty for all considered models
#' @param cAIC_min Index of the selected model among models in the \code{modelset}
#' @param var_cov_terms Variance and covariance terms in each
#' model (appropriate elements from matrix Sigma)
#' @param p Number of all fixed parameters under consideration (intercept included)
#'
#' @return 
#' \item{list_constraints}{List with constraints} 
#' 
#' @importFrom ks invvech
#'
#' @keywords internal
#'

construct_constraints <- function(degcAIC_models,
                                  cAIC_min,
                                  var_cov_terms,
                                  p) {
  list_constraints = list()
  
  degcAIC_models_not_cAIC_min <- degcAIC_models[-cAIC_min]
  n_not_selected_models  = length(degcAIC_models_not_cAIC_min)
  
  for (k in 1:n_not_selected_models) {
    A = invvech(var_cov_terms[k,])
    B = rep(0, p)
    C = -2 * degcAIC_models[cAIC_min] +
      2 * degcAIC_models_not_cAIC_min[k]
    list_constraints[[k]] = list(A, B, C)
  }
  
  list_constraints
}
