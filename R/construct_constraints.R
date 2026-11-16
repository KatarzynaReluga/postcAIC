#' Construct constraints
#'
#' Function \code{construct_constraints} provides the list of quadratic
#' constraints which define the selection region, that is the set of
#' \eqn{w} in \eqn{R^{2p}} such that, for every compared model \eqn{M_j},
#' \deqn{w^T \Lambda^{1/2} \{\Gamma(M_*) - \Gamma(M_j)\} \Lambda^{1/2} w
#' \ge 2 \{\nu(M_*) - \nu(M_j)\}.}
#'
#' @param degcAIC_models Penalty for all considered models
#' @param cAIC_min Index of the selected model among models in the \code{modelset}
#' @param models_compared Indices (rows of \code{modelset_matrix}) of the models
#' the selected model is compared with, that is the estimated set of
#' overparametrized models without the selected model
#' @param modelset_matrix Matrix composed of zeros and ones
#' @param I_mat Matrix \eqn{X^T V^{-1} X} of the full model
#' @param G_mat Matrix \eqn{X^T V^{-1} Z G Z^T V^{-1} X} of the full model
#' @param Lambda_sqrt Symmetric square root of the covariance matrix \eqn{\Lambda}
#'
#' @return
#' \item{list_constraints}{List with constraints. Each element is a list
#' \code{(A, B, C)} for the constraint \eqn{w^T A w + B^T w + C \ge 0},
#' as required by function \code{rtmg} of package \code{tmg}}
#'
#' @keywords internal
#'

construct_constraints <- function(degcAIC_models,
                                  cAIC_min,
                                  models_compared,
                                  modelset_matrix,
                                  I_mat,
                                  G_mat,
                                  Lambda_sqrt) {
  dim_w = nrow(Lambda_sqrt)
  indices_selected = which(modelset_matrix[cAIC_min, ] != 0)
  Gamma_selected = compute_Gamma(indices_selected, I_mat, G_mat)

  list_constraints = list()

  for (k in seq_along(models_compared)) {
    j = models_compared[k]
    indices_j = which(modelset_matrix[j, ] != 0)
    Gamma_j = compute_Gamma(indices_j, I_mat, G_mat)

    A = Lambda_sqrt %*% (Gamma_selected - Gamma_j) %*% Lambda_sqrt
    A = (A + t(A)) / 2
    B = rep(0, dim_w)
    C = -2 * (degcAIC_models[cAIC_min] - degcAIC_models[j])
    list_constraints[[k]] = list(A, B, C)
  }

  list_constraints
}
