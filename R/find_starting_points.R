#' Find starting points
#'
#' Function \code{find_starting_points} finds starting points
#' to sample from constrained normal distribution
#'
#' @param n_starting_points Number of initial starting points for sampling from truncated distribution
#' @param p Number of all fixed parameters under consideration (intercept included)
#' @param n_models_to_compare Number of models in the model set to compare with
#' @param list_constraints List witch describes the constraints imposed on the normal distribution
#' @param scale_mvrnorm Scale parameter for multivariate normal distribution to sample
#'
#' @importFrom mgcv rmvn
#'
#' @export


#Check of negative values
find_starting_points <- function(n_starting_points,
                                 p,
                                 n_models_to_compare,
                                 list_constraints,
                                 scale_mvrnorm = 10) {

  starting_points = matrix(0, nrow = p,
                           ncol = n_starting_points)

  for (k in 1:n_starting_points) {
    temp_initial = NULL
    loop = 1
    check_sum0 = n_models_to_compare
    check_negative_elements = numeric(n_models_to_compare)

    while (check_sum0 != 0) {
      temp_initial = rmvn(1, mu = rep(0, p), V = scale_mvrnorm * diag(p))
      for (i in 1:n_models_to_compare)      {
        list_constraints_k = list_constraints[[i]]

        temp = temp_initial %*% list_constraints_k[[1]] %*% temp_initial +
          list_constraints_k[[2]] %*% temp_initial + list_constraints_k[[3]]

        check_negative_elements[i] = temp
      }
      check_sum0 = sum(check_negative_elements < 0)
             loop=loop+1
      #        print(loop)
    }
    starting_points[, k] = temp_initial
  }

  starting_points

}
