#' Find starting points
#'
#' Function \code{find_starting_points} finds starting points which satisfy
#' the constraints defining the selection region. The starting points are then
#' used by the Markov chain samplers (\code{tmg} or elliptical slice sampling)
#' of the truncated normal distribution
#'
#' @param n_starting_points Number of initial starting points
#' for sampling from a truncated normal distribution
#' @param dim_w Dimension of the vector to sample, \eqn{2p}, where \eqn{p} is the
#' number of fixed parameters in the full model (intercept included)
#' @param n_models_to_compare Number of models the selected model is compared
#' with (models in the estimated set of overparametrized models, minus the
#' selected model)
#' @param list_constraints List which describes the constraints imposed on
#' the normal distribution
#' @param scale_mvrnorm Variance of the normal distribution from which
#' candidate starting points are drawn. Default: \code{scale_mvrnorm = 10}.
#' @param max_draws Maximal number of candidate draws for each starting point.
#' Default: \code{max_draws = 1e6}.
#'
#' @return
#' \item{starting_points}{Matrix with \code{n_starting_points} columns, each
#' a starting point}
#'
#' @details
#' Candidate points are drawn from the \eqn{N(0, scale_mvrnorm I)} distribution
#' until one satisfies all constraints strictly. The running time of function
#' \code{find_starting_points} is sensitive to the choice of scaling factor
#' \code{scale_mvrnorm}. We suggest choosing a larger/smaller value if the
#' running time is longer than 1 minute.
#'
#' @importFrom stats rnorm
#'
#' @export
#'

find_starting_points <- function(n_starting_points,
                                 dim_w,
                                 n_models_to_compare,
                                 list_constraints,
                                 scale_mvrnorm = 10,
                                 max_draws = 1e6) {
  starting_points = matrix(0, nrow = dim_w,
                           ncol = n_starting_points)

  for (k in 1:n_starting_points) {
    found = FALSE
    n_draws = 0

    while (!found) {
      n_draws = n_draws + 1
      if (n_draws > max_draws) {
        stop(
          "No point satisfying the constraints was found after 'max_draws' draws. ",
          "Try another value of 'scale_mvrnorm'."
        )
      }
      temp_initial = rnorm(dim_w, mean = 0, sd = sqrt(scale_mvrnorm))

      check_constraints = numeric(n_models_to_compare)
      for (i in 1:n_models_to_compare) {
        list_constraints_i = list_constraints[[i]]
        check_constraints[i] = sum(temp_initial *
                                     (list_constraints_i[[1]] %*% temp_initial)) +
          sum(list_constraints_i[[2]] * temp_initial) + list_constraints_i[[3]]
      }
      found = all(check_constraints > 0)
    }
    starting_points[, k] = temp_initial
  }

  starting_points
}
