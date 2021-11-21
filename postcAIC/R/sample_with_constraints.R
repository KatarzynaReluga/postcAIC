#' Sample with constraints
#'
#' Function \code{sample_with_constraints} provides samples from constrained normal
#' distribution. Constraints are consequences of model selection.
#'
#'
#' @inheritParams find_starting_points
#' @param n Number of clusters (random effects)
#' @param n_samples Number of samples from constrained distribution
#' @param burn.in The number of burn-in iterations. The Markov chain
#' is sampled n_samples + burn.in times, and the last n samples are returned.
#'
#'
#' @details
#' * \code{n_starting_points}  - in practice we use 2 times n_starting_points, becasue
#' we sample using initial values and negative inital values obtained applying function
#' * \code{burn.in} -  parameter of function rtmg
#'
#'
#' @importFrom tmg rtmg
#' @export
#'
#'
sample_with_constraints <- function(n_starting_points,
                                    p, n,
                                    n_models_to_compare,
                                    list_constraints,
                                    n_samples = 10000,
                                      burn.in = 1000, scale_mvrnorm) {

  starting_points  = find_starting_points(n_starting_points,
                                          p,
                                          n_models_to_compare,
                                          list_constraints,
                                          scale_mvrnorm)
  starting_points = cbind(starting_points, -starting_points)

  #Define the precision matrix and a linear term (for rtmg)
  M = diag(p)
  r = rep(0, p)

  # Set initial point for the Markov chain
  f = NULL
  g = NULL

  sample_fixed_list  = list()
  for (k in 1: (2 * n_starting_points)) {
    sample_fixed_list[[k]] = rtmg(n_samples, M, r, starting_points[, k], f,
                                  g, list_constraints, burn.in)
  }
  sample_fixed_full = do.call("rbind", sample_fixed_list)
  sample_random = rmvn(2 *n_starting_points * n_samples,
                        mu = rep(0, n), V = diag(n))

  sample_mix_full = cbind(sample_fixed_full, sample_random)

  output = list(sample_fixed_full = sample_fixed_full,
                sample_random = sample_random,
                sample_mix_full = sample_mix_full)
  output

}
