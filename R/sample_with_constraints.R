#' Sample with constraints
#'
#' Function \code{sample_with_constraints} provides samples from the standard
#' normal distribution in \eqn{R^{2p}} truncated to the selection region.
#' Constraints are imposed due to the data driven model selection.
#'
#' @inheritParams find_starting_points
#' @param n_samples Number of samples from each Markov chain. The total number
#' of draws is \code{2 * n_starting_points * n_samples} for every sampler.
#' @param burn.in The number of burn-in iterations of each Markov chain.
#' Each chain is run n_samples + burn.in times, and the last n_samples are
#' returned. Not used by rejection sampling.
#' @param sampler \code{"rejection"} (default): rejection sampling, with
#' elliptical slice sampling as a fallback when the acceptance rate is below
#' \code{min_acceptance}. \code{"tmg"}: exact Hamiltonian Monte Carlo of
#' function \code{rtmg} of package \code{tmg}, which must be installed
#' (it is archived on CRAN).
#' @param min_acceptance Smallest acceptance rate of rejection sampling,
#' estimated from a pilot batch of draws, for which rejection sampling is used.
#' Below it, elliptical slice sampling is used. Default: \code{min_acceptance = 0.005}.
#'
#'
#' @details
#' * Rejection sampling draws from the \eqn{N(0, I)} distribution in batches and
#' keeps the draws which satisfy all constraints. The draws are exact and independent.
#' * Elliptical slice sampling (Murray, Adams and MacKay, 2010) is a Markov chain
#' whose stationary distribution is the truncated distribution. Its chains
#' start from accepted draws of the pilot batch and their negatives, completed
#' if necessary by \code{find_starting_points}.
#' * For \code{sampler = "tmg"}, \code{2 * n_starting_points} chains are run, from
#' the starting points obtained with \code{find_starting_points} and their negatives
#' (the selection region is symmetric about zero).
#' * If \code{n_models_to_compare = 0}, there are no constraints and the
#' samples are drawn from the untruncated standard normal distribution.
#'
#' @return List with elements:
#' \item{sample_w}{Matrix with \code{2 * n_starting_points * n_samples} rows,
#' each a draw from the truncated standard normal distribution in \eqn{R^{2p}}}
#' \item{sampler_used}{Sampler actually used: \code{"rejection"},
#' \code{"elliptical slice"}, \code{"tmg"} or \code{"none"} (no constraints)}
#' \item{acceptance_rate}{Acceptance rate of rejection sampling in the pilot
#' batch (\code{NA} for \code{sampler = "tmg"})}
#'
#' @references
#' Murray, I., Adams, R. P. and MacKay, D. J. C. (2010). Elliptical slice
#' sampling. Proceedings of the Thirteenth International Conference on
#' Artificial Intelligence and Statistics, PMLR 9, 541-548.
#'
#' @importFrom stats rnorm runif
#' @export
#'
#'
sample_with_constraints <- function(n_starting_points,
                                    dim_w,
                                    n_models_to_compare,
                                    list_constraints,
                                    n_samples = 10000,
                                    burn.in = 1000,
                                    scale_mvrnorm,
                                    sampler = c("rejection", "tmg"),
                                    min_acceptance = 0.005) {
  sampler = match.arg(sampler)
  n_total = 2 * n_starting_points * n_samples

  # No constraints ------------------------------------------------------------
  if (n_models_to_compare == 0) {
    sample_w = matrix(rnorm(n_total * dim_w), nrow = n_total, ncol = dim_w)
    output = list(sample_w = sample_w,
                  sampler_used = "none",
                  acceptance_rate = 1)
    return(output)
  }

  # Exact Hamiltonian Monte Carlo of package tmg -------------------------------
  if (sampler == "tmg") {
    if (!requireNamespace("tmg", quietly = TRUE)) {
      stop(
        "Package 'tmg' is needed for sampler = 'tmg'. It is archived on CRAN; ",
        "install it with remotes::install_version('tmg', version = '0.3', ",
        "repos = 'http://cran.r-project.org'), or use sampler = 'rejection'."
      )
    }
    starting_points  = find_starting_points(n_starting_points,
                                            dim_w,
                                            n_models_to_compare,
                                            list_constraints,
                                            scale_mvrnorm)
    starting_points = cbind(starting_points, -starting_points)

    # Precision matrix M and linear term r, no linear constraints ------------
    M = diag(dim_w)
    r = rep(0, dim_w)
    f = NULL
    g = NULL

    sample_w_list  = list()
    for (k in 1:(2 * n_starting_points)) {
      sample_w_list[[k]] = tmg::rtmg(n_samples,
                                     M,
                                     r,
                                     starting_points[, k],
                                     f,
                                     g,
                                     list_constraints,
                                     burn.in)
    }
    output = list(sample_w = do.call("rbind", sample_w_list),
                  sampler_used = "tmg",
                  acceptance_rate = NA)
    return(output)
  }

  # Pilot batch of rejection sampling -----------------------------------------
  batch_size = 1e5
  pilot = matrix(rnorm(batch_size * dim_w), nrow = batch_size, ncol = dim_w)
  inside = check_constraints(pilot, list_constraints)
  acceptance_rate = mean(inside)

  if (acceptance_rate >= min_acceptance) {
    # Rejection sampling ------------------------------------------------------
    accepted = list(pilot[inside, , drop = FALSE])
    n_accepted = sum(inside)
    while (n_accepted < n_total) {
      batch = matrix(rnorm(batch_size * dim_w), nrow = batch_size, ncol = dim_w)
      inside_batch = check_constraints(batch, list_constraints)
      accepted[[length(accepted) + 1]] = batch[inside_batch, , drop = FALSE]
      n_accepted = n_accepted + sum(inside_batch)
    }
    sample_w = do.call("rbind", accepted)[1:n_total, , drop = FALSE]
    sampler_used = "rejection"
  } else {
    # Elliptical slice sampling -------------------------------------------------
    pilot_inside = pilot[inside, , drop = FALSE]
    n_from_pilot = min(nrow(pilot_inside), n_starting_points)
    starting_points = t(pilot_inside[seq_len(n_from_pilot), , drop = FALSE])
    if (n_from_pilot < n_starting_points) {
      starting_points = cbind(
        starting_points,
        find_starting_points(
          n_starting_points - n_from_pilot,
          dim_w,
          n_models_to_compare,
          list_constraints,
          scale_mvrnorm
        )
      )
    }
    starting_points = cbind(starting_points, -starting_points)

    sample_w_list = list()
    for (k in 1:(2 * n_starting_points)) {
      sample_w_list[[k]] = sample_elliptical_slice(n_samples,
                                                   starting_points[, k],
                                                   list_constraints,
                                                   burn.in)
    }
    sample_w = do.call("rbind", sample_w_list)
    sampler_used = "elliptical slice"
  }

  output = list(sample_w = sample_w,
                sampler_used = sampler_used,
                acceptance_rate = acceptance_rate)
  output
}


#' Check constraints
#'
#' Function \code{check_constraints} checks which rows of a matrix satisfy all
#' constraints \eqn{w^T A w + B^T w + C > 0}.
#'
#' @param W Matrix, one point per row
#' @param list_constraints List of constraints, each a list \code{(A, B, C)}
#'
#' @return Logical vector, \code{TRUE} for the rows satisfying all constraints
#'
#' @keywords internal
#'

check_constraints <- function(W, list_constraints) {
  inside = rep(TRUE, nrow(W))
  for (k in seq_along(list_constraints)) {
    A = list_constraints[[k]][[1]]
    B = list_constraints[[k]][[2]]
    C = list_constraints[[k]][[3]]
    value = rowSums((W %*% A) * W) + as.vector(W %*% B) + C
    inside = inside & (value > 0)
  }
  inside
}


#' Elliptical slice sampling from a truncated standard normal distribution
#'
#' Function \code{sample_elliptical_slice} runs one Markov chain of elliptical
#' slice sampling (Murray, Adams and MacKay, 2010) for the standard normal
#' distribution truncated to the set where all constraints hold.
#'
#' @param n_samples Number of samples returned
#' @param initial Starting point, which must satisfy all constraints
#' @param list_constraints List of constraints, each a list \code{(A, B, C)}
#' @param burn.in Number of burn-in iterations
#'
#' @return Matrix with \code{n_samples} rows, one draw per row
#'
#' @details
#' From the current point \eqn{w}, draw \eqn{\nu \sim N(0, I)} and an angle
#' \eqn{t} uniformly on \eqn{[0, 2\pi]}. The proposal
#' \eqn{w \cos t + \nu \sin t} is accepted if it satisfies the constraints;
#' otherwise the interval of angles is shrunk towards \eqn{t = 0}, which gives
#' back \eqn{w}, and a new angle is drawn from it.
#'
#' @keywords internal
#'

sample_elliptical_slice <- function(n_samples,
                                    initial,
                                    list_constraints,
                                    burn.in) {
  dim_w = length(initial)
  w = initial
  output = matrix(0, nrow = n_samples, ncol = dim_w)
  max_shrink = 200

  for (iter in 1:(n_samples + burn.in)) {
    nu = rnorm(dim_w)
    angle = runif(1, 0, 2 * pi)
    angle_min = angle - 2 * pi
    angle_max = angle

    for (s in 1:max_shrink) {
      w_proposal = w * cos(angle) + nu * sin(angle)
      if (check_constraints(matrix(w_proposal, nrow = 1), list_constraints)) {
        break
      }
      if (angle < 0) {
        angle_min = angle
      } else {
        angle_max = angle
      }
      angle = runif(1, angle_min, angle_max)
      # The interval has shrunk to (numerically) zero: stay at w
      if (s == max_shrink) {
        w_proposal = w
      }
    }

    w = w_proposal
    if (iter > burn.in) {
      output[iter - burn.in, ] = w
    }
  }

  output
}
