# Region {w in R^2 : w_1^2 > 4}: two symmetric pieces, probability 2 * pnorm(-2)
list_constraints = list(list(diag(c(1, 0)), c(0, 0), -4))

# Moments of the truncated distribution
prob_positive = 0.5
mean_w1_squared = 1 + 2 * dnorm(2) / pnorm(-2)

check_sample = function(sample_w) {
  expect_equal(nrow(sample_w), 2 * 2 * 5000)
  expect_true(all(sample_w[, 1] ^ 2 > 4))
  expect_equal(mean(sample_w[, 1] > 0), prob_positive, tolerance = 0.05)
  expect_equal(mean(sample_w[, 1] ^ 2), mean_w1_squared, tolerance = 0.05)
  expect_equal(var(sample_w[, 2]), 1, tolerance = 0.05)
}

test_that("Rejection sampling targets the truncated normal distribution", {
  results = sample_with_constraints(n_starting_points = 2,
                                    dim_w = 2,
                                    n_models_to_compare = 1,
                                    list_constraints = list_constraints,
                                    n_samples = 5000,
                                    burn.in = 100,
                                    scale_mvrnorm = 10,
                                    sampler = "rejection",
                                    min_acceptance = 0.01)
  expect_equal(results$sampler_used, "rejection")
  check_sample(results$sample_w)
})

test_that("Elliptical slice sampling targets the truncated normal distribution", {
  # min_acceptance above the acceptance rate (about 0.046) forces the fallback
  results = sample_with_constraints(n_starting_points = 2,
                                    dim_w = 2,
                                    n_models_to_compare = 1,
                                    list_constraints = list_constraints,
                                    n_samples = 5000,
                                    burn.in = 100,
                                    scale_mvrnorm = 10,
                                    sampler = "rejection",
                                    min_acceptance = 0.5)
  expect_equal(results$sampler_used, "elliptical slice")
  check_sample(results$sample_w)
})

test_that("tmg targets the truncated normal distribution", {
  skip_if_not_installed("tmg")
  results = sample_with_constraints(n_starting_points = 2,
                                    dim_w = 2,
                                    n_models_to_compare = 1,
                                    list_constraints = list_constraints,
                                    n_samples = 5000,
                                    burn.in = 100,
                                    scale_mvrnorm = 10,
                                    sampler = "tmg")
  expect_equal(results$sampler_used, "tmg")
  check_sample(results$sample_w)
})

test_that("No constraints gives untruncated draws", {
  results = sample_with_constraints(n_starting_points = 2,
                                    dim_w = 2,
                                    n_models_to_compare = 0,
                                    list_constraints = list(),
                                    n_samples = 5000,
                                    scale_mvrnorm = 10)
  expect_equal(results$sampler_used, "none")
  expect_equal(nrow(results$sample_w), 2 * 2 * 5000)
})
