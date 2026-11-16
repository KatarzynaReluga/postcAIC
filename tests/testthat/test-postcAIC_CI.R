# Define basic parameters -------------------------------------------------
n = 15
m_i = 5
m_total = n * m_i

beta = c(2.25, -1.1, 2.43, rep(0, 2))
sig_e = 1
sig_u = 1

X = simulations_n15_mi5
X_intercept = cbind(rep(1, m_total), X)

clusterID = rep(1:n, each = m_i)

# Create responses, errors and random effects  -------------------
e_ij = rnorm(m_total, 0, sqrt(sig_e))

u_i = rnorm(n, 0, sqrt(sig_u))
u_i_aug = rep(u_i, each = m_i)

y = X_intercept%*% beta + u_i_aug + e_ij

cAIC_model_set = compute_cAIC_for_model_set(X, y, clusterID,
                                            model = "NERM",
                                            covariate_selection_matrix = NULL,
                                            modelset  = "part_subset",
                                            common = c(1:2),
                                            intercept = FALSE)


cAIC_min = cAIC_model_set$cAIC_min
degcAIC_models = cAIC_model_set$degcAIC_models

X_full = cAIC_model_set$X_full
X_cluster_full = cAIC_model_set$X_cluster_full

sig_u_full = cAIC_model_set$sig_u_full
sig_e_full = cAIC_model_set$sig_e_full

beta_sel = cAIC_model_set$beta_sel
mu_sel = cAIC_model_set$mu_sel

modelset_matrix = cAIC_model_set$modelset_matrix
cAIC_models = cAIC_model_set$cAIC_models
x_beta_lin_com = cAIC_model_set$X_cluster_full


# Post-cAIC CI for mixed and fixed parameters -------------------------------------
# (few draws, to keep the tests fast)
postcAIC_CI_results = postcAIC_CI(cAIC_min,
                                  degcAIC_models,

                                  X_full,
                                  X_cluster_full,
                                  sig_u_full,
                                  sig_e_full,
                                  model = "NERM",
                                  clusterID,

                                  beta_sel,
                                  mu_sel,

                                  modelset_matrix,
                                  x_beta_lin_com = x_beta_lin_com,
                                  n_starting_points = 2,
                                  cAIC_models = cAIC_models,
                                  theta = "full",
                                  n_samples = 1000,
                                  burn.in = 100)


plot(postcAIC_CI_results, order_estimates = sample(n, replace = T))

test_that("Output is correct", {
  expect_match(class(postcAIC_CI_results), "postcAIC_CI")
  expect_length(postcAIC_CI_results, 10)
  expect_equal(postcAIC_CI_results$sampler_used %in%
                c("rejection", "elliptical slice", "none"), TRUE)
  expect_match(class(postcAIC_CI_results$beta_postcAIC_CI_up), "numeric")
  expect_match(class(postcAIC_CI_results$beta_postcAIC_CI_do), "numeric")
  expect_match(class(postcAIC_CI_results$mixed_postcAIC_CI_up), "numeric")
  expect_match(class(postcAIC_CI_results$mixed_postcAIC_CI_do), "numeric")
  expect_length(postcAIC_CI_results$mixed_postcAIC_CI_up, n)
  expect_length(postcAIC_CI_results$beta_x_postcAIC_CI_up, n)
  expect_true(all(postcAIC_CI_results$beta_postcAIC_CI_do <=
                    postcAIC_CI_results$beta_postcAIC_CI_up))
  expect_true(all(postcAIC_CI_results$mixed_postcAIC_CI_do <
                    postcAIC_CI_results$mixed_postcAIC_CI_up))
  expect_equal(postcAIC_CI_results$c_n, 2 * log(n))
})

test_that("Options var_psi and vcov work", {
  results_options = postcAIC_CI(cAIC_min,
                                degcAIC_models,
                                X_full,
                                X_cluster_full,
                                sig_u_full,
                                sig_e_full,
                                model = "NERM",
                                clusterID,
                                beta_sel,
                                mu_sel,
                                modelset_matrix,
                                n_starting_points = 2,
                                cAIC_models = cAIC_models,
                                theta = "full",
                                y = y,
                                var_psi = "g1+2g3",
                                vcov = "robust",
                                n_samples = 1000,
                                burn.in = 100)
  expect_match(class(results_options), "postcAIC_CI")
  expect_length(results_options$mixed_postcAIC_CI_up, n)
  expect_error(postcAIC_CI(cAIC_min, degcAIC_models, X_full, X_cluster_full,
                           sig_u_full, sig_e_full, model = "NERM", clusterID,
                           beta_sel, mu_sel, modelset_matrix,
                           cAIC_models = cAIC_models, theta = "full",
                           vcov = "robust"))
})

test_that("Selection with refitted variance parameters gives an error", {
  expect_error(postcAIC_CI(cAIC_min, degcAIC_models, X_full, X_cluster_full,
                           sig_u_full, sig_e_full, model = "NERM", clusterID,
                           beta_sel, mu_sel, modelset_matrix,
                           cAIC_models = cAIC_models, theta = "refit"))
})

test_that("Missing cAIC_models gives an error", {
  expect_error(postcAIC_CI(cAIC_min, degcAIC_models, X_full, X_cluster_full,
                           sig_u_full, sig_e_full, model = "NERM", clusterID,
                           beta_sel, mu_sel, modelset_matrix))
})

test_that("Full model selected: no constraints, intervals for all mixed effects", {
  # Only the full model in the model set
  cAIC_full = compute_cAIC_for_model_set(X, y, clusterID,
                                         model = "NERM",
                                         covariate_selection_matrix = matrix(1, 1, 5),
                                         modelset  = NULL,
                                         intercept = FALSE)
  results_full = postcAIC_CI(cAIC_full$cAIC_min,
                             cAIC_full$degcAIC_models,
                             cAIC_full$X_full,
                             cAIC_full$X_cluster_full,
                             cAIC_full$sig_u_full,
                             cAIC_full$sig_e_full,
                             model = "NERM",
                             clusterID,
                             cAIC_full$beta_sel,
                             cAIC_full$mu_sel,
                             cAIC_full$modelset_matrix,
                             n_starting_points = 2,
                             cAIC_models = cAIC_full$cAIC_models,
                             theta = cAIC_full$theta,
                             n_samples = 1000,
                             burn.in = 100)
  expect_length(results_full$models_compared, 0)
  expect_length(results_full$mixed_postcAIC_CI_up, n)
  expect_true(all(results_full$mixed_postcAIC_CI_do <
                    results_full$mixed_postcAIC_CI_up))
})
