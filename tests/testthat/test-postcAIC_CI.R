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
e_ij = rnorm(m_total, 0, sig_e)

u_i = rnorm(n, 0, sig_u)
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
x_beta_lin_com = cAIC_model_set$X_cluster_full


# Post-cAIC CI for mixed and fixed parameters -------------------------------------
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
                                  x_beta_lin_com = NULL)


plot(postcAIC_CI_results, order_estimates = sample(n, replace = T))

test_that("Output is correct", {
  expect_match(class(postcAIC_CI_results), "postcAIC_CI")
  expect_length(postcAIC_CI_results, 6)
  expect_match(class(postcAIC_CI_results$beta_postcAIC_CI_up), "numeric")
  expect_match(class(postcAIC_CI_results$beta_postcAIC_CI_do), "numeric")
  expect_match(class(postcAIC_CI_results$mixed_postcAIC_CI_up), "numeric")
  expect_match(class(postcAIC_CI_results$mixed_postcAIC_CI_do), "numeric")
})

