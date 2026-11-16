n = 10
m_i = 5
m_total = 50

clusterID = rep(1:n, each = m_i)
p = 10
beta = rep(2, p)
u_i = rnorm(n, 0, 2)
u_i_aug = rep(u_i, each = m_i)
X = matrix(rnorm(m_total * p), m_total, p)
y = X%*%beta + u_i_aug + rnorm(m_total, 0, 1)

cAIC_model_set = compute_cAIC_for_model_set(X, y, clusterID,
                                            model = "NERM",
                                            covariate_selection_matrix = NULL,
                                            modelset  = "part_subset",
                                            common = c(1:8),
                                            intercept = FALSE)

cAIC_model_set_refit = compute_cAIC_for_model_set(X, y, clusterID,
                                                  model = "NERM",
                                                  covariate_selection_matrix = NULL,
                                                  modelset  = "part_subset",
                                                  common = c(1:8),
                                                  intercept = FALSE,
                                                  theta = "refit")

test_that("Output is correct", {
  expect_match(class(cAIC_model_set), "list")
  expect_length(cAIC_model_set, 14)
  expect_equal(cAIC_model_set$theta, "full")
  expect_equal(cAIC_model_set$sig_u_sel, cAIC_model_set$sig_u_full)
  expect_equal(cAIC_model_set$sig_e_sel, cAIC_model_set$sig_e_full)
  expect_length(cAIC_model_set_refit, 14)
  expect_equal(cAIC_model_set_refit$theta, "refit")
})

test_that("Fit at the full-model variance parameters reproduces lme for the full model", {
  X_full = cAIC_model_set$X_full
  fit_lme = estimate_NERM(X_full, y, clusterID)
  fit_theta = fit_NERM_given_theta(X_full, y, clusterID,
                                   sig_u = fit_lme$sig_u,
                                   sig_e = fit_lme$sig_e)
  expect_equal(unname(fit_theta$beta), unname(fit_lme$beta), tolerance = 1e-6)
  expect_equal(unname(fit_theta$u), unname(fit_lme$u), tolerance = 1e-6)
  expect_equal(unname(c(fit_theta$mu)), unname(c(fit_lme$mu)), tolerance = 1e-6)
})
