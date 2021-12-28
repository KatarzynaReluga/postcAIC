n = 10
m_i = 5
m_total = 50

clusterID = rep(1:n, m_i)
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


cAIC_min = cAIC_model_set$cAIC_min
degcAIC_models = cAIC_model_set$degcAIC_models

X_full = cAIC_model_set$X_full
X_cluster_full = cAIC_model_set$X_cluster_full

sig_u_full = cAIC_model_set$sig_u_full
sig_e_full = cAIC_model_set$sig_u_full

beta_sel = cAIC_model_set$beta_sel
mu_sel = cAIC_model_set$mu_sel

# Naive CI for mixed and fixed parameters -------------------------------------

sig_u_sel = cAIC_model_set$sig_u_sel
sig_e_sel = cAIC_model_set$sig_e_sel
indices_sel = cAIC_model_set$indices_sel
X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(n))

x_beta_lin_com = cAIC_model_set$X_cluster_full

naive_CI_results  = naive_CI(
  beta_sel,
  mu_sel,
  sig_u_sel,
  sig_e_sel,
  sig_u_full,
  sig_e_full,

  X_full,
  C_cluster_sel,
  clusterID,
  indices_sel,
  type_MSE_mixed = "regular",
  x_beta_lin_com
)

test_that("Output is correct", {
  expect_match(class(naive_CI_results), "list")
  expect_length(naive_CI_results, 8)
})
