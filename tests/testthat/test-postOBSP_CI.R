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

# Post-OBSP inference ----------------------------------------
postOBSP_CI_results = postOBSP_CI(X, y,
                                  clusterID,
                                  X_cluster_full = NULL,
                                  model = "NERM",
                                  covariate_selection_matrix = NULL,
                                  modelset  = "part_subset",
                                  intercept = FALSE,
                                  common = c(1:2),
                                  boot = 1000)

test_that("Output is correct", {
  expect_match(class(postOBSP_CI_results), "list")
  expect_length(postOBSP_CI_results, 5)
  expect_match(class(postOBSP_CI_results$postOBSP_up), "numeric")
  expect_match(class(postOBSP_CI_results$postOBSP_do), "numeric")
  expect_match(class(postOBSP_CI_results$mu_hat_sel), "numeric")
  expect_match(class(postOBSP_CI_results$OBSP_models), "numeric")
})

