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
fit <- estimate_NERM(X, y, clusterID)

IC <- compute_information_criteria(X = X, y = y,
                                   clusterID = clusterID,
                                   model = "NERM",
                                   sig_u = fit$sig_u,
                                   sig_e = fit$sig_e,
                                   fit_model_fixed = fit$fit_model_fixed,
                                   fit_model_mixed = fit$fit_model_mixed)

test_that("Output is correct", {
  expect_match(class(IC), "list")
  expect_length(IC, 4)
  expect_match(class(IC$mAIC), "numeric")
  expect_match(class(IC$mBIC), "numeric")
  expect_match(class(IC$cAIC), "numeric")
  expect_match(class(IC$deg_cAIC), "numeric")
})
