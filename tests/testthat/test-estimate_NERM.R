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

fit_nerm <- estimate_NERM(X = X, y = y, clusterID = clusterID, X_cluster = NULL)

test_that("Output is correct", {
  expect_match(class(fit_nerm), "list")
  expect_length(fit_nerm, 7)
  expect_match(class(fit_nerm$fit_model_fixed), "numeric")
  expect_match(class(fit_nerm$fit_model_mixed), "numeric")
  expect_match(class(fit_nerm$sig_u), "numeric")
  expect_length(fit_nerm$sig_u, 1)
  expect_match(class(fit_nerm$sig_e), "numeric")
  expect_length(fit_nerm$sig_e, 1)
  expect_match(class(fit_nerm$beta), "numeric")
  expect_match(class(fit_nerm$u), "numeric")
  expect_match(class(fit_nerm$mu)[1], "matrix")
})
