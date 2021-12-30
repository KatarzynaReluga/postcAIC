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

fit_nerm <- estimate_NERM(X = X, y = y,
                          clusterID = clusterID,
                          X_cluster = NULL)
invK <- estimate_invK(X, sig_u = fit_nerm$sig_u,
                     sig_e = fit_nerm$sig_e,
                     clusterID = clusterID)


test_that("Output is correct", {
  expect_equal(is.matrix(invK), TRUE)
  expect_match(class(invK)[1], "matrix")
  expect_equal(nrow(invK), ncol(invK))
  expect_equal(nrow(invK), p + n)
})
