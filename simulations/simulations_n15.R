###############################################
# Author: Katarzyna Reluga                    #
# The computations were performed at the      #
# University of Geneva on the Baobab cluster. #
###############################################
######################################
# Simulation setting: n = 15, m_i = 5#
######################################
#rm(list=ls())

# Set seed ------------------------------------------------------------

set.seed(10)

# Define the number of clusters and units in each cluster -------------

n = 15
m_i = 5
m_total = n * m_i

# Define beta and sigmas ----------------------------------------------

beta = c(2.25, -1.1, 2.43, rep(0, 2))
sig_e = 1
sig_u = 1

# Load appropriate matrix X from data ----------------------------------
X = simulations_n15_mi5
X_intercept = cbind(rep(1, m_total), X)

# Create (and validate) a factor vector with clusters ------------

clusterID = rep(1:n, each = m_i)

# Create responses, errors and random effects  -------------------
e_ij = rnorm(m_total, 0, sig_e)

u_i = rnorm(n, 0, sig_u)
u_i_aug = rep(u_i, each = m_i)

y = X_intercept %*% beta + u_i_aug + e_ij

# Compute cAIC for models from the set of models -----------------------

cAIC_model_set = compute_cAIC_for_model_set(
  X,
  y,
  clusterID,
  model = "NERM",
  covariate_selection_matrix = NULL,
  modelset  = "part_subset",
  common = c(1:2),
  intercept = FALSE
)
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

postcAIC_CI_results = postcAIC_CI(
  cAIC_min,
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
  x_beta_lin_com = NULL
)

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
  type_MSE_mixed = "both",
  x_beta_lin_com
)

# Post-OBSP CI for mixed parameters -------------------------------------
postOBSP_CI_results = postOBSP_CI(
  X,
  y,
  clusterID,
  X_cluster_full,
  model = "NERM",
  covariate_selection_matrix = NULL,
  modelset  = "part_subset",
  intercept = FALSE,
  common = c(1:2),
  boot = 1000
)

results_to_plot = format_results(x = naive_CI_results,
                                 y = postcAIC_CI_results,
                                 z = postOBSP_CI_results,
                                 type = "both")

plot(results_to_plot, type = "both")
class(results_to_plot)
