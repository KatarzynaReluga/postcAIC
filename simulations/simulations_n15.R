###############################################
# Author: Katarzyna Reluga                    #
# The computations were performed at          #
# University of Geneva on the Baobab cluster. #
###############################################
######################
# Simulation setting:  n = 15, m_i = 5# #
######################
#rm(list=ls())
#
# Set seed ------------------------------------------------------------
#
#
library(postcAIC)
set.seed(10)
#
# Define the number of clusters and units in each cluster -------------
#
n = 100
m_i = 50
m_total = n * m_i

# Define beta and sigmas ----------------------------------------------

beta = c(2.25, -1.1, 2.43, rep(0, 2))
sig_e = 1
sig_u = 1

# Load appropriate matrix X from data ----------------------------------
#X = simulations_n15_mi5
Omega = matrix(0.25, 4, 4)
diag(Omega) <- 1
X = rmvn(n * m_i, mu  = rep(0, 4), V = Omega)
# Add intercept -----------------------------------------------------
X_intercept = cbind(rep(1, m_total), X)


# Create (and validate) a factor vector with clusters ------------

clusterID = rep(1:n, each = m_i)
#clusterID = validate_observations(clusterID, X, cluster = TRUE)


# Create responses, errors and random effects  -------------------
e_ij = rnorm(m_total, 0, sig_e)

u_i = rnorm(n, 0, sig_u)
u_i_aug = rep(u_i, each = m_i)

y = X_intercept%*% beta + u_i_aug + e_ij

#
# Compute cAIC for models from the set of models
#
a = Sys.time()
cAIC_model_set = compute_cAIC_for_model_set(X, y, clusterID,
                           model = "NERM",
                           covariate_selection_matrix = NULL,
                           modelset  = "all_subset",
                           intercept = FALSE)


b = Sys.time()
b-a

cAIC_min = cAIC_model_set$cAIC_min
degcAIC_models = cAIC_model_set$degcAIC_models

X_full = cAIC_model_set$X_full
X_cluster_full = cAIC_model_set$X_cluster_full

sig_u_full = cAIC_model_set$sig_u_full
sig_e_full = cAIC_model_set$sig_u_full

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

                                  modelset_matrix)

# Naive CI for mixed and fixed parameters -------------------------------------

sig_u_sel = cAIC_model_set$sig_u_sel
sig_e_sel = cAIC_model_set$sig_e_sel
indices_sel = cAIC_model_set$indices_sel
X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(n))

R_sel = cAIC_model_set$R_sel
V_sel = cAIC_model_set$V_sel
G_sel = cAIC_model_set$G_sel

naive_CI_results = naive_CI(beta_sel, mu_sel,
                            G_sel, R_sel, V_full,
                            sig_u_sel, sig_e_sel,
                            X_full, Z, C_cluster_sel = C_cluster_sel,
                            clusterID, indices_sel = indices_sel,
                            type_MSE_mixed = "corrected",
                            x_beta_lin_com)

# Post-OBSP CI for mixed parameters -------------------------------------
sig_u_full = cAIC_model_set$sig_u_full
sig_e_full = cAIC_model_set$sig_e_full
postOBSP_CI_results = postOBSP_CI(X_full, y, clusterID, Z, X_cluster_full,
                                  G_full, V_full, R_full,
                                  sig_u_full, sig_e_full,
                                  modelset_matrix,
                                  boot = 1000)


