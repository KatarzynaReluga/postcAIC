library(dplyr)

# Define outcome, covariates (features) and cluster variable ---------------
y = postcAIC_nhaens$log_BMI

X = data.frame(select(postcAIC_nhaens,-c("clusterID", "log_BMI")))

clusterID = postcAIC_nhaens$clusterID
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
  x_beta_lin_com = NULL,
  scale_mvrnorm = 2,
  n_starting_points = 10
)

plot(postcAIC_CI_results, y_axis_lim = c(3.10, 3.65))
# Naive CI for mixed and fixed parameters -------------------------------------
sig_u_sel = cAIC_model_set$sig_u_sel
sig_e_sel = cAIC_model_set$sig_e_sel
indices_sel = cAIC_model_set$indices_sel
X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(nrow(X_cluster_full)))

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
  type_MSE_mixed = "corrected",
  x_beta_lin_com
)
plot(naive_CI_results, type = "corrected", y_axis_lim = c(3.10, 3.65))
# Post-OBSP CI for mixed parameters -------------------------------------

## The running time of postOBSP_CI is long here due to the bootstrap
## estimation of MSE which involves inverting big matrices 
## boot = 1000 number of times. OBSP is a computer intensive 
## method in this case.  

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
                                 type = "corrected")

plot(results_to_plot, type = "corrected", y_axis_lim = c(3.10, 3.65))
