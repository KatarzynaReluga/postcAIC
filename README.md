# Post-cAIC selection inference under Linear Mixed Models

Author: Katarzyna Reluga

## Overview

postcAIC implements post-cAIC confidence intervals for mixed and fixed parameters under linear mixed models. Reference: Gerda Claeskens, Katarzyna Reluga, and Stefan Sperlich (2021). *Post-selection inference for linear mixed model parameters using the conditional Akaike information criterion*. Available at https://arxiv.org/abs/2109.10975

## Installation

You can install the most recent version of postcAIC from GitHub with:

``` r
install.pacakges("devtools")
library(devtools)

install_version("tmg", version = "0.3", repos = "http://cran.r-project.org")
install_github("KatarzynaReluga/postcAIC")
library(postcAIC)
```

## Simulation Study
All simulation studies from Gerda Claeskens, Katarzyna Reluga, and Stefan Sperlich (2021) can be reprodcued using codes in folder 
[simulations](https://github.com/KatarzynaReluga/postcAIC/tree/main/simulations). 

## Data example

``` r
y = postcAIC_nhaens$log_BMI

X = data.frame(select(postcAIC_nhaens, -c("clusterID", "log_BMI")))

clusterID = postcAIC_nhaens$clusterID

cAIC_model_set = compute_cAIC_for_model_set(X, y, clusterID,
                                            model = "NERM",
                                            covariate_selection_matrix =                                              NULL,
                                            modelset  = "part_subset",
                                            common = c(1:2),
                                            intercept = FALSE)

cAIC_min = cAIC_model_set$cAIC_min 
degcAIC_models = cAIC_model_set$degcAIC_models

Z = cAIC_model_set$Z
X_full = cAIC_model_set$X_full
X_cluster_full = cAIC_model_set$X_cluster_full

G_full = cAIC_model_set$G_full
R_full = cAIC_model_set$R_full
V_full = cAIC_model_set$V_full

beta_sel = cAIC_model_set$beta_sel
mu_sel = cAIC_model_set$mu_sel

modelset_matrix = cAIC_model_set$modelset_matrix
x_beta_lin_com = cAIC_model_set$X_cluster_full



# Post-cAIC CI for mixed and fixed parameters -------------------------------------
postcAIC_CI_results = postcAIC_CI(cAIC_min, degcAIC_models,
                                  Z, X_full, X_cluster_full,
                                  G_full, R_full, V_full,
                                  
                                  beta_sel, mu_sel,
                                  
                                  modelset  = "part_subset",
                                  common = c(1:2), 
                                  modelset_matrix, x_beta_lin_com,
                                  n_starting_points = 5, 
                                  scale_mvrnorm = 10)

# Naive CI for mixed and fixed parameters -------------------------------------

sig_u_sel = cAIC_model_set$sig_u_sel
sig_e_sel = cAIC_model_set$sig_e_sel
indices_sel = cAIC_model_set$indices_sel
X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(nlevels(clusterID)))

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
                                  boot = 200)

```



