
<!-- README.md is generated from README.Rmd. Please edit that file -->

# postcAIC

<!-- badges: start -->
<!-- badges: end -->

## Overview

Package **postcAIC** implements post-cAIC confidence intervals (CI) for
mixed and fixed parameters under linear mixed models. Post-cAIC CI
accounts for the data-driven model selection using conditional Akaike
information criterion (cAIC).

Reference: Claeskens, Reluga, and Sperlich (2021). *Post-selection
inference for linear mixed model parameters using the conditional Akaike
information criterion*.

Available at [arXiv.org](https://arxiv.org/abs/2109.10975)

In what follows, we use abbreviaiton CRS-2021 to refer to Claeskens,
Reluga, and Sperlich (2021).

## Installation

You can install the most recent version of postcAIC from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")

remotes::install_version("tmg", version = "0.3", repos = "http://cran.r-project.org")
# devtools::install_github("KatarzynaReluga/postcAIC")

devtools::install_github("KatarzynaReluga/postcAIC",   
                         ref="main",
                         auth_token =
          "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")

```

## Example

This is a basic example which shows you how to construct post-cAIC
confidence intervals for fixed and mixed parameters under nested-error
regression model (NERM).

First we generate outcomes, covariates and cluster labels.

``` r
library(postcAIC)

# Set seed ----------------------------------------------------
set.seed(10)

# Set the number of clusters and units in each cluster --------
n = 15
m_i = 5
m_total = n * m_i

# Set fixed (regression) parameters and variance parameters ---
beta = c(2.25, -1.1, 2.43, rep(0, 2))
sig_e = 1
sig_u = 1

# Load matrix X from data in package postcAIC -----------------
X = simulations_n15_mi5
X_intercept = cbind(rep(1, m_total), X)

# Create a factor vector with cluster labels ------------------
clusterID = rep(1:n, each = m_i)

# Generate stochastic errors, random effects and outcomes -----
e_ij = rnorm(m_total, 0, sig_e)
u_i = rnorm(n, 0, sig_u)
u_i_aug = rep(u_i, each = m_i)
y = X_intercept %*% beta + u_i_aug + e_ij
```

Second, we compute cAIC for each model in the considered model set,
construct post-cAIC CI for fixed and mixed effects and plot the latter.

``` r
# Compute cAIC for models from the set of models -------------
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

# Construct post-cAIC CI for fixed and mixed parameters and ---
postcAIC_CI_results = postcAIC_CI(
  cAIC_min = cAIC_model_set$cAIC_min,
  degcAIC_models =  cAIC_model_set$degcAIC_models,

  X_full = cAIC_model_set$X_full,
  X_cluster_full = cAIC_model_set$X_cluster_full,
  sig_u_full = cAIC_model_set$sig_u_full,
  sig_e_full = cAIC_model_set$sig_e_full,
  model = "NERM",
  clusterID,

  beta_sel = cAIC_model_set$beta_sel,
  mu_sel = cAIC_model_set$mu_sel,

  modelset_matrix = cAIC_model_set$modelset_matrix)

plot(postcAIC_CI_results, y_axis_lim = c(-0.5, 6.5))
```

<img src="man/figures/README-example3-1.png" width="100%" />

To compare the performance of post-cAIC CI, we construct naive CI which
do not take the model selection step into account. In addition, we
construct post-OBSP CI of Sugasawa, S., Kawakubo, Y. and Datta, G. S.
(2019) for mixed parameters and plot them together.

``` r
# Construct naive CI for mixed and fixed parameters -------------------
indices_sel = cAIC_model_set$indices_sel
X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(n))

naive_CI_results  = naive_CI(
  beta_sel = cAIC_model_set$beta_sel,
  mu_sel = cAIC_model_set$mu_sel,
  sig_u_sel = cAIC_model_set$sig_u_sel,
  sig_e_sel = cAIC_model_set$sig_e_sel,
  sig_u_full = cAIC_model_set$sig_u_full,
  sig_e_full = cAIC_model_set$sig_e_full,

  X_full = cAIC_model_set$X_full,
  C_cluster_sel,
  clusterID,
  indices_sel,
  type_MSE_mixed = "corrected",
  x_beta_lin_com = cAIC_model_set$X_cluster_full
)

# Construct post-OBSP CI for mixed parameters ---------------
postOBSP_CI_results = postOBSP_CI(
  X,
  y,
  clusterID,
  X_cluster_full = cAIC_model_set$X_cluster_full,
  model = "NERM",
  covariate_selection_matrix = NULL,
  modelset  = "part_subset",
  intercept = FALSE,
  common = c(1:2),
  boot = 500
)

results_to_plot = format_results(x = naive_CI_results,
                                 y = postcAIC_CI_results,
                                 z = postOBSP_CI_results,
                                 type = "corrected")

plot(results_to_plot, type = "corrected")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Simulation study from CRS-2021

All
[simulations](https://github.com/KatarzynaReluga/postcAIC/tree/main/simulations)
in Claeskens, Reluga, and Sperlich (2021) can be reproduced using
functions in package **postcAIC**.

## Data illustration from CRS-2021

Claeskens, Reluga, and Sperlich (2021) illustrated the performance of
post-cAIC CI for mixed and fixed effects using a subset of
**[NHAENS](https://cran.r-project.org/web/packages/NHANES/index.html)**
R-package. The
[subset](https://github.com/KatarzynaReluga/postcAIC/tree/main/data) and
the
[code](https://github.com/KatarzynaReluga/postcAIC/blob/main/data-raw/postcAIC_nhaens.R)
to construct it can be found in pacakge **postcAIC** together with the
[code](https://github.com/KatarzynaReluga/postcAIC/blob/main/data_example/data_example.R) to
analyse the results.
