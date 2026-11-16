######################################################################
# Simulations of Section 5: one replicate per array task.            #
#                                                                    #
# Usage (from an sbatch array job):                                  #
#   Rscript sim_postcAIC.r <n> <m_i> <setting>                       #
#   e.g. Rscript sim_postcAIC.r 90 5 2                               #
# n: number of clusters; m_i: units per cluster; setting: 1 or 2     #
#   S1: (sig2_e, sig2_u) = (1, 1);  S2: (sig2_e, sig2_u) = (1, 0.5)  #
#                                                                    #
# Each task screens its own block of seeds until cAIC (computed at   #
# the full-model variance estimate) selects the full model, then     #
# computes the intervals of all methods for that replicate:          #
#   PC        post-cAIC (postcAIC_CI), model-based Lambda, psi g1  #
#   PC-g3     post-cAIC, model-based Lambda, psi g1 + 2 g3         #
#   PC-rob    post-cAIC, robust Lambda, psi g1                     #
#   PC-rob-g3 post-cAIC, robust Lambda, psi g1 + 2 g3              #
#   N1, N2    naive (naive_CI), first- and second-order MSE for    #
#             mu_i (beta and linear combinations stored under N1)  #
#   OBSP      Sugasawa et al. (2019), postOBSP_CI, mu_i only       #
#   RBG       Ruegamer, Baumann and Greven (2022), selfmade::mocasin #
# and saves them, with the true values, in                           #
#   results/S<setting>_n<n>_m<m_i>/task_<id>.RData                   #
######################################################################

library(postcAIC)
library(selfmade)
library(lme4)

# Scenario ---------------------------------------------------------------
args = commandArgs(trailingOnly = TRUE)
n = as.integer(args[1])
m_i = as.integer(args[2])
setting = as.integer(args[3])
task_id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

stopifnot(setting %in% c(1, 2), !is.na(task_id))

m_total = n * m_i
beta = c(2.25, -1.1, 2.43, 0, 0)
sig_e = 1                                  # variance of errors
sig_u = if (setting == 1) 1 else 0.5       # variance of random effects

scenario = paste0("S", setting, "_n", n, "_m", m_i)
output_dir = file.path("results", scenario)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Covariates (fixed over replicates) ---------------------------------------
data_name = paste0("simulations_n", n, "_mi", m_i)
data(list = data_name, package = "postcAIC", envir = environment())
X = get(data_name)
X_intercept = cbind(rep(1, m_total), X)
clusterID = rep(1:n, each = m_i)

# Model set: intercept and x_2 in all models (2^3 = 8 models) --------------
common = c(1:2)
modelset_matrix = create_modelset("part_subset", common = common, p = 5)
index_full = which(rowSums(modelset_matrix) == ncol(modelset_matrix))

# Selection rule used everywhere (screening and RBG's congruency check):
# cAIC computed for every model at the variance estimate of the full model
select_model = function(y) {
  compute_cAIC_for_model_set(
    X,
    y,
    clusterID,
    model = "NERM",
    covariate_selection_matrix = NULL,
    modelset  = "part_subset",
    common = common,
    intercept = FALSE,
    theta = "full"
  )
}

# Screen this task's block of seeds until the full model is selected ------
seed_block = 10000
found = FALSE
n_selection_errors = 0    # seeds skipped because the selection failed
for (k in 1:(seed_block - 1)) {
  seed = task_id * seed_block + k
  set.seed(seed)

  e_ij = rnorm(m_total, 0, sqrt(sig_e))
  u_i = rnorm(n, 0, sqrt(sig_u))
  y = as.vector(X_intercept %*% beta) + rep(u_i, each = m_i) + e_ij

  sel = tryCatch(select_model(y), error = function(e) NULL)
  if (is.null(sel)) {
    n_selection_errors = n_selection_errors + 1
    next
  }
  if (sel$cAIC_min == index_full) {
    found = TRUE
    n_tries = k
    break
  }
}
if (!found) stop("The full model was not selected for any seed of this task's block.")

# True values ------------------------------------------------------------
X_cluster_full = as.matrix(sel$X_cluster_full)    # k_i = cluster means (with intercept)
k_bar = colMeans(X_cluster_full)                  # average linear combination
truth = list(
  beta = beta,
  kbar = sum(k_bar * beta),
  kbeta = as.vector(X_cluster_full %*% beta),
  mu = as.vector(X_cluster_full %*% beta) + u_i
)

# Linear combinations: n cluster means, then their average (last row)
x_beta_lin_com = rbind(X_cluster_full, k_bar)

# Helpers to store intervals as two-column matrices (lower, upper) -----
as_interval = function(lower, upper) {
  cbind(lower = as.vector(lower), upper = as.vector(upper))
}

split_lin_com = function(lower, upper) {
  list(kbeta = as_interval(lower[1:n], upper[1:n]),
       kbar = as_interval(lower[n + 1], upper[n + 1]))
}

intervals = list()
timings = list()
PC_info = list()

# Labels of the four versions of the post-cAIC intervals
PC_labels = c("model_g1" = "PC",
              "model_g1+2g3" = "PC-g3",
              "robust_g1" = "PC-rob",
              "robust_g1+2g3" = "PC-rob-g3")

# PC: post-cAIC intervals ---------------------------------------------------
for (vcov in c("model", "robust")) {
  for (var_psi in c("g1", "g1+2g3")) {
    label = PC_labels[[paste0(vcov, "_", var_psi)]]
    timings[[label]] = system.time({
      res_PC = postcAIC_CI(
        sel$cAIC_min,
        sel$degcAIC_models,
        sel$X_full,
        sel$X_cluster_full,
        sel$sig_u_full,
        sel$sig_e_full,
        model = "NERM",
        clusterID,
        sel$beta_sel,
        sel$mu_sel,
        sel$modelset_matrix,
        x_beta_lin_com = x_beta_lin_com,
        cAIC_models = sel$cAIC_models,
        theta = sel$theta,
        y = y,
        var_psi = var_psi,
        vcov = vcov
      )
    })[["elapsed"]]

    intervals[[label]] = c(
      list(beta = as_interval(res_PC$beta_postcAIC_CI_do, res_PC$beta_postcAIC_CI_up)),
      split_lin_com(res_PC$beta_x_postcAIC_CI_do, res_PC$beta_x_postcAIC_CI_up),
      list(mu = as_interval(res_PC$mixed_postcAIC_CI_do, res_PC$mixed_postcAIC_CI_up))
    )
    PC_info[[label]] = list(models_compared = res_PC$models_compared,
                            c_n = res_PC$c_n,
                            sampler_used = res_PC$sampler_used,
                            acceptance_rate = res_PC$acceptance_rate)
  }
}

# N1, N2: naive intervals ------------------------------------------------------
indices_sel = sel$indices_sel
C_cluster_sel = cbind(X_cluster_full[, indices_sel, drop = FALSE], diag(n))

timings[["N1-N2"]] = system.time({
  res_N = naive_CI(
    sel$beta_sel,
    sel$mu_sel,
    sel$sig_u_sel,
    sel$sig_e_sel,
    sel$sig_u_full,
    sel$sig_e_full,
    sel$X_full,
    C_cluster_sel,
    clusterID,
    indices_sel,
    type_MSE_mixed = "both",
    x_beta_lin_com = x_beta_lin_com
  )
})[["elapsed"]]

intervals[["N1"]] = c(
  list(beta = as_interval(res_N$beta_naive_CI_do, res_N$beta_naive_CI_up)),
  split_lin_com(res_N$beta_x_naive_CI_do, res_N$beta_x_naive_CI_up),
  list(mu = as_interval(res_N$mixed_naive_CI_do, res_N$mixed_naive_CI_up))
)
intervals[["N2"]] = list(
  mu = as_interval(res_N$mixed_naive_CI_corrected_do,
                   res_N$mixed_naive_CI_corrected_up)
)

# OBSP: Sugasawa et al. (2019), mixed effects only --------------------------
timings[["OBSP"]] = system.time({
  res_OBSP = tryCatch(
    postOBSP_CI(
      X,
      y,
      clusterID,
      X_cluster_full,
      model = "NERM",
      covariate_selection_matrix = NULL,
      modelset  = "part_subset",
      intercept = FALSE,
      common = common,
      boot = 1000
    ),
    error = function(e) NULL
  )
})[["elapsed"]]

if (!is.null(res_OBSP)) {
  intervals[["OBSP"]] = list(mu = as_interval(res_OBSP$postOBSP_do,
                                              res_OBSP$postOBSP_up))
} else {
  intervals[["OBSP"]] = list(mu = as_interval(rep(NA, n), rep(NA, n)))
}

# RBG: Ruegamer, Baumann and Greven (2022) ----------------------------------
dat = data.frame(y = y, X_intercept)
names(dat)[2:6] = c("X1", "X2", "X3", "X4", "X5")
dat$clusterID = clusterID

mod_sel = lmer(y ~ -1 + X1 + X2 + X3 + X4 + X5 + (1 | clusterID), data = dat)
this_Z = getME(mod_sel, "Z")

# Congruency check: same selection rule as the screening
n_check_errors = 0
checkFun = function(y) {
  sel_y = tryCatch(select_model(y), error = function(e) NULL)
  if (is.null(sel_y)) {
    n_check_errors <<- n_check_errors + 1
    return(FALSE)
  }
  sel_y$cAIC_min == index_full
}

# Lower and upper limits of a mocasin result (columns 4 and 5 of selinf)
rbg_limits = function(res, k = 1) {
  values = as.vector(unlist(res$selinf[[k]]))
  c(values[4], values[5])
}
rbg_try = function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

RBG_beta = matrix(NA, nrow = 5, ncol = 2, dimnames = list(NULL, c("lower", "upper")))
RBG_kbeta = matrix(NA, nrow = n, ncol = 2, dimnames = list(NULL, c("lower", "upper")))
RBG_mu = matrix(NA, nrow = n, ncol = 2, dimnames = list(NULL, c("lower", "upper")))
RBG_kbar = matrix(NA, nrow = 1, ncol = 2, dimnames = list(NULL, c("lower", "upper")))

timings[["RBG"]] = system.time({
  # beta_1 and beta_5
  res_RBG = rbg_try(mocasin(
    this_y = dat$y,
    mod = mod_sel,
    checkFun = checkFun,
    which = c(1, 5)
  ))
  if (!is.null(res_RBG)) {
    RBG_beta[1, ] = rbg_limits(res_RBG, 1)
    RBG_beta[5, ] = rbg_limits(res_RBG, 2)
  }

  # Average linear combination k_bar^T beta
  C_bar = cbind(t(as.matrix(k_bar)), this_Z[1, , drop = FALSE] * 0)
  res_RBG = rbg_try(mocasin(
    this_y = dat$y,
    mod = mod_sel,
    checkFun = checkFun,
    conditional = TRUE,
    lincomb_matrix = C_bar
  ))
  if (!is.null(res_RBG)) RBG_kbar[1, ] = rbg_limits(res_RBG)

  # First row of each cluster (clusters are numbered 1, ..., n)
  first_row = match(1:n, clusterID)

  for (i in 1:n) {
    # Row of Z for cluster i (its first unit)
    Z_row_i = this_Z[first_row[i], , drop = FALSE]

    # Linear combination k_i^T beta
    C_i = cbind(t(as.matrix(X_cluster_full[i, ])), Z_row_i * 0)
    res_RBG = rbg_try(mocasin(
      this_y = dat$y,
      mod = mod_sel,
      checkFun = checkFun,
      conditional = TRUE,
      lincomb_matrix = C_i
    ))
    if (!is.null(res_RBG)) RBG_kbeta[i, ] = rbg_limits(res_RBG)

    # Mixed effect mu_i = k_i^T beta + u_i
    C_i = cbind(t(as.matrix(X_cluster_full[i, ])), Z_row_i)
    res_RBG = rbg_try(mocasin(
      this_y = dat$y,
      mod = mod_sel,
      checkFun = checkFun,
      conditional = TRUE,
      lincomb_matrix = C_i
    ))
    if (!is.null(res_RBG)) RBG_mu[i, ] = rbg_limits(res_RBG)
  }
})[["elapsed"]]

intervals[["RBG"]] = list(beta = RBG_beta, kbar = RBG_kbar, kbeta = RBG_kbeta, mu = RBG_mu)

# Save ---------------------------------------------------------------------
Results = list(
  scenario = list(n = n, m_i = m_i, setting = setting,
                  sig_e = sig_e, sig_u = sig_u),
  task_id = task_id,
  seed = seed,
  n_tries = n_tries,
  n_selection_errors = n_selection_errors,
  truth = truth,
  intervals = intervals,
  PC_info = PC_info,
  n_check_errors = n_check_errors,
  timings = timings
)

save("Results", file = file.path(output_dir, sprintf("task_%04d.RData", task_id)))
