######################################################################
# Check of the fast selection rule (fast_selection.r) used in the    #
# congruency check of RBG.                                           #
#                                                                    #
# Usage (from the folder simulations/hpc):                           #
#   Rscript check_fast_selection.r <n> <m_i> <setting> [n_datasets]  #
#   e.g. Rscript check_fast_selection.r 90 5 1 200                   #
#                                                                    #
# For n_datasets responses generated as in sim_postcAIC.r, compares  #
# the cAIC values and the selected model of the fast rule with those #
# of compute_cAIC_for_model_set(..., theta = "full"), and reports    #
# the time per selection of both.                                    #
######################################################################

library(postcAIC)
source("fast_selection.r")

args = commandArgs(trailingOnly = TRUE)
n = as.integer(args[1])
m_i = as.integer(args[2])
setting = as.integer(args[3])
n_datasets = if (length(args) >= 4) as.integer(args[4]) else 200

m_total = n * m_i
beta = c(2.25, -1.1, 2.43, 0, 0)
sig_e = 1
sig_u = if (setting == 1) 1 else 0.5

data_name = paste0("simulations_n", n, "_mi", m_i)
data(list = data_name, package = "postcAIC", envir = environment())
X = get(data_name)
X_intercept = cbind(rep(1, m_total), X)
clusterID = rep(1:n, each = m_i)

common = c(1:2)
modelset_matrix = create_modelset("part_subset", common = common, p = 5)

# Selection rule of sim_postcAIC.r and its fast version
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
select_model_fast = make_fast_selection(X, clusterID, modelset_matrix)

# Responses generated as in sim_postcAIC.r
ys = lapply(1:n_datasets, function(r) {
  set.seed(r)
  e_ij = rnorm(m_total, 0, sqrt(sig_e))
  u_i = rnorm(n, 0, sqrt(sig_u))
  as.vector(X_intercept %*% beta) + rep(u_i, each = m_i) + e_ij
})

# One call of each before timing
invisible(select_model(ys[[1]]))
invisible(select_model_fast(ys[[1]]))

time_package = system.time(
  res_package <- lapply(ys, function(y)
    tryCatch(select_model(y), error = function(e) NULL))
)[["elapsed"]]
time_fast = system.time(
  res_fast <- lapply(ys, function(y)
    tryCatch(select_model_fast(y), error = function(e) NULL))
)[["elapsed"]]

failed_package = sapply(res_package, is.null)
failed_fast = sapply(res_fast, is.null)
both = which(!failed_package & !failed_fast)

max_difference = max(sapply(both, function(r)
  max(abs(res_package[[r]]$cAIC_models - res_fast[[r]]$cAIC_models))))
same_selection = sum(sapply(both, function(r)
  res_package[[r]]$cAIC_min == res_fast[[r]]$cAIC_min))

seconds_package = time_package / n_datasets
seconds_fast = time_fast / n_datasets
n_calls = 2 * n + 2

cat("Scenario S", setting, "_n", n, "_m", m_i, ", ", n_datasets,
    " datasets\n", sep = "")
cat("Failed selections: package", sum(failed_package),
    ", fast", sum(failed_fast), "\n")
cat("Largest absolute difference in cAIC:", signif(max_difference, 3), "\n")
cat("Datasets with the same selected model:", same_selection, "of",
    length(both), "\n")
cat("Seconds per selection: package", round(seconds_package, 4),
    ", fast", round(seconds_fast, 4), "\n")
cat("Hours for", n_calls, "calls of mocasin x 1000 selections: package",
    round(seconds_package * n_calls * 1000 / 3600, 1),
    ", fast", round(seconds_fast * n_calls * 1000 / 3600, 1), "\n")
