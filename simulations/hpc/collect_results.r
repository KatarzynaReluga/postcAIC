######################################################################
# Summary of one scenario of the simulations of Section 5.           #
#                                                                    #
# Usage:                                                             #
#   Rscript collect_results.r results/S2_n90_m5                      #
#                                                                    #
# Reads all task_*.RData files of the scenario. For every method,   #
# target and unit (coefficient beta_j, the average combination       #
# k_bar^T beta, or cluster i for k_i^T beta and mu_i) it computes,   #
# over the simulation runs in which the method returned an interval, #
#   cov    empirical coverage (%)                                    #
#   err    |cov - 95| / 95 * 100                                     #
#   len    average length of the finite intervals                    #
#   sd_len standard deviation of the lengths of the finite intervals #
#   inf    share of infinite intervals (%)                           #
#   runs   number of runs with an interval                           #
# For k_i^T beta and mu_i these per-cluster values are then averaged #
# over the clusters (runs: the smallest number over the clusters).   #
# The failures of the method of Ruegamer et al. (RBG) are reported   #
# separately, together with the runs in which it was started but     #
# did not finish.                                                    #
# Writes results/summary_<scenario>.csv and prints the table.        #
######################################################################

args = commandArgs(trailingOnly = TRUE)
folder = args[1]
scenario = basename(normalizePath(folder))

files = list.files(folder, pattern = "^task_.*\\.RData$", full.names = TRUE)
stopifnot("No task_*.RData files found" = length(files) > 0)

all_results = lapply(files, function(f) {
  load(f)
  Results
})
n_runs = length(all_results)

# Matrix runs x units of lower limits, upper limits or true values -------
stack_rows = function(results, get_values) {
  do.call(rbind, lapply(results, get_values))
}

summarise_units = function(lower, upper, truth) {
  covered = (lower <= truth) & (truth <= upper)
  len = upper - lower
  returned = !is.na(len)
  infinite = returned & !is.finite(len)
  len_finite = ifelse(returned & is.finite(len), len, NA)
  cov = 100 * colMeans(covered, na.rm = TRUE)
  data.frame(
    cov = cov,
    err = abs(cov - 95) / 95 * 100,
    len = colMeans(len_finite, na.rm = TRUE),
    sd_len = apply(len_finite, 2, sd, na.rm = TRUE),
    inf = 100 * colSums(infinite) / pmax(colSums(returned), 1),
    runs = colSums(returned)
  )
}

methods = unique(unlist(lapply(all_results, function(r) names(r$intervals))))
summary_rows = list()

for (method in methods) {
  # Runs in which the method was computed
  results_method = Filter(function(r) !is.null(r$intervals[[method]]), all_results)
  targets = names(results_method[[1]]$intervals[[method]])

  for (target in targets) {
    lower = stack_rows(results_method,
                       function(r) r$intervals[[method]][[target]][, "lower"])
    upper = stack_rows(results_method,
                       function(r) r$intervals[[method]][[target]][, "upper"])
    truth = stack_rows(results_method, function(r) r$truth[[target]])

    by_unit = summarise_units(lower, upper, truth)

    if (target == "beta") {
      # One row per coefficient; coefficients without intervals are skipped
      for (j in seq_len(nrow(by_unit))) {
        if (by_unit$runs[j] > 0) {
          summary_rows[[length(summary_rows) + 1]] =
            data.frame(method = method, target = paste0("beta", j), by_unit[j, ])
        }
      }
    } else {
      # Average over clusters (k_bar has a single unit)
      summary_rows[[length(summary_rows) + 1]] = data.frame(
        method = method,
        target = target,
        cov = mean(by_unit$cov, na.rm = TRUE),
        err = mean(by_unit$err, na.rm = TRUE),
        len = mean(by_unit$len, na.rm = TRUE),
        sd_len = mean(by_unit$sd_len, na.rm = TRUE),
        inf = mean(by_unit$inf, na.rm = TRUE),
        runs = min(by_unit$runs)
      )
    }
  }
}

summary_table = do.call(rbind, summary_rows)
summary_table = summary_table[, c("method", "target", "cov", "err", "len",
                                  "sd_len", "inf", "runs")]
rownames(summary_table) = NULL

output_file = file.path(dirname(normalizePath(folder)),
                        paste0("summary_", scenario, ".csv"))
write.csv(summary_table, output_file, row.names = FALSE)

# Print ----------------------------------------------------------------------
cat("Scenario", scenario, ":", n_runs, "runs\n\n")
print(format(summary_table, digits = 3), row.names = FALSE)

cat("\nAverage running time per run (seconds), over the runs with the method:\n")
timing_names = unique(unlist(lapply(all_results, function(r) names(r$timings))))
average_timings = sapply(timing_names, function(name) {
  mean(unlist(lapply(all_results, function(r) r$timings[[name]])))
})
print(round(average_timings, 1))

cat("\nAverage number of seeds tried per run:",
    round(mean(sapply(all_results, function(r) r$n_tries)), 1), "\n")
cat("Seeds skipped because the selection failed (all runs):",
    sum(unlist(lapply(all_results, function(r) r$n_selection_errors))), "\n")

cat("\nSampler used by PC, number of runs:\n")
print(table(sapply(all_results, function(r) r$PC_info[["PC"]]$sampler_used)))
cat("Average acceptance rate of rejection sampling:",
    round(mean(sapply(all_results, function(r)
      r$PC_info[["PC"]]$acceptance_rate), na.rm = TRUE), 3), "\n")
cat("Average number of models compared with:",
    round(mean(sapply(all_results, function(r)
      length(r$PC_info[["PC"]]$models_compared))), 2), "\n")

# Failures of the method of Ruegamer et al. (RBG) ----------------------------
results_RBG = Filter(function(r) !is.null(r$RBG_info), all_results)
# Runs in which RBG was started but did not finish (e.g. time limit)
n_RBG_unfinished = sum(sapply(all_results, function(r)
  identical(r$RBG_status, "started")))

if (length(results_RBG) > 0 || n_RBG_unfinished > 0) {
  cat("\nMethod of Ruegamer et al. (RBG):", length(results_RBG), "runs\n")
  cat("  Runs in which RBG was started but did not finish:",
      n_RBG_unfinished, "\n")
}
if (length(results_RBG) > 0) {
  info_names = c("n_calls", "n_failed", "n_warnings", "n_check_errors")
  info = t(sapply(results_RBG, function(r) unlist(r$RBG_info[info_names])))
  n_infinite = sapply(results_RBG, function(r) {
    limits = do.call(rbind, r$intervals[["RBG"]])
    len = limits[, "upper"] - limits[, "lower"]
    sum(!is.na(len) & !is.finite(len))
  })
  cat("  Calls of mocasin per run:", info[1, "n_calls"], "\n")
  cat("  Failed calls (no interval):", sum(info[, "n_failed"]),
      "in", sum(info[, "n_failed"] > 0), "runs\n")
  cat("  Infinite intervals:", sum(n_infinite),
      "in", sum(n_infinite > 0), "runs\n")
  cat("  Limits that are not the required quantile (warnings):",
      sum(info[, "n_warnings"]), "in", sum(info[, "n_warnings"] > 0), "runs\n")
  cat("  Failed selections in the congruency checks:",
      sum(info[, "n_check_errors"]), "\n")
}

cat("Saved to", output_file, "\n")
