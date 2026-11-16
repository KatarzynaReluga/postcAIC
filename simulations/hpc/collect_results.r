######################################################################
# Summary of one scenario of the simulations of Section 5.           #
#                                                                    #
# Usage:                                                             #
#   Rscript collect_results.r results/S2_n90_m5                      #
#                                                                    #
# Reads all task_*.RData files of the scenario and computes, for     #
# every method and target,                                           #
#   cov    empirical coverage (%)                                    #
#   err    |cov - 95| / 95 * 100                                     #
#   len    average length of the intervals                           #
#   sd_len standard deviation of the lengths over simulation runs    #
#   runs   number of runs with an interval (failed runs excluded)    #
# For beta_j and k_bar^T beta these are computed over runs. For      #
# k_i^T beta and mu_i they are computed over runs for every cluster  #
# i and then averaged over clusters.                                 #
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
stack_rows = function(get_values) {
  do.call(rbind, lapply(all_results, get_values))
}

summarise_units = function(lower, upper, truth) {
  covered = (lower <= truth) & (truth <= upper)
  len = upper - lower
  data.frame(
    cov = 100 * colMeans(covered, na.rm = TRUE),
    len = colMeans(len, na.rm = TRUE),
    sd_len = apply(len, 2, sd, na.rm = TRUE),
    runs = colSums(!is.na(len))
  )
}

methods = unique(unlist(lapply(all_results, function(r) names(r$intervals))))
summary_rows = list()

for (method in methods) {
  targets = names(all_results[[1]]$intervals[[method]])

  for (target in targets) {
    lower = stack_rows(function(r) r$intervals[[method]][[target]][, "lower"])
    upper = stack_rows(function(r) r$intervals[[method]][[target]][, "upper"])
    truth = stack_rows(function(r) r$truth[[target]])

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
        len = mean(by_unit$len, na.rm = TRUE),
        sd_len = mean(by_unit$sd_len, na.rm = TRUE),
        runs = min(by_unit$runs)
      )
    }
  }
}

summary_table = do.call(rbind, summary_rows)
summary_table$err = abs(summary_table$cov - 95) / 95 * 100
summary_table = summary_table[, c("method", "target", "cov", "err", "len", "sd_len", "runs")]
rownames(summary_table) = NULL

output_file = file.path(dirname(normalizePath(folder)),
                        paste0("summary_", scenario, ".csv"))
write.csv(summary_table, output_file, row.names = FALSE)

# Print ----------------------------------------------------------------------
cat("Scenario", scenario, ":", n_runs, "runs\n\n")
print(format(summary_table, digits = 3), row.names = FALSE)

cat("\nAverage running time per run (seconds):\n")
timings = stack_rows(function(r) unlist(r$timings))
print(round(colMeans(timings), 1))

cat("\nAverage number of seeds tried per run:",
    round(mean(sapply(all_results, function(r) r$n_tries)), 1), "\n")

cat("\nSampler used by PC, number of runs:\n")
print(table(sapply(all_results, function(r) r$PC_info[["PC"]]$sampler_used)))
cat("Average acceptance rate of rejection sampling:",
    round(mean(sapply(all_results, function(r)
      r$PC_info[["PC"]]$acceptance_rate), na.rm = TRUE), 3), "\n")
cat("Average number of models compared with:",
    round(mean(sapply(all_results, function(r)
      length(r$PC_info[["PC"]]$models_compared))), 2), "\n")

cat("\nRuns with failed selections in RBG's congruency checks:",
    sum(sapply(all_results, function(r) r$n_check_errors) > 0), "\n")
cat("Saved to", output_file, "\n")
