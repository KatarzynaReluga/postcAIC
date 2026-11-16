#!/bin/bash
#SBATCH --account=relugaka
#SBATCH --partition=standard
#SBATCH --mail-type=ALL
#SBATCH --job-name=S2_n15_m5
#SBATCH --array=1-1000
#SBATCH --time=10:00:00
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=3000MB
#SBATCH --output=logs/%x_%a.out

# Scenario S2, n = 15 clusters, m_i = 5 units per cluster.
# Submit from the folder containing sim_postcAIC.r, after: mkdir -p logs
# Results: results/S2_n15_m5/task_XXXX.RData

module load R/4.5.2-gcc15.2.0
export LD_LIBRARY_PATH=$HOME/lib:$LD_LIBRARY_PATH
export R_LIBS_USER=$HOME/Rlibs/4.5

ldd /software/R/4.5.2-gcc15.2.0/lib64/R/lib/libR.so | grep "not found"
Rscript -e 'R.version.string'
Rscript sim_postcAIC.r 15 5 2
