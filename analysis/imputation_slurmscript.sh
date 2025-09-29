#!/bin/bash
#SBATCH --job-name=mice_imputation       # Name of the job
#SBATCH --nodes=1                        # Number of nodes
#SBATCH --ntasks=1                       # Number of tasks (R is single-threaded for mice)
#SBATCH --cpus-per-task=1               # Absolute minimum - should start immediately
#SBATCH --partition=c64-m512            # Regular partition
#SBATCH --mem=8G                        # Minimal memory request
#SBATCH --output=mice_imputation_%j.out # Name of the output file with job ID
#SBATCH --error=mice_imputation_%j.err  # Name of the error file with job ID
#SBATCH --time=48:00:00                 # 2 days to compensate for minimal resources
#SBATCH --account=general               # Specify the account explicitly
#SBATCH --gres=none                     # Explicitly specify no GPU requirement

# Print job information
echo "Job started at: $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Running on node: $(hostname)"
echo "Working directory: $(pwd)"

# Change to project directory
cd ~/p4project

# Initialize conda
conda init bash > /dev/null 2>&1
source ~/.bashrc

# Activate conda environment
echo "Activating conda environment..."
conda activate /users/jguo258/.conda/envs/p4_project

# Check R and required packages
echo "Checking R version and packages..."
Rscript -e "cat('R version:', R.version.string, '\n'); cat('mice version:', packageVersion('mice'), '\n'); cat('dplyr version:', packageVersion('dplyr'), '\n')"

# Run the imputation script
echo "Starting mice imputation for all datasets..."
echo "Expected to process 6 datasets with ~12,271 rows and 50 columns each"
echo "30 imputations × 20 iterations per dataset"

# Set R environment variables for better performance
export R_MAX_VSIZE=8Gb
export OMP_NUM_THREADS=1

# Run the R script
Rscript analysis/psban_imputation_by_visit.R

# Print completion information
echo "Job completed at: $(date)"
echo "Exit code: $?"
