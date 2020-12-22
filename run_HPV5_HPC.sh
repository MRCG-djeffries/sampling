#! /bin/bash
# This uses array function of slurm
#SBATCH --job-name test_survey
#SBATCH --out survey.out
#SBATCH --nodes 1
#SBATCH --ntasks 1
#SBATCH --cpus-per-task 1
#SBATCH --mem 200
#SBATCH --array 1-980
./run_HPV5_HPC.R $1 $2 $SLURM_ARRAY_TASK_ID
