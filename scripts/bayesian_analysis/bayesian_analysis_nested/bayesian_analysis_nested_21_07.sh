#!/bin/bash

#SBATCH --job-name=salts_exp_bayes_nested_21_07
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=05:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/bayesian_analysis_nested_21_07.sh

cd analysis/

Rscript Bayesian\ scripts/models-nested/models_nested_21_07.r
