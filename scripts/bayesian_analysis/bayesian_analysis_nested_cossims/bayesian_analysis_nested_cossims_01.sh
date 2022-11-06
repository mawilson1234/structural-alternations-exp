#!/bin/bash

#SBATCH --job-name=salts_exp_bayes-nested-cossims_01
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=24:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b

echo Running script: scripts/bayesian_analysis_nested_cossims_01.sh

cd analysis/

Rscript Bayesian\ scripts/models-nested-cossims/models_nested_cossims_01.r
