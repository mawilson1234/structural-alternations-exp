#!/bin/bash

#SBATCH --job-name=salts_exp_power_analysis
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --nodes=1
#SBATCH --ntasks=4
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --time=48:00:00
#SBATCH --mail-type=FAIL,END,INVALID_DEPEND

module load R/4.1.0-foss-2020b # whatever this actually is, make sure the right packages are available

echo Running script: scripts/salts_exp_power_analysis.sh

cd analysis/

Rscript power_analysis.r