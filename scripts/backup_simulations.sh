#!/bin/bash

#SBATCH --job-name=backup_salts-exp-bayessims
#SBATCH --output=joblogs/%x_%j.txt
#SBATCH --time=00:45:00
#SBATCH --mail-type=END,FAIL,INVALID_DEPEND

backup analysis/ \
	--exclude=*.rds \
	# --exclude="./analysis/Bayesian scripts" \
	--exclude='./analysis/Pilot' \
	--exclude='./analysis/Models/Bayesian' \
	--exclude='./analysis/Plots/Bayesian' \
	--exclude=*.r \
	--exclude=results*.csv
