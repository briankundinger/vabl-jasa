#!/bin/bash
#SBATCH --array=1-30
#SBATCH --mail-type=end
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=nltcs_hash
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=32G

Rscript code/nltcs_hash.R
