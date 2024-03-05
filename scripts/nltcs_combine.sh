#!/bin/bash
#SBATCH --mail-type=end
#SBATCH --output=logs/%x.out
#SBATCH --error=logs/%x.err
#SBATCH --job-name=nltcs_combine
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=64G
#
# modules
#

Rscript code/nltcs_combine.R
