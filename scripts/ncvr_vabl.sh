#!/bin/bash
#SBATCH --array=1
#SBATCH --mail-type=end
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=ncvr_vabl
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=64G
#
# modules
#

Rscript code/ncvr_vabl.R
