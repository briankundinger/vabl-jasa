#!/bin/bash
#SBATCH --array=1
#SBATCH --mail-type=end
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=nltcs_inference
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=128G

Rscript code/nltcs_inference.R
