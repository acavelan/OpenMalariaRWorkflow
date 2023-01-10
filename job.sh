#!/bin/bash

#SBATCH --job-name=@jobname@
#SBATCH --time=00:30:00
#SBATCH --account=@account@
#SBATCH --qos=30min
#SBATCH --output=/dev/null #%a.err #%A_%a.err
#SBATCH --error=/dev/null #%a.err #%A_%a.out
#SBATCH --mem=1G
#SBATCH --array=1-@N@%1000

export LMOD_DISABLE_SAME_NAME_AUTOSWAP=no

ml OpenMalaria/44.0-iomkl-2019.01

SEEDFILE="commands.txt"
SEED=$(sed -n ${SLURM_ARRAY_TASK_ID}p $SEEDFILE)
eval $SEED
