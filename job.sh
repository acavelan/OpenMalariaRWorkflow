#!/bin/bash

#SBATCH --job-name=@jobname@
#SBATCH --time=@time@
#SBATCH --account=@account@
#SBATCH --partition=@partition@
#SBATCH --qos=@qos@
#SBATCH --output=log/%A_%a.out
#SBATCH --error=log/%A_%a.err
#SBATCH --mem-per-cpu=@MEM_PER_CPU@
#SBATCH --cpus-per-task=@CPUS_PER_TASK@
#SBATCH --array=1-@N@

export LMOD_DISABLE_SAME_NAME_AUTOSWAP="no"

module load parallel

SEEDFILE="commands.txt"
START=$(( (SLURM_ARRAY_TASK_ID - 1) * @BATCH_SIZE@ + 1 ))
END=$(( SLURM_ARRAY_TASK_ID * @BATCH_SIZE@ ))

sed -n "${START},${END}p" "$SEEDFILE" | parallel -j"@CPUS_PER_TASK@"
