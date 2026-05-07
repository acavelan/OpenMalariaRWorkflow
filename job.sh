#!/bin/bash

#SBATCH --job-name=@jobname@
#SBATCH --time=@time@
#SBATCH --account=@account@
#SBATCH --qos=@qos@
#SBATCH --output=log/%A_%a.out  # Job array output
#SBATCH --error=log/%A_%a.err   # Job array error
#SBATCH --mem-per-cpu=1G
#SBATCH --cpus-per-task=@CPUS_PER_TASK@  # Number of CPUs per job
#SBATCH --array=1-@N@             # Adjust @N@ to the number of job bundles

export LMOD_DISABLE_SAME_NAME_AUTOSWAP="no"

# Load GNU parallel
ml parallel

# Prepare environment
# ml OpenMalaria/48.0-intel-compilers-2025.2.0
ml CMake/4.0.3-GCCcore-14.3.0
ml GCCcore/14.3.0
ml intel-compilers/2025.2.0
ml GSL/2.8-intel-compilers-2025.2.0 
ml Xerces-C++/3.3.0-GCCcore-14.3.0
ml XSD/4.0.0-GCCcore-14.3.0

# Define the seed file
SEEDFILE="commands.txt"

# Calculate the start and end line numbers for this job array task
START=$(( (SLURM_ARRAY_TASK_ID - 1) * @BATCH_SIZE@ + 1 ))
END=$(( SLURM_ARRAY_TASK_ID * @BATCH_SIZE@ ))

# Pipe the relevant lines to GNU parallel
sed -n "${START},${END}p" "$SEEDFILE" | parallel -j"@CPUS_PER_TASK@"
