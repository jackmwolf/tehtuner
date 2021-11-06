#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=2580M
#SBATCH --mail-type=ALL
#SBATCH --mail-user=wolfx681@umn.edu
#SBATCH -A koopmein
#SBATCH -o /panfs/roc/groups/11/koopmein/wolfx681/Slurm/LogFiles/%A_%a.out
#SBATCH -e /panfs/roc/groups/11/koopmein/wolfx681/Slurm/LogFiles/%A_%a.err

date

path=/panfs/roc/groups/11/koopmein/wolfx681/VT/Chuyu/$c_name
mkdir -p $path
mkdir -p $path/Logfiles

cd $path
module load R/3.6.0
eval "R CMD BATCH --no-save --no-restore '--args dg=$dg vt1=$vt1 sims=$sims c_name=\"$c_name\"' $path/../Scripts/Rcode/Cond.R $path/Logfiles/$c_name.job$SLURM_ARRAY_TASK_ID.txt"
