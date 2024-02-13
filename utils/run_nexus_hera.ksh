#!/bin/bash

set -x

#SBATCH -A gsd-fv3-dev
#SBATCH -q debug
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --time=00:20:00
#SBATCH --chdir=.

rm -f NEXUS.log

srun ./nexus -c HEMCO_Config.rc -r grid_spec_C401.nc -d 

srun ./run_nco_combine_ant_bio.sh
