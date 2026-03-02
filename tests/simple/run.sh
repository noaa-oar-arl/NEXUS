#!/bin/bash
#SBATCH --job-name=nexus_job      # Name of the job
#SBATCH --output=nexus_%j.out    # Standard output log (%j expands to jobId)
#SBATCH --error=nexus_%j.err     # Standard error log
#SBATCH --nodes=1                # -N 1: Request 1 node
#SBATCH --ntasks=1               # -n 1: Run 1 task
#SBATCH --account=da-cpu    # -A: Account to charge
#SBATCH --time=00:10:00          # Wall time (HH:MM:SS) - adjust as needed
#SBATCH --partition=u1-compute        # Specify your partition/queue name

NXS_HOME=/scratch4/NCEPDEV/naqfc/Barry.Baker/models/NEXUSv2

# ./build.sh -f -c "-DCMAKE_BUILD_TYPE=Debug"
# Load modules but stay in current directory (tests/simple)
module use ${NXS_HOME}/modulefiles
module load nexus_ursa.intel

# Run from tests/simple so local nexus.rc is used
cd ${NXS_HOME}/tests/simple

# Standalone mode and grid file are now configured in nexus.rc

# Execute the application from the tests/simple directory so it reads the correct nexus.rc
timeout 30 srun ${NXS_HOME}/build/bin/nexus
