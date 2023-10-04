#!/bin/sh
#SBATCH --account=rda-arl-gpu
#SBATCH --qos=batch
#SBATCH --partition=hera
#SBATCH --nodes=1
#SBATCH --tasks-per-node=40
#SBATCH -t 04:30:00
#SBATCH -o nxs.log
#SBATCH -e nxs.err


# Embedded bash script:
binary=nexus

module use -a /scratch1/RDARCH/rda-arl-gpu/Barry.Baker/models/nexus_latest/modulefiles
ml hera.intel

#export OMP_NUM_THREADS=40 MKL_NUM_THREADS=0

pushd /scratch1/RDARCH/rda-arl-gpu/Barry.Baker/models/nexus_latest/config/gocart

binary=nexus
exp_name='simple_ceds'
dstr='20180201'
yyyymmdd=$(date -d ${dstr} +"%Y%m%d")
m=$(date -d ${dstr} +"%m")
y=$(date -d ${dstr} +"%Y")
#srun -l $binary -c NEXUS_Config.rc -d
#binary=hemco_standalone
./nexus_time_parser.py -f HEMCO_sa_Time.rc -s ${yyyymmdd}00 -m True
#srun -l $binary -c NEXUS_Config.rc 
for tile in  tile1 tile2 tile3 tile4 tile5 tile6; do 
    output=GEFS.${y}${m}01-${exp_name}.${tile}.nc
    grid=FV3Grids/C384_grid_spec.${tile}.nc
    srun -l $binary -c NEXUS_Config.rc -r ${grid} -d 0 -o ${output}
    pretty=${output}4
    ./make_nexus_output_pretty.py -s ${output} -g ${grid} -t HEMCO_sa_Time.rc -o ${pretty}
    rm ${output}
done
