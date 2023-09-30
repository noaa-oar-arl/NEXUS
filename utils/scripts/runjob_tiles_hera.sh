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
#for year in {1990..2018}; do 
#    for month in 01 02 03 04 05 06 07 08 09 10 11 12; do
#	yyyymmdd=$(date -d "${year}${month}01" +"%Y%m%d00")
#for i in {0..365}; do
#    yyyymmdd=$(date -d "20210101 + ${i} days" +"%Y%m%d00")
#    end_yyyymmdd=$(date -d "20210101 + $(expr $i + 1) days" +"%Y%m%d00")
	
  #  echo "./nexus_time_parser.py -f HEMCO_sa_Time.rc -s ${yyyymmdd} -m True"

#    ./nexus_time_parser.py -f HEMCO_sa_Time.rc -s ${yyyymmdd} -m True

#    srun -l $binary -c NEXUS_Config.rc
#done
 # ./nexus_time_parser.py -f HEMCO_sa_Time.rc -s ${yyyymmdd} --monthly=True
#      #for i in tile1 tile2 tile3 tile4 tile5 tile6; do
#      #srun -l $binary -c HEMCO_Config.rc # -r C384_grid_spec.${i}.nc  -d 1 -o GEFS.2019${m}01-${exp_name}.${i}.nc
#      srun -l $binary -c HEMCO_Config.rc
# #     done
# # #    rm NEXUS_Diag.nc
# # #    mv NEXUS_Diag.nc GEFS.2019${m}01-${exp_name}.nc
#  done
#done

# #dump to GEFS-Aerosol Binary input data
# conda activate dev

# for m in 01 02 03 04 05 06 07 08 09 10 11 12; do
#     rm tile1/*.dat tile2/*.dat tile3/*.dat tile4/*.dat tile5/*.dat tile6/*.dat 
#     for i in tile1 tile2 tile3 tile4 tile5 tile6; do
# 	./dump_to_binary.py -f GEFS.2019${m}01-${exp_name}.${i}.nc
#     done
# done

