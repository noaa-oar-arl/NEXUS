#!/bin/bash

#CMAQv5.2.1 CB6 Emission Species w/biogenics
declare -a species=( "AACD" "ACET" "ACROLEIN" "ALD2" "ALD2_PRIMARY" "ALDX" "APIN" "BENZ" "BUTADIENE13" "CH4" "CH4_INV" "CL2" "CO" "CO2_INV" "ETH" "ETHA" "ETHY" "ETOH" "FACD" "FORM" "FORM_PRIMARY" "HCL" "HONO" "IOLE" "ISOP" "KET" "MEOH" "NAPH" "NH3" "NH3_FERT" "NO" "NO2" "OLE" "PAL" "PAR" "PCA" "PCL" "PEC" "PFE" "PH2O" "PK" "PMC" "PMG" "PMN" "PMOTHR" "PNA" "PNCOM" "PNH4" "PNO3" "POC" "PRPA" "PSI" "PSO4" "PTI" "SESQ" "SO2" "SOAALK" "SULF" "TERP" "TOL" "UNK" "UNR" "VOC_INV" "XYLMN" )


# expect arguments 
if [ $# -lt 2 ]; then
  echo "Expected two arguments (input and output paths)"
  echo "Got: $@"
  exit 2
fi

#Set input and output files
input=$1 
output=$2
rm -f $output

#Process emissions
echo "Combining/calculating anthro and bio emissions..."
for specie in "${species[@]}"
do
echo ${specie}

#1.Use HEMCO MEGANv2.1 instantaneous diagnostic for some bio-only species
if [ ${specie} == "AACD" ] 
then
ncap2 -A -v -s "${specie} = InvMEGAN_AAXX" $input $output
elif [ ${specie} == "FACD" ]
then
ncap2 -A -v -s "${specie} = InvMEGAN_FAXX" $input $output
elif [ ${specie} == "APIN" ]
then
ncap2 -A -v -s "${specie} = InvMEGAN_APIN" $input $output
#Sesquiterpene is also only biogenic
elif [ ${specie} == "SESQ" ]
then
ncap2 -A -v -s "${specie} = ${specie}_bio" $input $output


#2. Following species need to combine HEMCO anthropogenic and MEGANv2.1 biogenic species
elif [ ${specie} == "ACET" ] || [ ${specie} == "ALD2" ] || [ ${specie} == "ETH" ] || [ ${specie} == "ETOH" ] || [ ${specie} == "ISOP" ] || [ ${specie} == "MEOH" ] || [ ${specie} == "OLE" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ${specie}_bio" $input $output
#Lumped terpene should combine all HEMCO MEGANv2.1 terpenoids
elif [ ${specie} == "TERP" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + MTPA_bio + MTPO_bio + LIMO_bio" $input $output

#3. Following species are approximately calculated from other BVOCs (estimated from NACC-CMAQv5.3.1/BEISv3.6.1 summer simulation) and then combined with anthropogenic
elif [ ${specie} == "IOLE" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + OLE_bio*0.967963" $input $output
elif [ ${specie} == "PAR" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + (MTPA_bio + MTPO_bio + LIMO_bio)*0.576825" $input $output
elif [ ${specie} == "ETHA" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ETH_bio*0.160406" $input $output
elif [ ${specie} == "ALDX" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ALD2_bio*0.166038" $input $output
elif [ ${specie} == "FORM" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ALD2_bio*0.914909" $input $output
elif [ ${specie} == "FORM_PRIMARY" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ALD2_bio*0.914909" $input $output
elif [ ${specie} == "ALD2_PRIMARY" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ALD2_bio*0.670921" $input $output
elif [ ${specie} == "KET" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + ACET_bio*0.0339559" $input $output
elif [ ${specie} == "CO" ]
then
ncap2 -A -v -s "${specie} = ${specie}_ant + (MTPA_bio + MTPO_bio + LIMO_bio)*0.4666" $input $output


#4. The remainder of species are just anthropogenic from HEMCO
else
ncap2 -A -v -s "${specie} = ${specie}_ant" $input $output
fi

done
