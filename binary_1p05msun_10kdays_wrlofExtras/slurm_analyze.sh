#!/bin/bash
#SBATCH --job-name bdbwe
#SBATCH -n 1 
#SBATCH -t 3-23:50
#SBATCH -p genx -c 1
#SBATCH --mem=11780
#SBATCH --mail-type=ALL
#SBATCH --mail-user=johncforbes@gmail.com


cd ${HOME}/ceph/binary_mesa_15/binary_1p05msun_10kdays_wrlofExtras/
./mk
./rn

