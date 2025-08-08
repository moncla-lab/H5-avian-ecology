#!/bin/bash
#BSUB -W 840:00 # runtime of 35 days 
#BSUB -J seraphim_summarystats_run # job name
#BSUB -o seraphim_summarystats_run.out
#BSUB -e seraphim_summarystats_run.err
#BSUB -M 20240
#BSUB -N
#BSUB -u clara.malekshahi@pennmedicine.upenn.edu    # sends email upon job completion

module load anaconda/3 proj/8.2.1 R/4.3 gdal/3.10.3

conda activate seraphim

Rscript estimate-statiastics.txt