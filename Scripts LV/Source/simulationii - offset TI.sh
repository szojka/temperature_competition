#!/bin/bash

#SBATCH --account=def-joeybern
#SBATCH --time=7-00:00
#SBATCH --mem=16G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=megan.szojka@case.edu
#SBATCH --job-name=CoexClimate_Simulation_ii_offset

### Load R
module load r/4.4.0

srun R --vanilla < 'Source - simulation scenario ii - offset TI.R' > CoexClimate_scenario_ii_output_offset.log