#!/bin/bash

#SBATCH --account=def-joeybern
#SBATCH --time=7-00:00
#SBATCH --mem=16G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=megan.szojka@case.edu
#SBATCH --job-name=CoexClimate_Simulation_v_offset

### Load R
module load r/4.4.0

srun R --vanilla < 'Source - simulation scenario v - offset TI.R' > CoexClimate_scenario_v_output_offset.log