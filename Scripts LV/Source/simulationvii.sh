#!/bin/bash

#SBATCH --account=def-joeybern
#SBATCH --time=7-00:00
#SBATCH --mem=16G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=mszojka@uoguelph.ca
#SBATCH --job-name=CoexClimate_Simulation_vii

### Load R
module load r/4.4.0

srun R --vanilla < 'Source - simulation scenario vii.R' > CoexClimate_scenario_vii_output.log