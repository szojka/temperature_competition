#########################################
# Set basic levels and objects
#########################################

# load libraries
library(scales)
library(stringr)
library(tidyverse)
library(deSolve)

# set sci notation 
options(scipen = 4)

################################
# for naming figures, loops, functions, list of names for each scenario and labels for eras and scenarios:

# alter the following according to scenarios we are testing:

scenario_names <- c("SGH intercepts", "Rstar intercepts", "SGH intra", "Rstar intra", "SGH inter", "Rstar inter", "SGH inter & intra", "Rstar inter & intra")

scenario_no <- 8 # usually 8 

species_names <- c('generalist', 'specialist')

scenario_list <- c("coex_data_list_i","coex_data_list_ii", "coex_data_list_iii", "coex_data_list_iv", "coex_data_list_v", "coex_data_list_vi",  "coex_data_list_vii", "coex_data_list_viii") # names of some lists that the pull together function needs:

suffix_list <- c("_h", "_c", paste0("_f", 1:10)) # names of some lists that the pull together function needs:

# name temperatures
era_temps <- c('historic', 'current', '0.5\u00B0C','1\u00B0C','1.5\u00B0C',
               '2\u00B0C','2.5\u00B0C','3\u00B0C',
               '3.5\u00B0C','4\u00B0C','4.5\u00B0C','5\u00B0C')

################################
# set number of iterations:

load(paste0(dir_string,"/Scripts LV/Final dataframes/replicate_dat.RData"))

rep_length <- length(replicate_dat[,1]) # 10,000

#############################################
# load combos needed for all alpha scripts:

load(paste0(dir_string,"/Scripts LV/Final dataframes/combos_dat.RData"))

# and constants to run alpha scripts:
alpha_intra_min <- 0.035 # choose this one, and work around it.
alpha_intra_max <- 1
# starting point for both specialist and generalist
alpha_min <- 0.01
# starting point for both specialist and generalist if facilitation is allowed:
facilitation_alpha_min <- -0.01


