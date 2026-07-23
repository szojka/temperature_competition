#############################################################
# Set up data to save into a dataframe with parameters
# for all scenarios
#############################################################

######################################
# Set up working environment
######################################

# clear working directory
rm(list = ls())

# set directory string (makes compatable with cluster)
dir_string <- getwd()
#dir_string <- "/cluster/medbow/project/coexistence/mszojka" # for cluster!

# for naming figures, list of names for each scenario
source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R"))

# define persistence positive zone:
source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - offset.R"))

#-----------------------------------------------------------------------
# Load data from all 8 simulations
#-----------------------------------------------------------------------

for (i in 1:8) {
  # call file name and load it
  load(paste0(dir_string, "/Scripts LV/Final dataframes/df_offset_categories",i,".Rdata"))
}

# need a list of matrices to loop through
list_dat <- list(df_categories1, df_categories2, df_categories3,  df_categories4, df_categories5, df_categories6, df_categories7, df_categories8)

list_dat_names <- c('df_categories1', 'df_categories2', 'df_categories3',  'df_categories4', 'df_categories5', 'df_categories6', 'df_categories7', 'df_categories8')

names(list_dat) <- list_dat_names

#---------------------------
# Prepare data for plotting
#---------------------------

##########################
# Change names of facets:

full_dat <- rbind(df_categories1, df_categories2, df_categories3,  df_categories4, df_categories5, df_categories6, df_categories7, df_categories8)

full_dat <- full_dat %>%
  mutate(era = fct_recode(era, 
                          '0.5°C' = '0.5 C',
                          '1°C' = '1 C',
                          '1.5°C' = '1.5 C',
                          '2°C' = '2 C',
                          '2.5°C' = '2.5 C',
                          '3°C' = '3 C',
                          '3.5°C' = '3.5 C',
                          '4°C' = '4 C',
                          '4.5°C' = '4.5 C',
                          '5°C' = '5 C'))
# order the eras:
full_dat$era <- factor(full_dat$era, 
                       levels = c('historic', 'current', '0.5°C','1°C','1.5°C',
                                  '2°C','2.5°C','3°C',
                                  '3.5°C','4°C','4.5°C','5°C'))

parameter_dat_offset <- full_dat
save(parameter_dat_offset, file = here::here("Scripts LV/Final dataframes/parameter_dat_offset.Rdata"))
