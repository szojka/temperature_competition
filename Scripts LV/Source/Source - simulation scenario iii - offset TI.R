#####################################
## SOURCE DATAFRAMES FOR ALL FIGURES 
#####################################

# SGH intra

######################################
# Set up working environment
######################################

# clear working directory
rm(list = ls())

# set directory string (makes compatable with cluster)
dir_string <- "/project/6087843/mszojka" # for cluster!

source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls - offset TI.R")) # loads replicate data too

source(paste0(dir_string,"/Scripts LV/Source/Source - functions.R") )

#############################################################
# Generate coexistence dataframes for each time period (era) 
#############################################################

# source performance curves
source(paste0(dir_string,"/Scripts LV/Intrinsic growth/r - offset.R") )# sources time series and function

# source alpha curves
source(paste0(dir_string,"/Scripts LV/Alphas/Alphas - SGH intra - offset TI.R"))

source(paste0(dir_string,"/Scripts LV/Source/Source - pull params together.R")) # issue could arise because of NAs in intrinsic growth code.


params <- data.frame()
k <- 1 # counter
for(i in list_mat){
  temp_params <- do.parameters(this_matrix = i, replicates = rep_length)
  temp_params$era <- era_temps[k]
  params <- rbind(params, temp_params)
  k <- k + 1
}
params$scenario <- scenario_names[3]

##################################################
# Add niche and fitness differences
##################################################

# LV p = sqrt((aij*aii)/(ajj*aji))
params$niche_d <- (1 - sqrt((params$aji * params$aij)/(params$ajj * params$aii)))

# LV kj/ki = sqrt((aij*aii)/(ajj*aji))
params$fit_d_kj <- sqrt((params$aij * params$aii)/(params$ajj * params$aji)) 

params$run <- as.factor(params$run)
params$scenario <- as.factor(params$scenario)
params$era <- as.factor(params$era)

#############################################################
# Categorize into coexistence vs exclusion vs extinction
#############################################################

#----------------------
# run function
#----------------------

df_cat <- do.categorize(df = params)

#------------------------------------------------
# data wrangling
#-----------------------------------------------

df_categories3 <- df_cat %>%
  pivot_longer(cols = all_of(category_cols),
               names_to = 'category', values_to = 'value') %>%
  filter(value != 0) %>%
  select(-value)

df_categories3$category <- as.factor(df_categories3$category) 
df_categories3$era <- factor(df_categories3$era,
                             levels = era_temps)
df_categories3$combo <- as.factor(df_categories3$combo)

#------------------------------------------------
# summarize this dataframe for plotting
#------------------------------------------------

df_cat_summarized3 <- df_categories3 %>%
  dplyr::group_by(scenario, era) %>%
  dplyr::mutate(total_rep = n()) %>% # env*rep_length
  dplyr::group_by(scenario, era, category) %>% 
  dplyr::mutate(count_cat = n()) %>%
  dplyr::mutate(prop = count_cat/total_rep) %>%
  dplyr::select(scenario, era, category, run, combo, count_cat, total_rep, prop)  %>%
  ungroup() %>%
  distinct() 
df_cat_summarized3$category <- as.factor(df_cat_summarized3$category)

################################################################################
# SAVE DATAFRAMES TO USE FOR FIGURES
################################################################################

save(df_categories3, file = paste0(dir_string, "/Scripts LV/Final dataframes/df_offset_categories3.Rdata"))
save(df_cat_summarized3, file = paste0(dir_string, "/Scripts LV/Final dataframes/df_offset_cat_summarized3.Rdata"))
