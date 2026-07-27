#####################################
## SOURCE DATAFRAMES FOR ALL FIGURES 
#####################################

# Rstar intra

######################################
# Set up working environment
######################################

# clear working directory
rm(list = ls())

# set directory string (makes compatable with cluster)
dir_string <- "/project/6087843/mszojka" # for cluster!

source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R")) # loads replicate data too

source(paste0(dir_string,"/Scripts LV/Source/Source - functions.R") )

#############################################################
# Generate coexistence dataframes for each time period (era) 
#############################################################

# source performance curves
source(paste0(dir_string,"/Scripts LV/Intrinsic growth/r - generalist v specialist.R")) # sources time series and function

# source alpha curves
source(paste0(dir_string,"/Scripts LV/Alphas/Alphas - Rstar intra.R"))

source(paste0(dir_string,"/Scripts LV/Source/Source - pull params together.R"))


params <- data.frame()
k <- 1 # counter
for(i in list_mat){
  temp_params <- do.parameters(this_matrix = i, replicates = rep_length)
  temp_params$era <- era_temps[k]
  params <- rbind(params, temp_params)
  k <- k + 1
}
params$scenario <- scenario_names[4]

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

df_categories4 <- df_cat %>%
  pivot_longer(cols = all_of(category_cols),
               names_to = 'category', values_to = 'value') %>%
  filter(value != 0) %>%
  select(-value)

df_categories4$category <- as.factor(df_categories4$category) 
df_categories4$era <- factor(df_categories4$era,
                             levels = era_temps)
df_categories4$combo <- as.factor(df_categories4$combo)

#------------------------------------------------
# summarize this dataframe for plotting
#------------------------------------------------

df_cat_summarized4 <- df_categories4 %>%
  dplyr::group_by(scenario, era) %>%
  dplyr::mutate(total_rep = n()) %>% # env*rep_length
  dplyr::group_by(scenario, era, category) %>% 
  dplyr::mutate(count_cat = n()) %>%
  dplyr::mutate(prop = count_cat/total_rep) %>%
  dplyr::select(scenario, era, category, run, combo, count_cat, total_rep, prop)  %>%
  ungroup() %>%
  distinct() 
df_cat_summarized4$category <- as.factor(df_cat_summarized4$category)

################################################################################
# SAVE DATAFRAMES TO USE FOR FIGURES
################################################################################

save(df_categories4, file = paste0(dir_string, "/Scripts LV/Final dataframes/df_categories4.Rdata"))
save(df_cat_summarized4, file = paste0(dir_string, "/Scripts LV/Final dataframes/df_cat_summarized4.Rdata"))
