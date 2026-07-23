
#########################################################################################################
# Only have to run this script once.
# Aim is to pull together the data from simulations into proportions our outcomes by scenario and theory
# This script then saves this dataframe to use as proportion_dat.rdata file
#########################################################################################################

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
source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - generalist v specialist.R"))

#-----------------------------------------------------------------------
# Load data from all 8 simulations
#-----------------------------------------------------------------------

for (i in 1:8) {
  # call file name and load it
  load(paste0(dir_string, "/Scripts LV/Final dataframes/df_cat_summarized",i,".Rdata"))
}

# need a list of matrices to loop through
list_dat <- list(df_cat_summarized1, df_cat_summarized2, df_cat_summarized3,  df_cat_summarized4, df_cat_summarized5, df_cat_summarized6, df_cat_summarized7, df_cat_summarized8)

list_dat_names <- c('df_cat_summarized1', 'df_cat_summarized2', 'df_cat_summarized3',  'df_cat_summarized4', 'df_cat_summarized5', 'df_cat_summarized6', 'df_cat_summarized7', 'df_cat_summarized8')

names(list_dat) <- list_dat_names

#---------------------------
# Prepare data for plotting
#---------------------------

##########################
# Change names of facets:

full_dat <- rbind(df_cat_summarized1, df_cat_summarized2, df_cat_summarized3,  df_cat_summarized4, df_cat_summarized5, df_cat_summarized6, df_cat_summarized7, df_cat_summarized8)

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

######################################
# Summary stats for General patterns:
full_dat %>%
  select(-combo, -run, -count_cat, -total_rep) %>%
  distinct() %>%
  group_by(scenario, era) %>%
  #filter(era %in% 'historic' & category %in% 'coexist')
  # 1 SGH intercepts      historic coexist  0.907
  # 2 Rstar intercepts    historic coexist  0.692
  # EXTINCTION
  #filter(era %in% '2 C' & category %in% 'specialist extinct') # specialist Tmax
  # across all scenarios 11.6% extinct at 2 warming, which is thermal limit for specialist
  # filter(era %in% '1.5 C' & category %in% 'specialist extinct') # compared to 1.5 (timeperiod cooler)
  # 5.5% across all scenarios of 1.5 warming
  # filter(era %in% '3 C' & category %in% 'both spp extinct') # generalist Tmax
  # approximately 14% across scenarios for generalist thermal max
  # filter(era %in% '2.5 C' & category %in% 'both spp extinct') # timeperiod before generalist tmax
  # about 6.3 % across scenarios
  # PRIORITY EFFECTS
  #filter(category %in% 'priority effect') %>%
  #ungroup() %>%
  #summarize(minimum = min(prop), maximum = max(prop))
  # 0.0000159  0.0241
  # COMPETETIVE EXCLUSION
  # filter(category %in% c('generalist excluded (-)','generalist excluded (+)')) %>% # average generalist exlucsion
  # ungroup() %>%
  # summarise(average = mean(prop)) # 4.6\% across eras and runs
  filter(category %in% c('specialist excluded (-)','specialist excluded (+)')) %>% # average specialist exlucsion
  ungroup() %>%
  summarise(average = mean(prop)) # 0.25\% across eras and runs
#4.6/0.25

#####################################
# Add scenario names and theory names columns:
# scenario names: constant, `interspecific \n temperature-dependent`, `intraspecific \n temperature-dependent`, `both \n temperature-dependent`
# theory names: SGH, R*

full_dat2 <- as.data.frame(full_dat)
full_dat2$scenario <- as.factor(full_dat2$scenario)

full_dat_plot <- full_dat2 %>%
  mutate(theory = case_when(scenario %in% c('SGH intercepts', 'SGH intra', 'SGH inter','SGH inter & intra') ~ 'Gradual',
                            scenario %in% c('Rstar intercepts', 'Rstar intra', 'Rstar inter','Rstar inter & intra') ~ 'Abrupt')) %>%
  mutate(comp_scenario = case_when(scenario %in% c('Rstar intercepts', 'SGH intercepts') ~ 'constant',
                                   scenario %in% c('Rstar intra', 'SGH intra') ~ "intraspecific \n temperature-dependent",
                                   scenario %in% c('Rstar inter', 'SGH inter') ~ "interspecific \n temperature-dependent", 
                                   scenario %in% c('Rstar inter & intra', 'SGH inter & intra') ~ "both \n temperature-dependent"))

# remove replicate entries as it confuses geom_col()
full_dat_plot <- full_dat_plot %>%
  select(-combo, -run, -total_rep, -count_cat) %>%
  distinct()

# order the comp_scenarios
full_dat_plot$comp_scenario <- factor(full_dat_plot$comp_scenario, levels = 
                                        c('constant', 
                                          "interspecific \n temperature-dependent",
                                          "intraspecific \n temperature-dependent",
                                          "both \n temperature-dependent"))
full_dat_plot$colors <- NA

full_dat_plot$colors[full_dat_plot$category ==  "coexist"] <- "lightblue"
full_dat_plot$colors[full_dat_plot$category ==  "specialist excluded (+)"] <- 'limegreen'
full_dat_plot$colors[full_dat_plot$category ==  "specialist excluded (-)"] <- 'limegreen'
full_dat_plot$colors[full_dat_plot$category == "generalist excluded (+)"] <- "magenta3"
full_dat_plot$colors[full_dat_plot$category == "generalist excluded (-)"] <- "magenta3"
full_dat_plot$colors[full_dat_plot$category == "specialist extinct"] <- 'mediumpurple1'
full_dat_plot$colors[full_dat_plot$category == "both spp extinct"]  <-'purple'
full_dat_plot$colors[full_dat_plot$category == "priority effect"] <- "turquoise"
full_dat_plot$colors <- as.factor(full_dat_plot$colors)

full_dat_plot$colors <- droplevels(full_dat_plot$colors) # to keep colors what is filtered

# arrange data so I can look for any quick issues
full_dat_plot <- full_dat_plot %>%
  arrange(scenario, era, category)

# now go to the figure of choice and load:

proportion_dat <- full_dat_plot
save(proportion_dat, file = here::here("Scripts LV/Final dataframes/proportion_dat.Rdata"))


