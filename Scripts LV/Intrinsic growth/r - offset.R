
######################################
# SET UP THERMAL PERFORMANCE CURVES
######################################

# this script uses the 'Source_timeseries_PRISM.R', and 'Source_functions.R'
source(paste0(dir_string,"/Scripts LV/Source/Source - timeseries PRISM.R"))
source(paste0(dir_string,"/Scripts LV/Source/Source - functions.R"))
source(paste0(dir_string, "/Scripts LV/Intrinsic growth/r - generalist v specialist.R"))

#######################################################
# for naming figures, list of names for each scenario
# used in all scripts

# using only growtin season months
temp_full <- seq(-10,30, by = 0.1) # fitting the growth curve off these full data: 

env_tmean_spring %>%
  # filter(year %in% c(1895:1950)) %>%
  summarize(max = max(tmean), min = min(tmean)) # 27.2 -2.79

##########################
# Species characteristics 
##########################

# Both species will be specialists with the same shape. One species will have an optimum 1 SD larger than the other.
# one species will have TI be 0.5 SD above historic mean, and the other will have a TI 1.5 SD above historic mean

#######################
# Calculate Topt
# 1 standard deviations above historic mean of Tmean
historic_sd <- sd(mat_historic) # 0.9910401
mean(mat_historic) # 11.46276

# sp i
TI_i <- historic_mean + 0.5*historic_sd # optimal consumption temperature
TI_i #11.98732

# sp j
TI_j <- historic_mean + 1.5*historic_sd # optimal consumption temperature
TI_j #12.97936


#########################
# Take the generalist specialist dataframe

full_rs <- full_rs %>%
  select(-rj)

# Slide the rj TPC to the left 0.5
ri_dat <- full_rs %>%
  select(temp_full, ri) %>%
  mutate(new_temp = temp_full - 0.5) %>%
  select(-temp_full)

# Slide the TPC to the right 0.5
rj_dat <- full_rs %>%
  select(temp_full, ri) %>%
  mutate(rj = ri) %>%
  select(-ri) %>%
  mutate(new_temp = temp_full + 0.5)%>%
  select(-temp_full)

full_rs_new <- full_join(ri_dat, rj_dat, by = 'new_temp')
full_rs_new[is.na(full_rs_new)] <- 0
# filter the correct temp_full range and rename to new_temp
full_rs <- full_rs_new %>%
  filter(new_temp %in% temp_full) %>%
  rename(temp_full = new_temp) %>%
  arrange(temp_full)
  
# plot
plot(full_rs$temp_full, full_rs$ri, type = "l", col = "red", xlab = "Mean temp", ylab = "r")
lines(full_rs$temp_full, full_rs$rj, type = "l", col = "green",xlim = c(0, 100)) # spp j
abline(v = TI, lty = "dashed") # good

# sp j min and max = 
min_spj <- min(full_rs$temp_full[full_rs$rj > 0]) 
max_spj <- max(full_rs$temp_full[full_rs$rj > 0]) 
# corresponds to 2.5 degree warming

# sp i min and max = 
min_spi <- min(full_rs$temp_full[full_rs$ri > 0]) 
max_spi <- max(full_rs$temp_full[full_rs$ri > 0]) 
# corresponds to 3.5 degree warming
