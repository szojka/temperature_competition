
######################################
# SET UP THERMAL PERFORMANCE CURVES
######################################

# library(tidyverse)

# this script uses the 'Source_timeseries_PRISM.R', and 'Source_functions.R'
source(paste0(dir_string,"/Scripts LV/Source/Source - timeseries PRISM.R"))
source(paste0(dir_string,"/Scripts LV/Source/Source - functions.R"))

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

# One species will have narrower breadth (specialist) with higher growth at Topt and one wider (generalist) with the same Topt but smaller growth
# I made the generalist's (spp. j) breadth wider by increasing beta rates, then rescaling based on desired Tmax's further down.
# The generalist produces 0.6 the amount of seeds near its optimum as the specialist.

#######################
# Calculate Topt
# 1 standard deviations above historic mean of Tmean
historic_sd <- sd(mat_historic) # 0.9910401
mean(mat_historic) # 11.46276

# Topt should be slightly higher than the mean conditions of occupied range (approximated here by mean(mat_historic))
# adding 1 sd because then Topt
TI <- historic_mean + historic_sd # optimal consumption temperature
TI #12.4538

#############
# PARAMETERS
# metabolics
ma <- 0.03 # # increasing ma increases y values the fxn expands over
mbi <- 0.02 # metabolics changing this one changes the shape of acceleration (smaller = slower), best for specialist
mbj <- 0.1 # increasing mb pushes inflection point of exp into higher x values
mc <- 0.02 # making mc smaller reduces intercept
# parameters to vary = beta has to do with breadth of Imax response. 
betaj <- 100 # was 250, experimenting with greater breadth
betai <- 50
beta <- 150
delta <- 0.5

####################################
# Maximum uptakes rates can change between species

# maximum uptake rate for species j
Ij <- rep(NA, times = length(temp_full)) 
Ij[1] <- 0

for (t in 1:length(temp_full)) {
  # parameters stay the same as outlined above
  #print(temp_full[t])
  Ij[t + 1] <- Imax(
    TI = TI,
    beta = betaj,
    Temp = temp_full[t])
}
Ij <- Ij[1:length(Ij)-1]
plot(temp_full, Ij,  type = "l",col = "blue")

# maximum uptake rate for species i
Ii <- rep(NA, times = length(temp_full)) # -1 bc form N[t+1] creates extra entry at the end equalling 40
Ii[1] <- 0

for (t in 1:length(temp_full)) {
  # parameters stay the same as outlined above
  Ii[t + 1] <- Imax(
    TI = TI, 
    beta = betai, 
    Temp = temp_full[t])
  
}
Ii <- Ii[1:length(Ii)-1]
plot(temp_full, Ii, type = "l", col = "blue")

####################################
# Respiration rate change Tmean between species:

# the skews on this could be greater. Struggling with shortening the function.
# however, by using the rescaling script further down, we solve this issue.
# generalist has faster acceleration to cut to thermal max at higher temps
Mj <- rep(NA, times = length(temp_full))
Mj[1] <- 0

for (t in 1:length(temp_full)) {
  # parameters stay the same as outlined above
  Mj[t + 1] <- m(
    ma = ma, 
    mb = mbj, 
    mc = mc,
    Temp = temp_full[t])
}
Mj <- Mj[1:length(Mj)-1]
plot(temp_full, Mj, type = "l", col = "orange")

########################################
# Calculate growth rates (TPC equation)

rj <- c(Ij*delta-Mj) # wider breadth
rj[rj < 0] <- 0 # negative persistence to one (persistence threshold) so rescale isn't conflated
ri <- c(Ii*delta-Mj) # narrower breadth
ri[ri < 0] <- 0 # negative persistence to one (persistence threshold) so rescale isn't conflated

# data frame with temperature values so I can filter later
full_rs <- data.frame(rj = rj, 
                           ri = ri,
                           temp_full = temp_full)

####################################
# Rescale these r values so that it represents max seed production

full_rs$rj <- full_rs$rj*80
full_rs$ri <- full_rs$ri*90  # specialist needs to have higher seed output 

####################################
# plot species scaled rs to see differences in breadth
plot(full_rs$temp_full, full_rs$ri, type = "l", col = "red", xlab = "Mean temp", ylab = "r")
lines(full_rs$temp_full, full_rs$rj, type = "l", col = "green",xlim = c(0, 100)) # spp j
abline(v = TI, lty = "dashed")

#######################################
# Topt is off, slide over curves to align peak of curve with TI

TI # 12.46276 same for both species
# current max for species specialist:
apex_si <- full_rs %>% filter(., ri == max(ri)) %>% distinct() %>% dplyr::select(temp_full) # 12.1
# current max for species generalist:
apex_sj <- full_rs %>% filter(., rj == max(rj)) %>% distinct() %>% dplyr::select(temp_full) # 11.6

# move sp. j (generalist) over TI-apex_sj x units away
units_j <- round(TI-apex_sj,1)
# same for sp.i (specialist)
units_i <- round(TI-apex_si,1)

# now slide over the curves by adding the units to the temperatures 
temp1 <- full_rs %>% 
  dplyr::select(rj, temp_full) %>%
  dplyr::mutate(temp_full = temp_full + as.numeric(units_j)) %>%
  dplyr::mutate(temp_full = round(temp_full,1))
plot(temp1$temp_full, temp1$rj, type = 'l')
abline(v = TI, lty = "dashed")

temp2 <- full_rs %>% 
  dplyr::select(ri, temp_full) %>%
  dplyr::mutate(temp_full = temp_full + as.numeric(units_i)) %>%
  dplyr::mutate(temp_full = round(temp_full,1))
plot(temp2$temp_full, temp2$ri, type = 'l')
abline(v = TI, lty = "dashed")

# join back together rs for each species
temp_rs <- full_join(temp1,temp2, by = 'temp_full')
# NAs are where there are some temps for one species but not the other so turn these to 0
temp_rs[is.na(temp_rs)] <- 0

# fill in the gaps on the cold end and cut off the extra 0s on the hot end to keep the range bw -10 and 30

# define a filler for tail ends of temperature range
# figure out what you are missing from the bottom
diff_low <- min(temp_rs$temp_full) # -9.6
seq_low <- c(seq(from = -10.0, to = diff_low, by = 0.1))
filler <- data.frame(rj = c(rep(0, times = length(seq_low))), 
                     ri = c(rep(0, times = length(seq_low))), 
                     temp_full = seq_low)

temp_rs <- rbind(temp_rs, filler)

# for cutting off high values make sure dataframe is organized low to high temps
temp_rs <- temp_rs %>% 
  arrange(temp_full)
# figure out what temps you are missing from the max
diff_high <- max(temp_rs$temp_full) # 30.9
cut_high <- length(seq(from = 0.1, to = (diff_high - 30.0), by = 0.1)) # 30 is my maxium temp range, so remove cut_high no. of entries from the top
full_rs <- temp_rs %>%
  filter(row_number() <= n()-cut_high) 
max(full_rs$temp_full) # check this should be 30.0

# plot to be sure the shift in temperatures is doing what you want
plot(full_rs$temp_full, full_rs$ri, type = "l", col = "red", xlab = "Mean temp", ylab = "r")
lines(full_rs$temp_full, full_rs$rj, type = "l", col = "green",xlim = c(0, 100)) # spp j
abline(v = TI, lty = "dashed") # good

#####################################
# Rescale min and max of each niche 
# breadth to fall systematically away from historic mean

# notes on niche breadth:

# changing delta also changes breadth, but max r gets smaller with niche breadth, opposite of what I want.
# max(temp) experienced in generalist TPC should be about mean of future (ii) - ends up being true, see parameter figure

historic_mean # 11.46276
max(mat_historic) # 14.1572
hist(mat_historic)

current_mean # 11.56295
max(mat_current) # 18.11959
hist(mat_current)

future1_mean # 13.46276
max(mat_future1) # 19.18126
hist(mat_future1)

future2_mean # 14.96276
max(mat_future2) # 22.58113
hist(mat_future2)

future3_mean # 16.46276
max(mat_future3) # 26.18965
hist(mat_future3)

#####################################
# Generalist should have Tmax at 3 historic_sd above historic_mean
sj_max <- historic_sd*4 + historic_mean # max goal 
# shrink min proportionally:
# difference bw desired max and current max = 
sj_diff <- max(full_rs$temp_full[full_rs$rj > 0]) - sj_max 
sj_min <- min(full_rs$temp_full[full_rs$rj > 0]) + sj_diff

# current
min(full_rs$temp_full[full_rs$rj > 0]) 
max(full_rs$temp_full[full_rs$rj > 0]) 

#####################################
# Specialist: should have Tmax 2 historic_sd above historic_mean
si_max <- historic_sd*3 + historic_mean # max goal 
# shrink min proportionally:
# difference bw desired max and current max = 
si_diff <- max(full_rs$temp_full[full_rs$ri > 0]) - si_max
si_min <- min(full_rs$temp_full[full_rs$ri > 0]) + si_diff 

# current:
min(full_rs$temp_full[full_rs$ri > 0]) 
max(full_rs$temp_full[full_rs$ri > 0]) 

#####################################
# source script that will change each species min and max based on values you defined above:
source(paste0(dir_string,"/Scripts LV/Intrinsic growth/r - nonsymmetrical rescaling.R"))

##########################################
# within niches, species r must be > 0
# outside of niches these rs can be capped at 0 (no negative numbers)

# Instead, find true niche breadth for each species and use that to filter 'persistence area' for coexistence graphs
# sp j min and max = 
min_spj <- min(full_rs$temp_full[full_rs$rj > 0]) 
max_spj <- max(full_rs$temp_full[full_rs$rj > 0]) 

# sp i min and max = 
min_spi <- min(full_rs$temp_full[full_rs$ri > 0]) 
max_spi <- max(full_rs$temp_full[full_rs$ri > 0]) 

#view(full_rs) 
# final plotting to see effects of rescaling:
plot(full_rs$temp_full, full_rs$ri, type = "l", col = "red", xlab = "Mean temp", ylab = "r")
lines(full_rs$temp_full, full_rs$rj, type = "l", col = "green",xlim = c(0, 100)) # spp j
abline(v = TI, lty = "dashed") # good

##########################################
# What r value at Topt for both species?
max(full_rs$ri[full_rs$temp_full == 12.5]) # 34.0
max(full_rs$rj[full_rs$temp_full == 12.5]) # 30.4



