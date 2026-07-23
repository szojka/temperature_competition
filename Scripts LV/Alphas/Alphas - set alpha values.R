#########################################
# Set alpha values
#########################################

#-------------------------------------
# Explanation of what this script is doing:
# using a random uniform distrubtion, we selected the specialists' maximum interspecific strength, which was allowed to vary from 0.035-0.02 to 0.035+0.02. This was done to maximize opportunities for coexistence (intrasepcific commonly exceeds interspecific strengths), as one of our model assumptions is that the specialist and generalist coexist under historical conditions.
# Once we had pulled 1000 random values for the specialist's maximum interspecific strength, we used a random uniform distribution to determine the generalist's maximum interspecific strength. To ensure the generalist was always less competitive than the specialist, we constrained this sampling to be greater than the specialist's maximum interspecific value for a given replicate multiplied by 0.9 and - 0.02, and less than the specialist's maximum interspecific value multiplied by 0.9. For any situation that sampled a negative value of a species' interspecific maximum, we changed these values to a small positive number 0.015.
#-------------------------------------

# Basically so that nothing is hard coded

set.seed(222) # same alpha runif distribution everytime.

#----------------------------------
# my arbitrarily chosen alphas

alpha_intra_min <- 0.035 # choose this one, and work around it.
alpha_intra_max <- 1

# starting point for both specialist and generalist
alpha_min <- 0.01

# starting point for both specialist and generalist if facilitation is allowed:
facilitation_alpha_min <- -0.01

# constant for SGH intercept only
# sampled along a the piecewise alpha function of Script SGH only:
# pull constant value at env = round(historic_mean,1) (11.4913)

# constant for Rstar intercept only
# sampled along a the piecewise alpha function of Script Rstar only:
# pull constant value at env = round(historic_mean,1) (11.4913)


#-------------------------------------------------------------------------
# Now determine combos of specialist and generalist alphas to sample over:

# to determine min and max for the runif function, the mid line of alpha, 
# right now alpha is 0.035 and inter_i is 0.045 and inter_j is 0.04

# 0.04-0.035 # 0.005
# 0.045-0.035 # 0.01
# 
# 0.04/0.045 # 88% 

# max should be 0.02 above intra
# min should be 0.02 below intra

# make loop, so that like environmental conditions, you are saving 100 combinations:

# i is specialist
alpha_max_i <- runif(n = 1000, min = alpha_intra_min-0.02, max = alpha_intra_min+0.01) # coex when intra > inter, need to leave opportunity for that

# prepare dataframe for looping
combos <- data.frame(alpha_max_i = alpha_max_i)
combos$combo <- NA
combos$combo <- rownames(combos)
combos$alpha_max_j <- NA

# j is generalist
for(i in 1:length(alpha_max_i)) {
# generalist maximum is 90% of specialists maximum, can be much smaller
  combos$alpha_max_j[i] <- runif(n = 1, min = (combos$alpha_max_i[i]*0.9)-0.02, max = combos$alpha_max_i[i]*0.9) # NaN when max smaller than min, so constraining min based on specialist as well.
}

# for situations that we sample a negative, change to small positive number:
combos$alpha_max_j[combos$alpha_max_j < 0.015] <- 0.015

head(combos)
names(combos)

#---------------------------------------------------------------------------------
# these various combinations must meet the criteria of most historic coexistence:

check_combos <- combos

# calculate fitness and niche differences
check_combos$niche_d <- (1 - sqrt((check_combos$alpha_max_i * check_combos$alpha_max_j)/(alpha_intra_min * alpha_intra_min)))
check_combos$fit_d_kj <- sqrt((check_combos$alpha_max_j * alpha_intra_min)/(alpha_intra_min * check_combos$alpha_max_i)) 
check_combos$niche_d <- round(check_combos$niche_d, 3)
check_combos$fit_d_kj <- round(check_combos$fit_d_kj, 3)

niche_diff <- seq(from = -.25, to = 1, by = 0.001) # this equals 1-rho, i.e., x axis
rho <- 1-niche_diff
rho # fitness_ratio_min
1/rho # fitness_ratio_max

df_condition <- data.frame(niche_diff = niche_diff, # = SD, x-axis, 1-rho
                           rho = rho, # rho
                           one_over_rho = 1/rho) # 1/rho


check_combos$coex <- NA
# i <- 2
# check coexistence:
for(i in 1:length(check_combos$niche_d)){ 
  
  # allows finding of rho and 1/rho for specific niche_diff value of this row:
  temp <- dplyr::filter(df_condition, niche_diff %in% as.character(check_combos$niche_d[i])) 
  
  # condition for coexistence:
ifelse(as.numeric(check_combos$fit_d_kj[i]) >= as.numeric(temp$rho) & as.numeric(check_combos$fit_d_kj[i]) <= as.numeric(temp$one_over_rho), check_combos$coex[i] <- 1, check_combos$coex[i] <- 0)

} 

# percentage coexistence from these constants:
check_combos %>%
  na.omit() %>%
  mutate(n_coex = sum(coex)) %>%
  mutate(perc_coex = n_coex/1000) %>%
  select(perc_coex) %>%
  distinct() # 0.67 coexistence! Proceed with this and see what happens for all parameters.

#------------------------------------------------------------------------
# Set up data so I can loop through alpha combos in my alpha scripts:

combos_dat <- check_combos %>%
  na.omit() %>%
  select(combo, alpha_max_i, alpha_max_j)

save(combos_dat, file = paste0(getwd(), "/Scripts LV/Final dataframes/combos_dat.RData"))


