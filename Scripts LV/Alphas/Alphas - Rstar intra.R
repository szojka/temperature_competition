######################################
# SET UP ALPHA CURVES
######################################

# Motivate changing interspecific alpha by theory: 
# (i) none, (ii) R*, (iii) stress-gradient. 
# Motivate intraspecific alpha by K (see Sunday paper)

temp_full <- seq(-10,30, by = 0.1) # fitting the growth curve off these full data: 
temp_full <-  data.frame(temp_full = temp_full) 

#################
# Inter-specific
#################

#############
# no change

# combos_dat


#################
# Intra-specific
#################

###############################################################
# Piecewise function so that accelerates and declerates slower,
# but also flattens out for majority of curve

# spefify these mean and maxes as the bounds of the normal curve
minTj <- min(full_rs$temp_full[full_rs$rj > 0])
maxTj <- max(full_rs$temp_full[full_rs$rj > 0])

minTi <- min(full_rs$temp_full[full_rs$ri > 0])
maxTi <- max(full_rs$temp_full[full_rs$ri > 0])

# min plus a few degrees and max minus a few degress, use logistic acceleration function already made for R*

#################################
# spp j: generalist

# minimum
xj_min <- seq(minTj, minTj+1, by = 0.1) # minimum and 1 degree higher
yj_min <- logistic(x = xj_min, k = exp(1))*-1 # to make it plateau on low value, using k = 'e' so its symmetrical to maximum increase exp()
plot(xj_min,yj_min)
# maximum
xj_max <- seq(maxTj-1, maxTj, by = 0.1) # maximum and 1 degree lower
yj_max <- exp(xj_max) 
# rescale
yj_min <- scales::rescale(yj_min, to = c(alpha_intra_min,alpha_intra_max))
yj_max <- scales::rescale(yj_max, to = c(alpha_intra_min,alpha_intra_max))
plot(xj_max,yj_max)
plot(xj_min,yj_min)
# make middle plateau
xj_mid <- seq(from = (max(xj_min)+0.1), to = (min(xj_max)-0.1), by = 0.1)
yj_mid <- rep(alpha_intra_min, times = length(xj_mid))
# bind pieces together
ajj_temp <- data.frame(ajj = c(yj_min,yj_mid,yj_max),
                       temp_full = c(xj_min,xj_mid,xj_max))
ajj_temp$temp_full <- as.factor(ajj_temp$temp_full) # changing class so the join works
temp_full$temp_full <- as.factor(temp_full$temp_full)
# join with temp_full df so and fill empty ajj with 1
ajj_df <- left_join(temp_full, ajj_temp, by = "temp_full")
ajj_df[is.na(ajj_df)] <- alpha_intra_max

###########################
# spp i: specialist

# minimum
xi_min <- seq(minTi, minTi+1, by = 0.1) # minimum and 1 degree higher
yi_min <- logistic(x = xi_min, k = exp(1))*-1 # to make it plateau on low value,  using k = 'e' so its symmetrical to maximum increase exp()
plot(xi_min,yi_min)
# maximum
xi_max <- seq(maxTi-1, maxTi, by = 0.1) # maximum and 1 degree lower
yi_max <- exp(xj_max) 
plot(xi_max,yi_max)
# rescale
yi_min <- scales::rescale(yi_min, to = c(alpha_intra_min,alpha_intra_max))
yi_max <- scales::rescale(yi_max, to = c(alpha_intra_min,alpha_intra_max))
plot(xi_max,yi_max)
plot(xi_min,yi_min)
# make middle plateau
xi_mid <- seq(from = (max(xi_min)+0.1), to = (min(xi_max)-0.1), by = 0.1)
yi_mid <- rep(alpha_intra_min, times = length(xi_mid))
# bind pieces together
aii_temp <- data.frame(aii = c(yi_min,yi_mid,yi_max),
                       temp_full = c(xi_min,xi_mid,xi_max))
aii_temp$temp_full <- as.factor(aii_temp$temp_full) # changing class so the join works
temp_full$temp_full <- as.factor(temp_full$temp_full)
# ioin with temp_full df so and fill empty aii with 1
aii_df <- left_join(temp_full, aii_temp, by = "temp_full")
aii_df[is.na(aii_df)] <- alpha_intra_max

K_intra <- full_join(aii_df,ajj_df, by = 'temp_full')

#######################
# Pull into dataframe
#######################

K_intra # needs to repeat all combo levels
combos_dat # needs to repeat length(temp_full) for all combo levels

full_alphas <- data.frame(
  temp_full = rep(K_intra$temp_full, times = length(unique(combos_dat$combo))),
  ajj = rep(K_intra$ajj, times = length(unique(combos_dat$combo))),
  aii = rep(K_intra$aii, times = length(unique(combos_dat$combo))),
  combo = rep(1:length(unique(combos_dat$combo)), each = length(K_intra$aii)),
  aij = rep(combos_dat$alpha_max_j, each = length(K_intra$aii)),
  aji = rep(combos_dat$alpha_max_i, each = length(K_intra$aii))
)

full_alphas$temp_full <- as.numeric(as.character(full_alphas$temp_full))
full_alphas$combo <- as.factor(full_alphas$combo)

# need to match decimal places with model
full_alphas$temp_full <- round(full_alphas$temp_full,1)

