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

#########
# R*

#"The minimum resource requirement for growth,R*—an inverse indicator of competitive ability"
# no skew, symmetrical
# inversse of R star = highly competition within benign temps, small comp when at edges of suitability
# U shape
# Sunday et al 2023: alpha coeffs are cold shifted compared to growth rate, U shapes are symetrical

# Comparing species:
# trade off with speialistc or generalist alphas vs r
# start with specialist more competitive and narrower like TPC, U shapes are symmetrical, generalist wider but less competitive at peak.

# max for specialist = 0.3
# max for generalist = 0.2
# min is 0 @ TPC minimum for both species
# max temp that competitive ability is high is 1/2 between Topt and max(TPC) [visual analysis of Sunday Fig 3]
# slight curve on cold side, 90 degree corner at heat side

Topt <- TI

temp_full <- as.data.frame(temp_full)
temp_full$temp_full <- as.numeric(temp_full$temp_full) 
temp_full$temp_full <- as.character(round(temp_full$temp_full,1)) # if i don't round we get issues with length of temps vs lengths of alphas

min_tpcj <- min(full_rs$temp_full[full_rs$rj > 0]) # generalist
min_tpci <- min(full_rs$temp_full[full_rs$ri > 0]) # specialist

max_tpcj <- max(full_rs$temp_full[full_rs$rj > 0]) # generalist
max_tpci <-max(full_rs$temp_full[full_rs$ri > 0]) # specialist

# parameters for determining cold bias
coldbiasj <- round(max_tpcj - ((max_tpcj-Topt)/2),1)
coldbiasi <- round(max_tpci - ((max_tpci-Topt)/2),1)

##############################################
# aji guidelines for specialist on generalist
# exerts higher competition but narrower set of ranges
# filter below min(t) aji = 0.01
# filter (max(t)-Topt)/2  aji= 0.01
# filter between min(t) and (max(t)-Topt)/2  aji = 0.2

Rstar_interji <- data.frame()

for(i in 1:length(combos_dat$combo)){
  
below <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(-10.0, min_tpci, by = 0.1),1))) %>% 
  mutate(aji = 0.0) %>% 
  mutate(combo = i) 

mid <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(min_tpci+0.1, coldbiasi, by = 0.1),1))) %>% 
  mutate(aji = 0.1)%>% 
  mutate(combo = i) 

# Smooth the increase of mid on cold side
# logistic, a=1,B=1,k=4
x <- as.numeric(temp_full$temp_full) # use full data range so I can select the elements I need.
y <- logistic(x=x)
#plot(x, y) # great!!!
#round(y,4) # things start to pick up around the 85th element

# use Y as a scaler (note min will be different)
# find elements I need for mid...
mid # min temp = min_tpci+0.1 = , max temp coldbiasi = 
seriesM <- length(seq(min_tpci+0.1, coldbiasi, by = 0.1)) # 135

#select seriesB+1:seriesL
y1 <- y[85:(84+seriesM)] # selects a chunk of the data the same length as mid

# check
seriesY <- length(y1)
seriesY == seriesM # TRUE

# scale and add constant 
mid$aji <- (mid$aji*y1) 
#plot(mid$temp_full,mid$aji) #decline starts at exactly the right place :) 

above <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(coldbiasi+0.1, 30.0, by = 0.1),1))) %>% 
  mutate(aji = 0.0) %>% 
  mutate(combo = i) 

this_iteration <- rbind(below,mid,above)
this_iteration$aji <- scales::rescale(this_iteration$aji, to = c(alpha_min, combos_dat$alpha_max_i[i])) # scale y values to desired min and max

Rstar_interji <- rbind(Rstar_interji, this_iteration)

}

#plot(Rstar_interji$temp_full, Rstar_interji$aji) 

################################################
# aij guidelines for generalist on specialist
# exerts lower competition but wider set of ranges
# filter below min(t) aij = 0.01
# filter (max(t)-Topt)/2 = 0.01
# filter between min(t) and (max(t)-Topt)/2 = 0.1


Rstar_interij <- data.frame()

for(i in 1:length(combos_dat$combo)){
  
# write alphas below Tmin
below <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(-10.0, min_tpcj, by = 0.1),1))) %>% 
  mutate(aij = 0.0)  %>%
  mutate(combo = i)

# write alphas between Tmin and Tmax
mid <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(min_tpcj+0.1, coldbiasj, by = 0.1),1))) %>% 
  mutate(aij = 0.1)  %>%
  mutate(combo = i)

# Smooth the increase of mid on cold side
# logistic, a=1,B=1,k=4
x <- as.numeric(temp_full$temp_full) # use full data range so I can select the elements I need.
y <- logistic(x=x)
#plot(x, y) # great!!!

# find elements I need for mid...
# mid # min temp = min_tpcj+0.1 = -3.1, max temp coldbiasj = 16.6
seriesM <- length(seq(min_tpcj+0.1,coldbiasj, by = 0.1)) # 86

# because y is build on -10:30 by 0.1 series, select a portion of this series that is the same length as mid
# length of mid = seriesM
# start at a place where curve begins to accelerate
length(y)
y1 <- y[71:(70+seriesM)] 

# scale and add constant to bring 0 to minimum
mid$aij <- round((mid$aij*y1),2)

# pull funtion to the left (remove zeros, add equal elements to the other end of max to balance)
#plot(mid$temp_full[20:198],mid$aij[20:198]) # removing the first 20 elements leads to nice quick increase
mid$aij <- c(mid$aij[20:seriesM], rep(max(mid$aij), length.out = 19))
#plot(mid$temp_full,mid$aij)

# write alphas above Tmax
above <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(coldbiasj+0.1, 30.0, by = 0.1),1))) %>% 
  mutate(aij = 0.0)  %>%
  mutate(combo = i)

this_iteration <- rbind(below,mid,above)
# scale y values to desired min and max
this_iteration$aij <- scales::rescale(this_iteration$aij, to = c(alpha_min, combos_dat$alpha_max_j[i])) 

Rstar_interij <- rbind(Rstar_interij, this_iteration)
}

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

Rstar_interij <- left_join(Rstar_interij, K_intra, by = "temp_full")

full_alphas <- full_join(Rstar_interij, Rstar_interji, by = c("temp_full", "combo"))

full_alphas$temp_full <- as.numeric(as.character(full_alphas$temp_full))
full_alphas$combo <- as.factor(full_alphas$combo)

# need to match decimal places with model
full_alphas$temp_full <- round(full_alphas$temp_full,1)

