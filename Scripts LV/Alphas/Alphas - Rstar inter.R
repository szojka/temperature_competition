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

# maxes is sampled from parameter combos
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

# will be constant: alpha_intra_min

#######################
# Pull into dataframe
#######################

full_alphas <- full_join(Rstar_interji, Rstar_interij, by = c("temp_full", "combo"))
full_alphas <- full_alphas %>%
  mutate(aii = alpha_intra_min) %>%
  mutate(ajj = alpha_intra_min)

full_alphas$temp_full <- as.numeric(full_alphas$temp_full)
full_alphas$combo <- as.factor(full_alphas$combo)

# need to match decimal places with model
full_alphas$temp_full <- round(full_alphas$temp_full,1)

