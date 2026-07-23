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

########################
# generalist sp j

temp_full <- as.data.frame(temp_full)
temp_full$temp_full <- as.numeric(temp_full$temp_full) 
temp_full$temp_full <- as.character(round(temp_full$temp_full,1)) # if i don't round we get issues with length of temps vs lengths of alphas

Topt = round(TI, 1)
maxTj <- max(full_rs$temp_full[full_rs$rj > 0])
minTj <- min(full_rs$temp_full[full_rs$rj > 0]) 

  cold_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq(-10.0,minTj, by = 0.1),1))) %>% #  stressful
    # stress = low constant:
    mutate(ajj = alpha_intra_max)
  
  benign <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((minTj+0.1),Topt, by = 0.1),1))) %>% 
    # benign, linear increase in comp:
    mutate(ajj = seq(alpha_intra_max, alpha_intra_min, length.out = length(seq((minTj+0.1),Topt, by = 0.1))))
  
  heat_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((Topt+0.1),maxTj, by = 0.1),1))) %>%
    # very fast decrease: 
    mutate(ajj = seq(alpha_intra_min, alpha_intra_max, 
                     length.out = length(factor(seq((Topt+0.1), maxTj, by = 0.1))))) 
  
  boiling_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((maxTj+0.1),30.0, by = 0.1),1))) %>% 
    # stress = low constant"
    mutate(ajj = alpha_intra_max) 
  
  stress_intrajj <- rbind(cold_stress, benign, heat_stress, boiling_stress)

#plot(stress_intrajj$temp_full, stress_intrajj$ajj) # gorge.

########################
# specialist sp i

maxTi <- max(full_rs$temp_full[full_rs$ri > 0]) # 17.2
minTi <- min(full_rs$temp_full[full_rs$ri > 0]) # 0.6

cold_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq(-10.0, minTi, by = 0.1),1))) %>% 
    # stress = low constant
    mutate(aii = alpha_intra_max)
  
  benign <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((minTi+0.1),Topt, by = 0.1),1))) %>% 
    # benign, linear increase in comp:
    mutate(aii = seq(alpha_intra_max, alpha_intra_min, length.out = length(seq((minTi+0.1),Topt, by = 0.1))))
  
  heat_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((Topt+0.1),maxTi, by = 0.1),1))) %>%
    # very fast decrease
    dplyr::mutate(aii = seq(alpha_intra_min, alpha_intra_max, 
                            length.out = length(factor(seq((Topt+0.1),maxTi, by = 0.1))))) 
  
  boiling_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((maxTi+0.1),30.0, by = 0.1),1))) %>% 
    # stress = low constant
    mutate(aii = alpha_intra_max)
  
  stress_intraii <- rbind(cold_stress, benign, heat_stress, boiling_stress)

#plot(stress_intraii$temp_full, stress_intraii$aii) # gorge.

#######################
# Pull into dataframe
#######################

# stress_intraii # needs to repeat all combo levels
# stress_intrajj # needs to repeat all combo levels
# combos_dat # needs to repeat length(temp_full) for all combo levels

full_alphas <- data.frame(
  temp_full = rep(stress_intraii$temp_full, times = length(unique(combos_dat$combo))),
  ajj = rep(stress_intrajj$ajj, times = length(unique(combos_dat$combo))),
  aii = rep(stress_intraii$aii, times = length(unique(combos_dat$combo))),
  combo = rep(1:length(unique(combos_dat$combo)), each = length(stress_intraii$aii)),
  aij = rep(combos_dat$alpha_max_j, each = length(stress_intraii$aii)),
  aji = rep(combos_dat$alpha_max_i, each = length(stress_intraii$aii))
)

full_alphas$temp_full <- as.numeric(as.character(full_alphas$temp_full))
full_alphas$combo <- as.factor(full_alphas$combo)

# need to match decimal places with model
full_alphas$temp_full <- round(full_alphas$temp_full,1)

