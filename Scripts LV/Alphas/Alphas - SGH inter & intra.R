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

########################
# Stress-gradient 

# linear increase to Topt, then linear decrease, 
# species differ in their range
# go off of values in full_rs
# view(full_rs)

########################
# generalist sp j

temp_full <- as.data.frame(temp_full)
temp_full$temp_full <- as.numeric(temp_full$temp_full) 
temp_full$temp_full <- as.character(round(temp_full$temp_full,1)) # if i don't round we get issues with length of temps vs lengths of alphas

Topt = round(TI, 1)
maxTj <- max(full_rs$temp_full[full_rs$rj > 0]) #19.2
minTj <- min(full_rs$temp_full[full_rs$rj > 0]) #-5.9


stress_interij <- data.frame()

for(i in 1:length(combos_dat$combo)){
  
  # i <- 1
  
  cold_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq(-10.0,minTj, by = 0.1),1))) %>% # -10 to -5.9 stressful
    # stress = low constant:
    mutate(aij = alpha_min) %>% 
    mutate(combo = i) 
  
  benign <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((minTj+0.1),Topt, by = 0.1),1))) %>% 
    # benign, linear increase in comp:
    mutate(aij = seq(alpha_min, combos_dat$alpha_max_j[i], length.out = length(seq((minTj+0.1),Topt, by = 0.1)))) %>% 
    mutate(combo = i) 
  
  heat_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((Topt+0.1),maxTj, by = 0.1),1))) %>%
    # very fast decrease: 
    mutate(aij = seq(max(benign$aij), min(cold_stress$aij), 
                     length.out = length(factor(seq((Topt+0.1), maxTj, by = 0.1))))) %>% 
    mutate(combo = i)  
  
  boiling_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((maxTj+0.1),30.0, by = 0.1),1))) %>% 
    # stress = low constant"
    mutate(aij = alpha_min) %>%
    mutate(combo = i)
  
  this_iteration <- rbind(cold_stress, benign, heat_stress, boiling_stress)
  stress_interij <- rbind(stress_interij, this_iteration)
  
}

#plot(stress_interij$temp_full, stress_interij$aij, col = stress_interij$combo) # gorge.

########################
# specialist sp i

maxTi <- max(full_rs$temp_full[full_rs$ri > 0]) # 17.2
minTi <- min(full_rs$temp_full[full_rs$ri > 0]) # 0.6

stress_interji <- data.frame()

for(i in 1:length(combos_dat$combo)){
  
  # i <- 1
  
  cold_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq(-10.0, minTi, by = 0.1),1))) %>% 
    # stress = low constant
    mutate(aji = alpha_min) %>%
    mutate(combo = i)
  
  benign <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((minTi+0.1),Topt, by = 0.1),1))) %>% 
    # benign, linear increase in comp:
    mutate(aji = seq(alpha_min, combos_dat$alpha_max_i[i], length.out = length(seq((minTi+0.1),Topt, by = 0.1))))%>%
    mutate(combo = i)
  
  heat_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((Topt+0.1),maxTi, by = 0.1),1))) %>%
    # very fast decrease
    dplyr::mutate(aji = seq(max(benign$aji), min(cold_stress$aji), 
                            length.out = length(factor(seq((Topt+0.1),maxTi, by = 0.1))))) %>%
    mutate(combo = i)
  
  boiling_stress <- temp_full %>%
    dplyr::filter(temp_full %in% as.character(round(seq((maxTi+0.1),30.0, by = 0.1),1))) %>% 
    # stress = low constant
    mutate(aji = alpha_min)%>%
    mutate(combo = i)
  
  this_iteration <- rbind(cold_stress, benign, heat_stress, boiling_stress)
  stress_interji <- rbind(stress_interji, this_iteration)
  
}

#plot(stress_interji$temp_full, stress_interji$aji, col = stress_interji$combo)

#################
# Intra-specific
#################

########################
# generalist sp j

temp_full <- as.data.frame(temp_full)
temp_full$temp_full <- as.numeric(temp_full$temp_full) 
temp_full$temp_full <- as.character(round(temp_full$temp_full,1)) # if i don't round we get issues with length of temps vs lengths of alphas

Topt = round(TI, 1)
maxTj <- max(full_rs$temp_full[full_rs$rj > 0]) #19.2
minTj <- min(full_rs$temp_full[full_rs$rj > 0]) #-5.9

cold_stress <- temp_full %>%
  dplyr::filter(temp_full %in% as.character(round(seq(-10.0,minTj, by = 0.1),1))) %>% # -10 to -5.9 stressful
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

# repeat the same intra for all combos:

intra_dat <- data.frame(
  temp_full = rep(stress_intraii$temp_full, times = length(unique(combos_dat$combo))),
  ajj = rep(stress_intrajj$ajj, times = length(unique(combos_dat$combo))),
  aii = rep(stress_intraii$aii, times = length(unique(combos_dat$combo))),
  combo = rep(1:length(unique(combos_dat$combo)), each = length(stress_intraii$aii)))

inter_dat <- full_join(stress_interji, stress_interij, by = c("temp_full", "combo"))
  
# then full join with inter dataframes:

full_alphas <- full_join(inter_dat, intra_dat, by = c("temp_full", "combo"))

full_alphas$temp_full <- as.numeric(as.character(full_alphas$temp_full))
full_alphas$combo <- as.factor(full_alphas$combo)

# need to match decimal places with model
full_alphas$temp_full <- round(full_alphas$temp_full,1)




