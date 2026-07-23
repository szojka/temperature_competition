######################################
# CONSTANTS FOR R*
######################################

# required the stress-gradient piecewise function:
source(paste0(dir_string,"/Scripts LV/Alphas/Alphas - Rstar inter.R"))

# temp_full <- seq(-10,30, by = 0.1) # fitting the growth curve off these full data: 
# temp_full <-  data.frame(temp_full = temp_full) 

#############
# no change

# will be specific to the combo:
alphas_constant <- full_alphas %>%
  filter(temp_full %in%  as.character(round(historic_mean,1) ))

full_alphas <- data.frame(
  temp_full = rep(temp_full$temp_full, times = length(unique(alphas_constant$combo))),
  ajj = rep(alphas_constant$ajj, each = length(temp_full$temp_full)),
  aii = rep(alphas_constant$aii, each = length(temp_full$temp_full)),
  aij = rep(alphas_constant$aij, each = length(temp_full$temp_full)),
  aji = rep(alphas_constant$aji, each = length(temp_full$temp_full)),
  combo = rep(1:length(unique(alphas_constant$combo)), each = length(temp_full$temp_full))
)

full_alphas$temp_full <- as.numeric(full_alphas$temp_full)

# need to match decimal places with model
full_alphas$temp_full <- round(full_alphas$temp_full,1)
