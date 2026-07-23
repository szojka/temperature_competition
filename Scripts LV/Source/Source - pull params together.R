################################
# PULL PARAMETERS TOGETHER
################################
# 
# full_params <- cbind(full_rs, full_alphas) 
# full_params <- select(full_params, c(-8))

# repeat full_rs for as many times there are combos

full_params <- left_join(full_alphas, full_rs, by = 'temp_full')
