
################
# TIME SERIES
################

# runs will be 1000 sites
dir_string <- getwd()

env_tmean_spring <- read_csv(paste0(dir_string,"/Data/tmean_prism_filtered.csv"))

# organize so that runs are columns, remove lat long, year and month will also be columns, env value will fill values under runs
env_tmean <- env_tmean_spring %>%
  dplyr::select(-lat, -long) %>%
  tidyr::pivot_wider(., names_from = plot_id, values_from = tmean)

###################################
# divide into historical, current
###################################

#min(env_tmean$year) # 1895
#max(env_tmean$year) # 2019
# historical = typically up to 1850, though I'll have to justify splitting this timeseries in approximately half, 1895-1950
length(1895:2019)/2 # 62.5
1895 + 62 # 1957 
length(1895:1957)
# current = other half of timeseries, 1959-2019 (note that perfectly half would be 1957)
#names(env_tmean)
length(1958:2019) # one year shorter to be conservative

env_tmean_historic <- env_tmean %>%
  dplyr::filter(year %in% c(1895:1957))

env_tmean_current <- env_tmean %>%
  dplyr::filter(year %in% c(1958:2019))

# clean up historic and current data
env_tmean_historic <- as.data.frame(env_tmean_historic)
rownames(env_tmean_historic) <- env_tmean_historic$year 
env_tmean_historic <- dplyr::select(env_tmean_historic, -year)

env_tmean_current <- as.data.frame(env_tmean_current)
rownames(env_tmean_current) <- env_tmean_current$year 
env_tmean_current <- dplyr::select(env_tmean_current, -year)

#####################################################
# find means and std. dev for historic and current
#####################################################

historic_val <- env_tmean_spring %>%
  filter(year %in% c(1895:1957)) %>%
  mutate(mean = mean(tmean)) %>%
  mutate(sd = sd(tmean)) %>%
  select(mean, sd) %>%
  distinct()
historic_mean <- as.numeric(historic_val[1])
historic_sd_original <- as.numeric(historic_val[2])

current_val <- env_tmean_spring %>% 
  filter(year %in% c(1958:2019)) %>%
  mutate(mean = mean(tmean)) %>%
  mutate(sd = sd(tmean)) %>%
  select(mean, sd) %>%
  distinct()
current_mean <- as.numeric(current_val[1])
current_sd_original <- as.numeric(current_val[2]) # don't end up using

#########################################################
# Find slope of temperature increase across timeseries
#########################################################

mod_historic_dat <- env_tmean_historic %>%
  as.data.frame() %>%
  mutate(year = row.names(.),
         year = as.numeric(year)) %>%
  pivot_longer(cols = 1:1000, names_to = 'site', values_to = 'temperature') %>%
  mutate(site = as.factor(site))
str(mod_historic_dat)

mod.h <- glmmTMB::glmmTMB(temperature ~ year + (1|site), 
                          data = mod_historic_dat)
summary(mod.h)
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -7.2881007  0.3398407  -21.45   <2e-16 ***
#   year         0.0097505  0.0001528   63.81   <2e-16 ***

mod_current_dat <- env_tmean_current %>%
  as.data.frame() %>%
  mutate(year = row.names(.),
         year = as.numeric(year)) %>%
  pivot_longer(cols = 1:1000, names_to = 'site', values_to = 'temperature') %>%
  mutate(site = as.factor(site))
str(mod_current_dat)

mod.c <- glmmTMB::glmmTMB(temperature ~ year + (1|site), 
                          data = mod_current_dat)
summary(mod.c)
# Conditional model:
#   Estimate  Std. Error z value Pr(>|z|)    
# (Intercept) -15.0848014   0.3473280  -43.43   <2e-16 ***
#   year          0.0133920   0.0001536   87.18   <2e-16 ***

###########################################################
# center each run on the run-specific mean for each era
###########################################################

# this is a for loop iterating through each column (representing 1000 locations), 
# then rescaling on mean of each location's mean temperature and the historic location-std. deviation,
# (so that increased variation shows up), then assigning standardized values to a matrix.

# historic first:
mat_historic <- matrix(ncol = 1000, nrow = dim(env_tmean_historic)[1])
colnames(mat_historic) <- c(1:1000)

# save run specific means and sd to use for current, future (i), etc.
historic_run_mean <- c()
historic_run_sd <- c()

for(i in 1:length(colnames(env_tmean_historic))){
  x <- env_tmean_historic[,i] # z score
  historic_run_mean[i] <- mean(x)
  historic_run_sd[i] <- sd(x)
  mat_historic[,i] <- (x - historic_run_mean[i]) / historic_run_sd[i] 
}
rownames(mat_historic) <- rownames(env_tmean_historic)

round(mean(mat_historic),4) == 0 # must be true to move on
round(sd(mat_historic,2)) == 1 # must be true to move on

mat_historic <- mat_historic + historic_mean # back to normal temperatures for tpc
mean(mat_historic)
sd(mat_historic)

################
# current next:

mat_current <- matrix(ncol = 1000, nrow = dim(env_tmean_current)[1])
colnames(mat_current) <- c(1:1000)

# save run specific means and sd to use for current, future (i), etc.
current_run_mean <- c()

for(i in 1:length(colnames(env_tmean_current))){
  x <- env_tmean_current[,i] # z score
  current_run_mean[i] <- mean(x)
  mat_current[,i] <- (x - current_run_mean[i]) / historic_run_sd[i]
}
rownames(mat_current) <- rownames(env_tmean_current)
round(mean(mat_current),4)
sd(mat_current)
mat_current <- mat_current + historic_mean # slide values back up into real temperatures

################
# Create futures
################

# this is where we can hopefully loop and create our new data:
# runs are standardized above, will just have to add warming increment to them

# Years to fill in below (used in density plots too)
hyears <- seq(from = 1895, to = 1957, by = 1)
cyears <- seq(from = 1958, to = 2019, by = 1)
# 2020 + 630 = 2650
# this will be broken into 10 eras (5/0.5)
fyears_all <- seq(from = 2020, to = 2649, by = 1) # whole future to fill in

# basing futures off of historic matrix, where
 #length = 63 (years for each run) * (5 (degrees) / 0.5 (degree increments))
 # cols = locations aka runs 1000
 era <- 5/0.5 # break into ten eras
 degrees <- 0.5
 length_time <- 63
 
 #note futures don't need to be standardized if historic is already standardized
 
 for(i in 1:era){ 
temp <- mat_historic
rownames(temp) <- fyears_all[((length_time*i)-(length_time-1)):(length_time*i)] # selects correct string of years
temp <- temp + degrees # add degrees to mat_historic
assign(paste0("mat_future", i), temp) # name the df with the future era we are at
assign(paste0("future", i, "_mean"), historic_mean + degrees) # save era specific means
assign(paste0("f", i, "years"), c(fyears_all[((length_time*i)-(length_time-1)):(length_time*i)])) # save year sequences for each era

degrees <- degrees + 0.5 # increasing increment for next run
 } 
 
# need a list of matrices to loop through
list_mat <- list(mat_historic, mat_current, mat_future1,  mat_future2, mat_future3, mat_future4, mat_future5, mat_future6, mat_future7, mat_future8, mat_future9, mat_future10)
 
list_mat_names <- c("mat_historic", "mat_current", "mat_future1",  "mat_future2", "mat_future3", "mat_future4", "mat_future5", "mat_future6", "mat_future7", "mat_future8", "mat_future9", "mat_future10")
 
names(list_mat) <- list_mat_names


