# NOTES FOR MEGAN:
# I originally did this where the new min temp (where r = 0) was an equal distance away from the mean as for the max
# That kinda shrunk the left tail though, so this version allows you to set what temp you want the min at and what temp
# you set the max at, while keeping the T_opt of r the same as in the dataset you gave me.

head(full_rs)

# returns standardized parameter values from newmin to newmax
# currentmin = current r min values
# currentmax = current r max values
# xvals is the sequence of temps with r > 0
std_params <- function(currentmin, currentmax, newmin, newmax, opt_temp, xvals) {
  y <- rep(NA, length(xvals))
  # actual equation is (newmax - newmin)*(xvals-currentmin)/(currentmax-currentmin) + newmin
  # however, if > opt_temp, opt_temp is our newmin and our current min
  y <- ifelse(xvals > opt_temp, (newmax-opt_temp)*(xvals-opt_temp)/(currentmax-opt_temp) + opt_temp, y)
  # if <= opt_temp, then opt_temp is our newmax and our currentmax
  y <- ifelse(xvals <= opt_temp, (opt_temp-newmin)*(xvals-currentmin)/(opt_temp-currentmin) + newmin, y)

  return(y)
}

# define goal min and max for Species

# try it with r data
# sj first
sj <- full_rs %>%
  filter(rj != 0)

currentmin <- min(sj$temp_full)
currentmax <- max(sj$temp_full)

opt_temp <- sj$temp_full[which(sj$rj == max(sj$rj))]

newmin <- sj_min
newmax <- sj_max

newxvals <- std_params(currentmin = currentmin, currentmax = currentmax,
                       newmin = newmin, newmax = newmax, opt_temp=opt_temp, xvals = sj$temp_full)

#double check
min(newxvals)
max(newxvals)

sj$temp_rescaled <- newxvals

# now si
si <- full_rs %>%
  filter(ri != 0)

currentmin <- min(si$temp_full)
currentmax <- max(si$temp_full)
opt_temp <- si$temp_full[which(si$ri == max(si$ri))]

newmin <- si_min # set low to show how the min and max don't have to be symmetrical
newmax <- si_max

newxvals <- std_params(currentmin = currentmin, currentmax = currentmax,
                       newmin = newmin, newmax = newmax,  opt_temp=opt_temp, xvals = si$temp_full)

#double check
min(newxvals)
max(newxvals)

si$temp_rescaled <- newxvals

#quartz()
ggplot() +
  theme_classic() +
  geom_line(data=sj, aes(x=temp_full, y=rj), color="lightblue", linetype="dashed", linewidth=2) +
  geom_line(data=sj, aes(x=temp_rescaled, y=rj), color="lightblue", linetype="solid", linewidth=2) +
  geom_line(data=si, aes(x=temp_full, y=ri), color="orchid1", linetype="dashed", linewidth=2) +
  geom_line(data=si, aes(x=temp_rescaled, y=ri), color="orchid1", linetype="solid", linewidth=2) +
  labs(x="Temp", y="r")

  # round temp_rescaled to 2 places 00.00 as meaningful differences are at this scale. Make sure this doesn't cause problems in the coexistence simulations
  # OR round to 1 and remove duplicates, because the problem will be that the length of the environment will be different between alphas and rs causing issues
  # link sj and s2 to full series of temperatures then make values that are missing into r = 0
  # name the new dataframe with rj and r2 and temp_full 'r_full'
  
# remove temperature duplicates
sj.full <- sj %>%
  dplyr::select(-temp_full) %>%
  dplyr::mutate(temp_full = round(temp_rescaled,1)) %>%
  dplyr::select(-temp_rescaled, -ri) %>%
  dplyr::distinct(temp_full, .keep_all = TRUE)
sj.full$temp_full <- as.character(round(sj.full$temp_full,1)) # change to character for joining

# data frame I can use to get full temperature range:
s.full <- data.frame(temp_full = temp_full)
s.full$temp_full <- as.character(round(s.full$temp_full,1))# change to character for joining

# join species j rs to full temperature and fill in missing values with 0
rm(full_rs) # make sure previously named object is gone
full_rs <- full_join(s.full, sj.full, by = 'temp_full')

# species i same process
si.full <- si %>%
  dplyr::select(-temp_full) %>%
  dplyr::mutate(temp_full = round(temp_rescaled,1)) %>%
  dplyr::select(-temp_rescaled, -rj) %>%
  dplyr::distinct(temp_full, .keep_all = TRUE)# removes duplicates of temp_rescaled
si.full$temp_full <- as.character(round(si.full$temp_full,1)) # change to character for joining

full_rs <- full_join(full_rs, si.full, by = 'temp_full') # now using full_rs instead of s.full so that both species are in same dataframe
full_rs[is.na(full_rs)] <- 0 # turn NAs to 0 growth rate

# change back to numbers and match decimal places with values in models
full_rs$temp_full <- as.numeric(full_rs$temp_full) 
full_rs$temp_full <- round(full_rs$temp_full,1)
#view(full_rs) # good
  