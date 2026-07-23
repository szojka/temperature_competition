##########################################
# Find replicates to run simulation over
##########################################


# total of 10,000 replicates

# for each of those, select a combo from Alphas - set alpha values.R
load(here::here("Scripts LV/Final dataframes/combos_dat.Rdata") )

# select a location (1:1000)
# for a given matrix,
# select col 1:1000

#make dataframe of each row is i, column is combo # and locations #
# and sample these with replacement 4000

replicate_dat <- matrix(ncol = 2, nrow = 10000)
colnames(replicate_dat) <- c('location', 'combo')

for(i in 1:10000){
  replicate_dat[i,1] <- sample(1:1000, size = 1, replace = TRUE) # fill in location
  replicate_dat[i,2] <-  sample(1:length(combos_dat$combo), size = 1, replace = TRUE) # fill in combo
} # replacement doesn't matter when you manually loop with replacement

# To get these models to run on the cluster, need to constrain our replicates to 4000
# hashing out to see if Canada cluster can run full 10,000:
# replicate_dat <- replicate_dat[1:4000,]

save(replicate_dat, file = here::here("Scripts LV/Final dataframes/replicate_dat.RData"))

# then use to filter env out of mat_historic, mat_current, etc, and the filter temp_string and combo out of full_params








