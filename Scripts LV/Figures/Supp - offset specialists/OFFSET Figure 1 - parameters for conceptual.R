
#--------------------------------------------
# PARAMETER FIGURES
#--------------------------------------------

######################################
# Set up working environment
######################################

# clear working directory
rm(list = ls())

# set directory string (makes compatable with cluster)
dir_string <- getwd()
#dir_string <- "/cluster/medbow/project/coexistence/mszojka" # for cluster!

# for naming figures, list of names for each scenario
source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls - offset TI.R"))

######################
# Load rs
######################

source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - offset.R"))
tpc_dat <- full_rs %>%
  pivot_longer(., cols = c("rj", "ri"), names_to = "species", values_to = "r")
tpc_dat$species <- case_match(tpc_dat$species, 
                              "rj" ~ "spp j; warm specialist",
                              "ri" ~ "spp i; cool specialist")
# get max specialist and max generalist value:
tpc_dat %>%
  group_by(species) %>%
  mutate(max = max(r)) %>%
  select(max, species) %>%
  distinct()
# max species          
# <dbl> <chr>            
#   1  34.0 spp j; generalist
# 2  34.0 spp i; specialist

######################
# PLOTTING MEAN PARAMETERS
######################

# for plotting range of environmental conditions for each era...
ranges <- data.frame(era = c
                     ("historic", "current", "future (1\u00B0C)", "future (3\u00B0C)", 'future (5\u00B0C)'),
                     minimumT = c(min(mat_historic), min(mat_current),min(mat_future2),min(mat_future6),min(mat_future10)), maximumT = c(max(mat_historic), max(mat_current),max(mat_future2),max(mat_future6),max(mat_future10)), position = c(-1,-2,-3,-4,-5))

# the colors I want:
library(viridis)
viridis(5, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")
library(RColorBrewer)
display.brewer.pal(6, 'Purples')
pal <- brewer.pal(6, 'Purples') # colors for eras.

ranges$era <- as.factor(ranges$era)
ranges$era <- factor(ranges$era, levels = c("historic", "current","future (1\u00B0C)", "future (3\u00B0C)", 'future (5\u00B0C)'))
ranges$range <- ranges$maximumT - ranges$minimumT
ranges <- ranges %>% arrange(era) # I want to make sure historic comes first
ranges$colors <- pal[2:6]

# Define the colors
tpc_colors <- c( "midnightblue","steelblue")
range_colors <- unique(ranges$colors)
combined_colors <- c(range_colors, "orange3", tpc_colors)

# Define the labels (ensure order matches the colors)
combined_labels <- c("historic","current","future (1\u00B0C)", "future (3\u00B0C)", 'future (5\u00B0C)', 
                     expression(T[opt]),
                     'spp i; cool specialist', 'spp j; warm specialist')

combined_breaks <- c("historic","current","future (1\u00B0C)", "future (3\u00B0C)", 'future (5\u00B0C)', 
                     "Topt",
                     'spp i; cool specialist', 'spp j; warm specialist')

# Create the plot
# r plot
r_plot <- ggplot() +
  geom_line(data = tpc_dat, aes(x = temp_full, y = r, color = species),
            linewidth =  3, alpha = 0.7) + 
  theme_light() +
  ggstance::geom_linerangeh(data = ranges, aes(y = position, xmin = minimumT , xmax = maximumT, group = era, 
                                               color = as.factor(era)), size = 2) +
  scale_color_manual(
    labels = combined_labels,
    values = combined_colors,
    breaks = combined_breaks#,
   # guides = guide_legend(direction = "horizontal", ncol = 4, nrow = 2)
  ) +
  labs(x = paste0("Temperature ","\u00B0","C"), y = expression(r), color = "") +
  theme(legend.position = 'top',#c(0.75,0.75)
        text = element_text(size = 16)) +
  xlim(0,25) +
  geom_vline(aes(xintercept = historic_mean), color = '#54278F', linewidth = .8, linetype = 'dashed')

  # geom_text(aes(x = -Inf, y = Inf, label = 'A'),
  #           hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") 
r_plot

#---------------------------------------------------
jpeg("Figures-output/OFFSET_params_tpc.jpeg", res = 600, width=14, height=5, units="in")
r_plot + theme(text = element_text(size = 26))
dev.off()
#---------------------------------------------------

