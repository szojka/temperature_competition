
#--------------------------------------------
# SUPPLEMENTAL PARAMETER SHAPE FIGURES
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
source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R"))

######################
# Load rs
######################

source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - generalist v specialist.R"))
tpc_dat <- full_rs %>%
  pivot_longer(., cols = c("rj", "ri"), names_to = "species", values_to = "r")
tpc_dat$species <- case_match(tpc_dat$species, 
                              "rj" ~ "spp j; generalist",
                              "ri" ~ "spp i; specialist")
# get max specialist and max generalist value:
tpc_dat %>%
  group_by(species) %>%
  mutate(max = max(r)) %>%
  select(max, species) %>%
  distinct()
#30.44/34 # = generalist is 89.41176 % of the specialist

######################
# Load alphas:
######################

load(paste0(dir_string,"/Scripts LV/Final dataframes/combos_dat.RData"))

# source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - Rstar intercepts.R")) 
# alp_dat1 <- full_alphas %>%
#   filter(combo %in% 1) %>%
#   select(-combo) %>%
#   pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
#   dplyr::mutate(scenario = "Rstar intercepts")%>%
#   dplyr::mutate(theory = "Abrupt")%>%
#   dplyr::mutate(shape = "constant") %>%
#   dplyr::mutate(form = "Constant")
# source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - SGH intercepts.R"))
# alp_dat2 <- full_alphas %>%  
#   filter(combo %in% 1) %>%
#   select(-combo) %>%
#   pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
#   dplyr::mutate(scenario = "SGH intercepts")%>%
#   dplyr::mutate(theory = "Gradual")%>%
#   dplyr::mutate(shape = "constant") %>%
#   dplyr::mutate(form = "Constant")
# source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - Rstar intra.R"))
# alp_dat3 <- full_alphas %>%
#   filter(combo %in% 1) %>%
#   select(-combo) %>%
#   pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
#   dplyr::mutate(scenario = "Rstar intra")%>%
#   dplyr::mutate(theory = "Abrupt")%>%
#   dplyr::mutate(shape = NA) %>%
#   dplyr::mutate(form = NA)
source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - Rstar inter & intra.R"))
alp_dat4 <- full_alphas %>%
  filter(combo %in% 1) %>%
  select(-combo) %>%
  pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
  dplyr::mutate(scenario = "Rstar inter & intra")%>%
  dplyr::mutate(theory = "Abrupt")%>%
  dplyr::mutate(shape = "temperature-dependent") %>%
  dplyr::mutate(form = "Abrupt") # helps for organization later
# source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - Rstar inter.R"))
# alp_dat5 <- full_alphas %>%
#   filter(combo %in% 1) %>%
#   select(-combo) %>%
#   pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
#   dplyr::mutate(scenario = "Rstar inter")%>%
#   dplyr::mutate(theory = "Abrupt")%>%
#   dplyr::mutate(shape = NA) %>%
#   dplyr::mutate(form = NA)
source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - SGH inter & intra.R"))
alp_dat6 <- full_alphas %>%
  filter(combo %in% 1) %>%
  select(-combo) %>%
  pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
  dplyr::mutate(scenario = "SGH inter & intra")%>%
  dplyr::mutate(theory = "Gradual")%>%
  dplyr::mutate(shape = "temperature-dependent") %>%
  dplyr::mutate(form = "Gradual") # helps for organization later
# source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - SGH inter.R"))
# alp_dat7 <- full_alphas %>%
#   filter(combo %in% 1) %>%
#   select(-combo) %>%
#   pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
#   dplyr::mutate(scenario = "SGH inter")%>%
#   dplyr::mutate(theory = "Gradual")%>%
#   dplyr::mutate(shape = NA)%>%
#   dplyr::mutate(form = NA)
# source(paste0(here::here(), "/Scripts LV/Alphas/Alphas - SGH intra.R"))
# alp_dat8 <- full_alphas %>%
#   filter(combo %in% 1) %>%
#   select(-combo) %>%
#   pivot_longer(., cols = 2:5, names_to = "term", values_to = "alpha") %>%
#   dplyr::mutate(scenario = "SGH intra") %>%
#   dplyr::mutate(theory = "Gradual") %>%
#   dplyr::mutate(shape = NA)%>%
#   dplyr::mutate(form = NA)

# want to make each scenario its own data frame to display all 4 main alpha scenarios
alp_dat <- rbind(alp_dat4,alp_dat6)

alp_dat$species <- NA
alp_dat$species[alp_dat$term %in% c("ajj", "aij")] <- "spp j; generalist"
alp_dat$species[alp_dat$term %in% c("aii", "aji")] <- "spp i; specialist"

alp_dat$term <- case_match(alp_dat$term, 
                           "ajj" ~ "intraspecific",
                           "aii" ~ "intraspecific",
                           "aij" ~ "interspecific",
                           "aji" ~ "interspecific")

alp_dat$scenario <- as.factor(alp_dat$scenario)
alp_dat$scenario <- factor(alp_dat$scenario, levels = scenario_names)
levels(alp_dat$scenario)
alp_dat$scenario <- droplevels(alp_dat$scenario)
levels(alp_dat$scenario)

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
                     'spp i; specialist', 'spp j; generalist')

combined_breaks <- c("historic","current","future (1\u00B0C)", "future (3\u00B0C)", 'future (5\u00B0C)', 
                     "Topt",
                     'spp i; specialist', 'spp j; generalist')

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
  geom_vline(aes(xintercept = historic_mean), color = '#54278F', linewidth = .8, linetype = 'dashed') +
  geom_vline(aes(xintercept = TI, color = "Topt"), linewidth = .8, linetype = 'dashed') 
  # geom_text(aes(x = -Inf, y = Inf, label = 'A'),
  #           hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") 
r_plot

#------------------------------------------------------------------------------
jpeg("Figures-output/params_tpc.jpeg", res = 600, width=14, height=5, units="in")
r_plot + theme(text = element_text(size = 26))
dev.off()
#------------------------------------------------------------------------------

########################
# Alphas together:
alp_dat <- alp_dat %>%
  select(-form, -shape, -scenario) %>%
  distinct()
str(alp_dat)
alp_dat$term <- as.factor(alp_dat$term)
alp_dat$theory <- as.factor(alp_dat$theory)
alp_dat$species <- as.factor(alp_dat$species)
alp_dat$theory <- factor(alp_dat$theory, levels = c("Gradual", "Abrupt"))


alpha_plot <- ggplot(data = alp_dat, mapping = aes(x = temp_full, y = alpha, color = species, group = species)) +
  geom_point(size = 1) +
  geom_line(linewidth = 1.5) + 
  geom_vline(xintercept = 11.5, color = 'grey50', linetype = 'dashed',linewidth = 1)+
  theme_light() +
  scale_color_manual(values = c("midnightblue","steelblue")) +
                     #guides = guide_legend(direction = "horizontal", ncol = 1, nrow = 2)) +
  labs(x = paste0("Temperature"), y = expression(alpha), color  = "") + # in breeding season
  theme(legend.position = "none",
        text = element_text(size = 16)
  ) +
  facet_grid(cols = vars(theory),rows = vars(term), scales = 'free_y') +
  scale_y_continuous() +
  scale_x_continuous(n.breaks = 5, labels = function(x) paste0(x, "\u00B0C"), 
                     limits = c(0,20))  
alpha_plot 

# FIXME what is going on with  Constant? Just make a line.

# # Organizing facets
# fullplot <-  r_plot | alpha_plot  + plot_layout(guides = 'collect') &
#   theme(legend.position = 'top')
# fullplot

#-----------------------------------------------------
jpeg("Figures-output/params_alpha.jpeg", res = 600, width=7, height=4, units="in")
alpha_plot 
dev.off()

# jpeg("Figures-output/LV Figures/parameters_new.jpeg",res = 600, width=10, height=5, units="in")
# fullplot
# dev.off()
#-----------------------------------------------------
