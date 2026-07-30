
#################################
# Temperature and map figure
#################################

library(grid)
library(sf)
library(mapview)
library(tidyverse)
library(here)

######################################
# Set up working environment
######################################

# clear working directory
rm(list = ls())

# set directory string (makes compatable with cluster)
dir_string <- getwd()
#dir_string <- "/cluster/medbow/project/coexistence/mszojka" # for cluster!

source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R"))

# NOTES
# using matrices means the values are already standardized based on historical mean and sd.
# no variation is added when I created future projections, just upped intercepts.
# this standardization can be found in Source_timeseries_PRSIM_full.R

# This script has 2 figures: first uses min and max values from each year as measure of variation, 
# the second plots all raw runs in the background 

#############################
# USA map

mapviewOptions(fgb = FALSE)

usadat <- st_read(paste0(here(),"/Data/random_pts_1sqmi_waterways.gpkg"))

usa <- tigris::states()

usa %>% filter(STUSPS %in% state.abb[-c(2,11)]) -> usa

USAmap <- usadat %>% 
  ggplot() +
  geom_sf(data = usa) +
  geom_sf(color = "navy", size = 0.5) +
  theme_classic() 
USAmap

library(patchwork)
jpeg("Figures-output/maplocs.jpeg", res = 600, width=7, height=6, units="in")
USAmap
dev.off()

#############################
# Create environmental graph

source(paste0(here(),"/Scripts LV/Source/Source - timeseries PRISM.R"))

# I just rbind all the matrices than specifiy once the pivot longer
# use they hyears, cyears, etc. maximum to put the dotted lines on the graph

mat_allyears <- rbind(mat_historic,
                      mat_current,
                      mat_future1,
                      mat_future2,
                      mat_future3,
                      mat_future4,
                      mat_future5,
                      mat_future6,
                      mat_future7,
                      mat_future8,
                      mat_future9,
                      mat_future10)

# make dataframe and wrangle
df_allyears <- as.data.frame(mat_allyears) 
df_allyears$year <- rownames(df_allyears)
df_allyears <- df_allyears %>% 
  pivot_longer(cols = 1:1000, names_to = "locations", values_to = "tmean") %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(mean_tmean = mean(tmean)) %>%
  dplyr::mutate(max_tmean = max(tmean)) %>%
  dplyr::mutate(min_tmean = min(tmean)) %>%
  dplyr::distinct()
df_allyears$year <- as.numeric(df_allyears$year)

#################
# Visual prep

df_allyears$locations <- as.factor(df_allyears$locations )
levels(df_allyears$locations)

 fyears <- rownames(mat_future1)
 allyears <- c(rep(hyears, times = 1),
               c(rep(cyears, times = 1)),
               c(rep(fyears, times = 10)))

 era_column <- c(rep("historic", times = length(hyears)),
                 c(rep("current", times = length(cyears))),
                 c(rep("0.5 C", times = length(f1years))),
                 c(rep("1 C", times = length(f2years))),
                 c(rep("1.5 C", times = length(f3years))),
                 c(rep("2 C", times = length(f4years))),
                 c(rep("2.5 C", times = length(f5years))),
                 c(rep("3 C", times = length(f6years))),
                 c(rep("3.5 C", times = length(f7years))),
                 c(rep("4 C", times = length(f8years))),
                 c(rep("4.5 C", times = length(f9years))),
                 c(rep("5 C", times = length(f10years))))

 mat_allyears <- rbind(mat_historic,
                       mat_current,
                       mat_future1,
                       mat_future2,
                       mat_future3,
                       mat_future4,
                       mat_future5,
                       mat_future6,
                       mat_future7,
                       mat_future8,
                       mat_future9,
                       mat_future10)

 length(era_column)
 length(allyears)
 length(mat_allyears)

 mat_allyears <- cbind(mat_allyears, era_column, allyears)

 # make dataframe and wrangle
 df_allyears <- as.data.frame(mat_allyears)
 names(df_allyears)[1001] <- 'era'
 names(df_allyears)[1002] <- 'year'

 df_allyears <- df_allyears %>%
   pivot_longer(cols = 1:1000, names_to = "locations", values_to = "tmean")
 df_allyears$year <- as.factor(df_allyears$year)
 df_allyears$era <- as.factor(df_allyears$era)
 df_allyears$tmean <- as.numeric(df_allyears$tmean)

 df_meanT <- df_allyears %>%
   dplyr::group_by(era, year) %>%
   dplyr::mutate(mean_tmean = mean(tmean)) %>%
   dplyr::mutate(max_tmean = max(tmean)) %>%
   dplyr::mutate(min_tmean = min(tmean)) %>%
   dplyr::select(-locations) %>%
   distinct()
 tail(df_meanT)
 
 df_meanT$year <- as.character(df_meanT$year)
 df_meanT$year <- as.numeric(df_meanT$year)

 levels(df_meanT$era) <- c(era_temps[3:12], era_temps[2],era_temps[1])
 df_meanT$era <- factor(df_meanT$era, levels = era_temps)
 levels(df_meanT$era)
 #filter(df_meanT, is.na(era)) # check

 #################
 # Visual
###################
 
 library(RColorBrewer)
 library(viridis)
 # the colors I want:
 pal <- brewer.pal(11, 'PuOr')
 cols <- c("grey13", pal[11],pal[10],pal[9],pal[8],pal[7],pal[6],pal[5],pal[4],pal[3],pal[2],pal[1]) # need an extra purple to get to 12

 env_year2 <- ggplot(df_meanT) +
   geom_line(aes(x = year, y = mean_tmean, color = era), linewidth = 1) +
   #scale_color_manual(values = cols) +
   scale_color_viridis_d(option = 'magma')+
   theme_classic() +
   theme(text = element_text(size = 16),
         axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5, size = 16),
         legend.position = 'right') +
   labs(y = paste0("Temperature"), x = "Year", color = "Timeseries") +
   geom_vline(xintercept = c(max(hyears),
                             max(cyears),
                           max(f1years)),color = "grey", linetype = "dashed") +
   scale_x_continuous(n.breaks = 20) +
   scale_y_continuous(limits = c(8,20), labels = function(x) paste0(x, "\u00B0C"))+
   annotate(geom = 'text', x = 1895+31, y = 9, label = "Historic", size = 5, color = 'grey40')+
   annotate(geom = 'text', x = 1957+31, y = 9, label = "Current", size = 5, color = 'grey40') +
   annotate(geom = 'text', x = 2019+31, y = 9, label = "Warming", size = 5, color = 'grey40')
 env_year2

 library(patchwork)
 #jpeg("Figures-output/tmean_year_byloc.jpeg", res = 600, width=7, height=6, units="in")
 jpeg("Figures-output/tmean_year_stacked.jpeg", res = 600, width=8, height=6, units="in")
 env_year2
 dev.off()
 
 # make insert
 library(cowplot)
 plot.with.inset <-
   ggdraw() +
   draw_plot(env_year2) +
   draw_plot(USAmap, x = 0.1, y = .68, width = .4, height = .3)
 #plot.with.inset
 
 #----------------------------------------------
 # FIGURE 1
 
 library(patchwork)
 jpeg("Figures-output/FigureS1.jpeg", res = 600, width=8, height=6, units="in")
 plot.with.inset
 dev.off()
 
 pdf("Figures-output/FigureS1.pdf", width=8, height=6)
 plot.with.inset
 dev.off()
 
 #------------------------------------------------
