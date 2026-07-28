
###################################################
# Figures depicting niche & fitness differences 
# on coexistence space, 
# and as proportions changing over eras
###################################################

dir_string <- getwd()

source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R"))

###############################
# Prepare parameter_dat
###############################

# Filter niche area (where persistence > 1)
# for coexistence graphs, remove all rows of parameter_dat where a r is < 0,
# as this is indicative of extinction rather than a true exclusion

load(paste0(here::here(),"/Scripts LV/Final dataframes/parameter_dat.Rdata")) # updated 7-27-2026
source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - generalist v specialist.R"))

#filter out extinction situations when r > 0
df_params <- parameter_dat %>%
  dplyr::filter(env %in% seq(min_spi,max_spi, by = 0.1)) %>%# min_spj max_spj =  temperature of niche breadth for spp j
  dplyr::filter(env %in% seq(min_spj,max_spj, by = 0.1))# min_spi max_spi = temperature of niche breadth for spp i

#########################
# Libraries
#########################

library(viridis)
library(ggplot2)
library(ggside)
library(purrr) # for insets
library(RColorBrewer)

##############################
# Select facets of interest
##############################

temp <- df_params %>% 
  filter(era %in% c('historic','2°C')) %>%
  filter(scenario %in% c('Rstar inter & intra', 'SGH inter & intra',
                         'Rstar intercepts', 'SGH intercepts')) %>% # results in 274,090 rows 
  mutate(theory = fct_collapse(scenario, 
                               "Abrupt" = c('Rstar inter & intra', 'Rstar intercepts'),
                               "Gradual" = c('SGH inter & intra','SGH intercepts')),
         competition_type = fct_collapse(scenario, 
                                         "Constant" = c('Rstar intercepts','SGH intercepts'),
                                         "Temperature-dependent" = c('Rstar inter & intra', 'SGH inter & intra'))) 
head(temp)
# necessary changes to make insets work later in the script:
temp$era <- droplevels(temp$era)
# change order so that we can see historic overtop of 2c
temp$theory <- droplevels(temp$theory)
temp$scenario <- droplevels(temp$scenario)
temp$competition_type <- droplevels(temp$competition_type)
temp$competition_type <- factor(temp$competition_type, 
                                levels = c('Temperature-dependent','Constant'))
temp <- as.data.frame(temp)

##############################################
# Visualize coexistence graph
##############################################

min(temp$niche_d)
max(temp$niche_d)

niche_diff <- seq(from = -.25, to = 1, by = 0.001) # this equals 1-rho, i.e., x axis
rho <- 1-niche_diff
rho # fitness_ratio_min
1/rho # fitness_ratio_max

df_condition <- data.frame(niche_diff = niche_diff, # = SD, x-axis, 1-rho
                  rho = rho, # rho
                  one_over_rho = 1/rho) # 1/rho
df_condition$one_over_rho <- round(df_condition$one_over_rho, 3)
df_condition$rho <- round(df_condition$rho, 3)


df_condition <- df_condition %>%
  filter(niche_diff >= -0.181 & niche_diff <= 0.986)

coex_graph_all <- 
  ggplot() +
  # # Shaded region BELOW the coexistence box (exclusion zone)
  # geom_ribbon(data = df_condition,
  #             aes(x = niche_diff, ymin = 0, ymax = rho)) +
  # 
  # # Shaded region ABOVE the coexistence box (exclusion zone)
  # geom_ribbon(data = df_condition,
  #             aes(x = niche_diff, ymin = one_over_rho, ymax = Inf)) +
  
  # coexistence box
  # geom_ribbon(data = df_condition, aes(x = niche_diff, ymin = rho, ymax = one_over_rho),
  #             fill = 'lightblue', alpha = 0.8) +  
  
  # 2D density contours for parameter draws
  geom_density_2d_filled(
    data = temp,
    aes(x = niche_d, y = fit_d_kj, 
        group = competition_type,
        fill = after_stat(interaction(level, group))),
    alpha = 0.8, contour_var = "ndensity",
    breaks = seq(0.1, 1, length.out = 14) 
  ) +
  scale_fill_manual(
    values = c(
      colorRampPalette(c("orangered2","gold"))(13),
      colorRampPalette(c("grey20","grey95"))(13)
    ),
    name = "Density",
    guide = 'none'
  ) + 
  
  # make legend - add dummy geom to create the color legend
  geom_line(data = data.frame(x = NA, y = NA, type = c('constant', 'temperature-dependent')),
            aes(x = x, y = y, color = type), 
            linewidth = 0) +  # invisible 
  
  scale_color_manual(
    values = c('constant' = 'grey50', 'temperature-dependent' = 'orangered2'),
    labels = c('constant competition', 'temperature-dependent competition'),
    name = NULL  # or add a title if you want
  ) + 
  
  
  # facets
  facet_grid(rows = vars(theory), cols=vars(era)) +
  
  # Axes
  scale_y_log10(name = expression(paste("Fitness difference: ", kappa[j]/kappa[i])),   
                limits = c(0.05, 20)) + # ,   limits = c(0.05, 20)
  xlab(expression(paste("Niche Difference: ", 1-rho))) +
  
  # themes
  theme_light() +
  theme(legend.position = "top",
       # panel.background = element_rect(fill = "lightblue"),       # just the data panel
        text = element_text(size = 16)) +
  
  # outline boundary
  geom_line(data = df_condition, aes(x = niche_diff, y = rho), color = 'black', linewidth = .5) +
  geom_line(data = df_condition, aes(x = niche_diff, y = one_over_rho), color = 'black', linewidth = .5)

coex_graph_all

#----------------------------------------------------------------------
path <- paste0(here::here(),"/Figures-output/Figure 4 - new coex graph.jpeg")
jpeg(path, res = 600, width=6, height=7, units="in")
coex_graph_all 
Sys.sleep(3)
dev.off()
#--------------------------------------------------------------------

##########################################
# How much extinction in each era 
# by theory by competition_type facet?
##########################################

load(here::here("Scripts LV/Final dataframes/proportion_dat.Rdata"))

pie_limits <- proportion_dat %>%
  group_by(era, theory, comp_scenario) %>%
  filter(era %in% c('historic', '2°C') & comp_scenario %in% c('both \n temperature-dependent')) %>%
  filter(scenario %in% c('Rstar inter & intra', 'SGH inter & intra',
                         'Rstar intercepts', 'SGH intercepts')) %>%
  mutate(category = fct_collapse(category,
                                 'extinct' = c('both spp extinct', 'specialist extinct'),
                                 'within thermal limits' = c('coexist','priority effect','generalist excluded (+)','generalist excluded (-)','specialist excluded (-)','specialist excluded (+)'))) %>%
  group_by(category,era, theory, comp_scenario) %>%
  mutate(percent = ifelse(n() > 1, sum(prop)*100, prop*100)) %>%
  select(era, theory, comp_scenario, category, percent) %>%
  distinct()

# make annotated pie charts for this:
# Basic piechart
pie_figs <- ggplot(pie_limits, aes(x="", y=percent, fill=category)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  facet_wrap(~era + theory) +
  theme_void() + # remove background, grid, numeric labels
  scale_fill_manual(values = c("purple",'white'))
pie_figs

#----------------------------------------------------------------------
path <- paste0(here::here(),"/Figures-output/PIE_fig3.jpeg")
jpeg(path, res = 600, width=7, height=6, units="in")
pie_figs 
dev.off()
#--------------------------------------------------------------------

#########################################
# RESULTS: extinction in Pie graphs
#########################################

view(pie_limits)

