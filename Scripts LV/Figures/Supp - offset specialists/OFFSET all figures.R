#####################################################
# SAVE ALL MAIN FIGURES WITHIN OFFSET Topt SCENARIO
#####################################################

# This includes figure 2, 3, and 4

###############################################################################################
# Figure 2 - proportions of simulation outcomes for constant and temperature-dependent competition
# Figure 4 - abrupt loss of coexistence from warming step to warming step
###############################################################################################

library(tidyverse)
library(patchwork)

dir_string <- getwd()

# load proportion data
load(here::here("Scripts LV/Final dataframes/proportion_dat_offset.Rdata"))

proportion_dat <- proportion_dat_offset %>%
  mutate(category = fct_recode(category,
                               "competitive exclusion" = "specialist excluded (+)",
                               
                               "competitive exclusion" =  "generalist excluded (+)",
                               "competitive exclusion" =  "generalist excluded (-)")) %>%
  group_by(scenario, era, category) %>%
  mutate(prop = ifelse(n() > 1, sum(prop), prop)) %>%
  distinct() %>%
  mutate(colors = fct_recode(colors, 
                             "plum" = "turquoise",
                             "magenta3" = "limegreen")) %>%
  distinct()
levels(proportion_dat$category)

#####################################
# Create color story you want

colors_dat <- data.frame(category = c("coexist","competitive exclusion","specialist extinct","both spp extinct","priority effect"),
                         colors = c("lightblue","magenta3","mediumpurple1","purple","plum"))

# now make sure they appear in the same order
colors_dat$colors <- as.factor((colors_dat$colors))
colors_dat$category <- as.factor(colors_dat$category)

colors_dat$colors <- factor(colors_dat$colors, 
                            levels =c("lightblue","purple","mediumpurple1","magenta3","plum"))

colors_dat$category <- factor(colors_dat$category, 
                              levels = c("coexist","both spp extinct","specialist extinct","competitive exclusion","priority effect"))
levels(colors_dat$colors)
levels(colors_dat$category)

###################################
# Visualize bar graphs

Sub_dat_plot <- proportion_dat %>% 
  filter(comp_scenario %in% c('constant','both \n temperature-dependent')) 
# check things are adding up:
Sub_dat_plot %>%
  group_by(scenario, era) %>%
  summarize(total = sum(prop)) %>%
  filter(!total == 1) # issue is in Rstar inter & intra
Sub_dat_plot %>%
  filter(scenario %in% "Rstar inter & intra" & era %in% 'historic') %>%
  #select(category, prop) %>%
  ungroup() %>%
  mutate(total = sum(prop)) # two competitive exclusions!! BC colors...


# colors in proportion data as well:
# in proportion data as well:
Sub_dat_plot$colors <- factor(Sub_dat_plot$colors,
                              levels = c("lightblue","purple","mediumpurple1","magenta3","plum"))

Sub_dat_plot$category <- factor(Sub_dat_plot$category,
                                levels = c("coexist","both spp extinct","specialist extinct","competitive exclusion","priority effect"))

# levels(colors_dat$colors) == levels(Sub_dat_plot$colors)
# levels(colors_dat$category) == levels(Sub_dat_plot$category)
# 
Sub_dat_plot$comp_scenario <- droplevels(Sub_dat_plot$comp_scenario)
Sub_dat_plot$category <- droplevels(Sub_dat_plot$category)

# gradual first, then abrupt
Sub_dat_plot$theory <- as.factor(Sub_dat_plot$theory)
Sub_dat_plot$theory <- factor(Sub_dat_plot$theory, levels = 
                                c("Gradual", "Abrupt"))

# tags for facets
facet_labels_top <- data.frame(
  category = 'coexist',
  theory = rep(c( "Gradual"), each = 2),  
  comp_scenario = rep(c(levels(Sub_dat_plot$comp_scenario)), times = 1),
  label = c("A","B"))

# tags for facets
facet_labels_bottom <- data.frame(
  category = 'coexist',
  theory = rep(c("Abrupt"), each = 2),  
  comp_scenario = rep(c(levels(Sub_dat_plot$comp_scenario)), times = 1),
  label = c("D", "E"))

# order the comp_scenarios
facet_labels_top$comp_scenario <- factor(facet_labels_top$comp_scenario, levels = 
                                           c('constant', 
                                             "both \n temperature-dependent"))
facet_labels_bottom$comp_scenario <- factor(facet_labels_bottom$comp_scenario, levels = 
                                              c('constant', 
                                                "both \n temperature-dependent"))


# figure
fig3top <- ggplot(filter(Sub_dat_plot, theory %in% 'Gradual'), mapping = aes(x= era, y = prop*100, fill = category)) + 
  geom_col() +
  scale_fill_manual(values = levels(colors_dat$colors)) + 
  theme_light() +
  facet_wrap(~comp_scenario, nrow = 1) +
  labs(x = "", y = "Percent of simulations", fill = "" ) +
  theme(text = element_text(size = 16),
        legend.position ='none',
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(3, "lines")) +
  guides(fill = guide_legend(ncol = 3)) +
  scale_x_discrete(breaks = function(x) x[c(FALSE,TRUE)]) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(data = facet_labels_top, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") 
fig3top

# note that this coloring has a different order so that the legend ordering behaves
fig3bottom <- ggplot(filter(Sub_dat_plot, theory %in% 'Abrupt'), mapping = aes(x= era, y = prop*100, fill = category)) + 
  geom_col() +
  scale_fill_manual(values = c("magenta3", "purple", "lightblue", "plum", "mediumpurple1"),
                    breaks = c('competitive exclusion', 'both spp extinct', 'coexist', 
                               'priority effect', 'specialist extinct'))+ 
  theme_light() +
  facet_wrap(~comp_scenario, nrow = 1) +
  labs(x = "", y = "Percent of simulations", fill = "" ) +
  theme(text = element_text(size = 16),
        legend.position ='bottom',
        legend.justification = "right", 
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(3, "lines")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_x_discrete(breaks = function(x) x[c(FALSE,TRUE)]) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_text(data = facet_labels_bottom, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") 
fig3bottom

###################################
# Visualize lollipop graphs
###################################

# select scenarios to compare:
unique(Sub_dat_plot$scenario)
Sub_dat_plot$scenario <- droplevels(Sub_dat_plot$scenario )
all_diff <- Sub_dat_plot %>%
  # sum duplicate exlcusion categories to be able to widen dataframe
  group_by(era, scenario, category) %>%
  mutate(prop = ifelse(n() > 1, sum(prop), prop)) %>%
  distinct() %>%
  # now prepare to widen 
  ungroup() %>%
  group_by(category) %>%
  distinct() %>%
  pivot_wider(., names_from = 'scenario', values_from = 'prop', values_fill = 0) # is na, turn to 0

within_theories <- all_diff %>%
  group_by(category) %>%
  # stress gradient intercept compared to others:
  mutate(`Gradual: constant - temperature-dependent` = `SGH intercepts` - `SGH inter & intra`) %>%
  # Rstar intercept compared to others:
  mutate(`Abrupt: constant - temperature-dependent` = `Rstar intercepts` - `Rstar inter & intra`) %>%
  distinct() %>%
  select(era, category, #`SGH: constant - interspecific \n temperature-dependent`, 
         # `SGH: constant - intraspecific \n temperature-dependent`, 
         `Gradual: constant - temperature-dependent` , 
         # `R*: constant - interspecific \n temperature-dependent`, 
         #  `R*: constant - intraspecific \n temperature-dependent`,
         `Abrupt: constant - temperature-dependent`) %>%
  pivot_longer(., cols = 3:4, names_to = 'comparing', values_to = 'difference') %>%
  mutate(category = fct_collapse(category, 'extinction' = c("specialist extinct", "both spp extinct"))) %>%
  group_by(era, category, comparing) %>%
  mutate(difference = sum(difference)) 
head(within_theories)

# order facets:
within_theories$comparing <- as.factor(within_theories$comparing)
within_theories$comparing <- factor(within_theories$comparing, levels = 
                                      c( "Gradual: constant - temperature-dependent",
                                         "Abrupt: constant - temperature-dependent"))

# set up tag labels
facet_labels_C <- data.frame(
  comparing = c(levels(within_theories$comparing)[1]), 
  label = c("C"),
  category = 'coexist')
facet_labels_F <- data.frame(
  comparing = c(levels(within_theories$comparing)[2]), 
  label = c("F"),
  category = 'coexist')

# Plot
diff_top <- ggplot(filter(within_theories, comparing %in% levels(within_theories$comparing)[1])) + 
  geom_segment(mapping = aes(x=era, xend=era, y=0, yend=difference*100), color = 'black') +
  geom_point(mapping = aes(x = era, y = difference*100, fill = category),
             size = 3, shape = 21, color = 'black') +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(3, "lines")) +
  labs(x = "", y = "Percent difference", color = '') + # rm \n (constant - temperature-dependent)
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-20, 20)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey') +
  #facet_wrap(~comparing, ncol = 1) +
  scale_fill_manual(values = c('lightblue','purple', 'magenta3', 'plum'))+
  scale_x_discrete(breaks = function(x) x[c(FALSE,TRUE)]) +
  geom_text(data = facet_labels_C, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, color = "black", fontface = "bold")
diff_top

diff_bottom <- ggplot(filter(within_theories, comparing %in% levels(within_theories$comparing)[2])) + 
  geom_segment(mapping = aes(x=era, xend=era, y=0, yend=difference*100), color = 'black') +
  geom_point(mapping = aes(x = era, y = difference*100, fill = category),
             size = 3, shape = 21, color = 'black') +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none',
        text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.y = unit(3, "lines")) +
  labs(x = "", y = "Percent difference", color = '') + # rm \n (constant - temperature-dependent)
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey') +
  #facet_wrap(~comparing, ncol = 1) +
  scale_fill_manual(values = c('lightblue','purple', 'magenta3', 'plum'))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-20, 20)) +
  scale_x_discrete(breaks = function(x) x[c(FALSE,TRUE)]) +
  geom_text(data = facet_labels_F, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, color = "black", fontface = "bold") 
diff_bottom

####################################
# Save each component of figure 2
####################################

#----------------------------------------------------------------------------
jpeg(paste0(dir_string,"/Figures-output/LV Figures/OFFSET Figure 2 - gradual prop.jpeg"), res = 600, width=7, height=3.5, units="in")
fig3top
dev.off()

jpeg(paste0(dir_string,"/Figures-output/LV Figures/OFFSET Figure 2 - abrupt prop.jpeg"), res = 600, width=7, height=4.4, units="in")
fig3bottom
dev.off()

jpeg(paste0(dir_string,"/Figures-output/LV Figures/OFFSET Figure 2 - gradual diff.jpeg"), res = 600, width=3.5, height=3.5, units="in")
diff_top
dev.off()

jpeg(paste0(dir_string,"/Figures-output/LV Figures/OFFSET Figure 2 - abrupt diff.jpeg"), res = 600, width=3.5, height=3.5, units="in")
diff_bottom
dev.off()
#----------------------------------------------------------------------------

################################################################################
# Figure 4 - trends of coexistence loss over warming
# All outcomes compared to historic
################################################################################

# select scenarios to compare:
unique(Sub_dat_plot$scenario)

all_diff_warming <- Sub_dat_plot %>%
  filter(scenario %in% c("Rstar inter & intra", "Rstar intercepts",
                         "SGH inter & intra", "SGH intercepts") ) %>% # try idea for just abrupt temperature dependence first
  # sum duplicate exlcusion categories to be able to widen dataframe
  group_by(era, category, comp_scenario, theory) %>%
  mutate(category = fct_collapse(category, 'extinction' = c("specialist extinct", "both spp extinct"))) %>%
  mutate(prop = ifelse(n() > 1, sum(prop), prop)) %>%
  distinct() %>%
  # now prepare to widen 
  ungroup() %>%
  group_by(category, comp_scenario) %>%
  distinct() %>%
  pivot_wider(., names_from = 'era', values_from = 'prop', values_fill = 0)
all_diff_warming$scenario <- droplevels(all_diff_warming$scenario )

head(all_diff_warming)

# Calculate differences between consecutive temperature columns
era_diffs2 <- all_diff_warming %>%
  group_by(category, comp_scenario) %>%
  mutate(
    "historic to current" = current - historic,
    "historic to 0.5°C" = `0.5°C` - historic,
    "historic to 1°C" = `1°C` - historic,
    "historic to 1.5°C" = `1.5°C` - historic,
    "historic to 2°C" = `2°C` - historic,
    "historic to 2.5°C" = `2.5°C` - historic,
    "historic to 3°C" = `3°C` - historic,
    "historic to 3.5°C" =`3.5°C` - historic,
    "historic to 4°C" = `4°C` - historic,
    "historic to 4.5°C" = `4.5°C` - historic,
    "historic to 5°C" = `5°C` - historic
  ) %>%
  pivot_longer(., cols = 18:28, names_to = 'warming_step', values_to = 'difference') %>%
  select(theory, colors, comp_scenario, category, warming_step, difference)
head(era_diffs2)

# order steps
era_diffs2$warming_step <- factor(era_diffs2$warming_step, levels = c("historic to current",
                                                                      "historic to 0.5°C",
                                                                      "historic to 1°C",
                                                                      "historic to 1.5°C",
                                                                      "historic to 2°C",
                                                                      "historic to 2.5°C",
                                                                      "historic to 3°C",
                                                                      "historic to 3.5°C",
                                                                      "historic to 4°C",
                                                                      "historic to 4.5°C",
                                                                      "historic to 5°C"))

era_diffs2$category <- factor(era_diffs2$category, levels = c('coexist','competitive exclusion', 'extinction', 'priority effect'))
era_trends2 <- ggplot(filter(era_diffs2, !category  %in% 'priority effect')) + 
  # geom_rect(data = rect_data,
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = 'lightblue', alpha = 0.3, color = 'lightblue') +
  geom_line(mapping = aes(x = warming_step, y = difference*100, color = comp_scenario, group = comp_scenario), linewidth = 1) +
  geom_point(mapping = aes(x = warming_step, y = difference*100, fill = comp_scenario),
             size = 3, shape = 21, color = 'black') +
  theme_light() +
  ggh4x::facet_grid2(cols = vars(category), rows = vars(theory), scales = 'free_y', independent = 'y') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'top',
        text = element_text(size = 16)
        #strip.background = element_blank(),
        #strip.text.x = element_blank(),
        #panel.spacing.y = unit(2, "lines")
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "", y = "Percent difference (warming - historic)", color = '') + # rm \n (constant - temperature-dependent)
  #geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey') +
  scale_fill_manual(labels = c('constant competition', 'temperature-dependent competition'),
                    values = c('grey70','orangered'))+
  scale_color_manual(labels = c('constant competition', 'temperature-dependent competition'),
                     values = c('grey70','orangered')) +
  scale_x_discrete(breaks = function(x) x[c(FALSE,TRUE)]) +
  guides(fill = "none")
era_trends2

#----------------------------------------------------------------
jpeg(paste0(dir_string,"/Figures-output/LV Figures/OFFSET Figure 3 - trends.jpeg"), res = 600, width=8, height=8, units="in")
era_trends2
dev.off()
#----------------------------------------------------------------


###################################################
# Figure 3 - coexistence density plots
###################################################

dir_string <- getwd()

source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R"))

###############################
# Prepare parameter_dat
###############################

# Filter niche area (where persistence > 1)
# for coexistence graphs, remove all rows of parameter_dat where a r is < 0,
# as this is indicative of extinction rather than a true exclusion

load(paste0(here::here(),"/Scripts LV/Final dataframes/df_condition.Rdata")) # /January 2025/
load(paste0(here::here(),"/Scripts LV/Final dataframes/parameter_dat_offset.Rdata")) # updated 10-27-2025
source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - offset.R"))

#filter out extinction situations when r > 0
df_params <- parameter_dat_offset %>%
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

df_condition <- df_condition %>%
  filter(niche_diff >= -0.181 & niche_diff <= 0.988)

coex_graph_all <- 
  ggplot() +
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
path <- paste0(here::here(),"/Figures-output/LV Figures/OFFSET Figure 2 - new coex graph.jpeg")
jpeg(path, res = 600, width=6, height=7, units="in")
coex_graph_all 
Sys.sleep(3)
dev.off()
#--------------------------------------------------------------------

##########################################
# How much extinction in each era 
# by theory by competition_type facet?
##########################################

load(here::here("Scripts LV/Final dataframes/proportion_dat_offset.Rdata"))

pie_limits <- proportion_dat_offset %>%
  group_by(era, theory, comp_scenario) %>%
  filter(era %in% c('historic', '2°C') & comp_scenario %in% c('both \n temperature-dependent')) %>%
  filter(scenario %in% c('Rstar inter & intra', 'SGH inter & intra',
                         'Rstar intercepts', 'SGH intercepts')) %>%
  mutate(category = fct_collapse(category,
                                 'extinct' = c('both spp extinct', 'specialist extinct'),
                                 'within thermal limits' = c('coexist','priority effect','generalist excluded (+)','generalist excluded (-)','specialist excluded (+)'))) %>%
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
path <- paste0(here::here(),"/Figures-output/LV Figures/OFFSET PIE_fig3.jpeg")
jpeg(path, res = 600, width=7, height=6, units="in")
pie_figs 
dev.off()
#--------------------------------------------------------------------