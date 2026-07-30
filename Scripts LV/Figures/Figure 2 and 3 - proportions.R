###############################################################################################
# Figure 2 - proportions of simulation outcomes for constant and temperature-dependent competition
# Figure 3 - abrupt loss of coexistence from warming step to warming step
###############################################################################################

library(tidyverse)
library(patchwork)

dir_string <- getwd()

# load proportion data
load(here::here("Scripts LV/Final dataframes/proportion_dat.Rdata"))


proportion_dat <- proportion_dat %>%
  mutate(category = fct_recode(category,
                             "competitive exclusion" = "specialist excluded (+)",
                             "competitive exclusion" = "specialist excluded (-)",
                             "competitive exclusion" =  "generalist excluded (+)",
                             "competitive exclusion" =  "generalist excluded (-)")) %>%
  group_by(scenario, era, category) %>%
  mutate(prop = ifelse(n() > 1, sum(prop), prop)) %>%
  distinct() %>%
  mutate(colors = fct_recode(colors, 
                             "plum" = "turquoise",
                             "magenta3" = "limegreen")) %>%
  distinct()

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
fig2top <- ggplot(filter(Sub_dat_plot, theory %in% 'Gradual'), mapping = aes(x= era, y = prop*100, fill = category)) + 
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
            hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") +
  # thermal limits
  geom_vline(xintercept = c('3°C','4°C'), color = c('midnightblue',"steelblue",'midnightblue',"steelblue"), linewidth = 1, linetype = 'longdash')

fig2top

# note that this coloring has a different order so that the legend ordering behaves
fig2bottom <- ggplot(filter(Sub_dat_plot, theory %in% 'Abrupt'), mapping = aes(x= era, y = prop*100, fill = category)) + 
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
            hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") +
  # thermal limits
  geom_vline(xintercept = c('3°C','4°C'), color = c('midnightblue',"steelblue",'midnightblue',"steelblue"), linewidth = 1, linetype = 'longdash')
fig2bottom

#################################
# Results for figure 2 AB, DE
#################################

# how much coexistence under historical conditions in constant and TD situations?
Sub_dat_plot %>%
  filter(era %in% 'historic' & category %in% 'coexist') %>%
  mutate(prop = prop*100)

# often did the generalist outcompete the specialist, and vis versa?
load(here::here("Scripts LV/Final dataframes/proportion_dat.Rdata"))
specialist_v_generalist_dat <- proportion_dat %>%
  filter(comp_scenario %in% c('constant','both \n temperature-dependent')) %>%
  mutate(category = fct_recode(category,
                               "specialist excluded" = "specialist excluded (+)",
                               "specialist excluded" = "specialist excluded (-)",
                               "generalist excluded" =  "generalist excluded (+)",
                               "generalist excluded" =  "generalist excluded (-)")) %>%
  group_by(scenario, era, category) %>%
  mutate(prop = ifelse(n() > 1, sum(prop), prop)) %>%
  distinct() %>%
  group_by(category) %>%
  mutate(prop = prop*100,
         mean_comp_outcome = mean(prop),
         sd_comp_outcome = sd(prop)) %>%
  ungroup() %>%
  select(category, mean_comp_outcome, sd_comp_outcome) %>%
  distinct()
specialist_v_generalist_dat

# how often did priority effects occur?
Sub_dat_plot %>%
  filter(category %in% 'priority effect') %>%
  ungroup() %>%
  mutate(prop = prop*100,
         mean_PE = mean(prop),
         sd_PE = sd(prop),
         min_PE = min(prop),
         max_PE = max(prop)) %>%
  select(category, mean_PE, sd_PE,min_PE, max_PE) %>%
  distinct()

# In the gradual TD scenario, how much higher was coexistence in temperature-dependent compared to constant?
Sub_dat_plot %>%
  filter(category %in% 'coexist' & theory %in% 'Gradual', era %in% 'historic') %>%
  mutate(prop = prop*100)
# 90.5 98.5

Sub_dat_plot %>%
  filter(category %in% 'coexist' & theory %in% 'Abrupt', era %in% 'historic') %>%
  mutate(prop = prop*100)

# how do extinction rates for each species change based on era (warming period)?
# summarized across scenario
extinct_dat <- Sub_dat_plot %>%
  filter(category %in% c('specialist extinct', 'both spp extinct') & era %in% c('1.5°C', '2°C', '2.5°C', '3°C')) %>%
  mutate(prop = prop*100) %>%
  group_by(category, era) %>%
  mutate(mean_extinct = mean(prop),
         sd_extinct = sd(prop)) %>%
  select(category, era, mean_extinct, sd_extinct) %>%
  distinct()
extinct_dat

# When do extinction cross 50% threshold, for each facet of figure 2?
extinct_dat2 <- Sub_dat_plot %>%
  filter(category %in% c('specialist extinct', 'both spp extinct'))  %>%
  mutate(prop = prop*100) %>%
  group_by(scenario, era) %>%
  mutate(total_extinct = sum(prop)) %>%
  select(scenario, era, total_extinct) %>%
  distinct()
extinct_dat2
# view(extinct_dat2)

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

###################################
# RESULTS for figure C, F
###################################

# Determing the difference at each era
within_theories %>%
  filter(category %in% 'coexist') %>%
  mutate(difference = difference*100)
  view()

# In the gradual TD scenario, how much did the difference bw constant and TD coexistence shrink by degree?
within_theories %>%
  filter(comparing %in% levels(within_theories$comparing)[1] & category %in% 'coexist') %>%
  # mean shrinking:
  ungroup() %>%
  mutate(difference = difference * 100,
         mean_coexist_diff = mean(difference),
         sd_coexist_diff = sd(difference)) %>%
  select(mean_coexist_diff,sd_coexist_diff) %>%
  distinct()

# In the abrupt scenario, how much disagreement was there at 2.5C warming?
within_theories %>%
  filter(comparing %in% levels(within_theories$comparing)[2] & era %in% '2.5°C') %>%
  ungroup() %>%
  mutate(difference = difference * 100) %>%
  distinct()

within_theories %>%
  filter(comparing %in% levels(within_theories$comparing)[2] & era %in% c('1°C','4°C')) %>%
  ungroup() %>%
  mutate(difference = difference * 100) %>%
  distinct()

####################################
# Save each component of figure 2
####################################

#----------------------------------------------------------------------------
jpeg(paste0(dir_string,"/Figures-output/Figure 2 - gradual prop.jpeg"), res = 600, width=7, height=3.5, units="in")
fig2top
dev.off()

jpeg(paste0(dir_string,"/Figures-output/Figure 2 - abrupt prop.jpeg"), res = 600, width=7, height=4.4, units="in")
fig2bottom
dev.off()

jpeg(paste0(dir_string,"/Figures-output/Figure 2 - gradual diff.jpeg"), res = 600, width=3.5, height=3.5, units="in")
diff_top
dev.off()

jpeg(paste0(dir_string,"/Figures-output/Figure 2 - abrupt diff.jpeg"), res = 600, width=3.5, height=3.5, units="in")
diff_bottom
dev.off()
#----------------------------------------------------------------------------

################################################################################
# Figure 3 - trends of coexistence loss over warming
# All outcomes compared to historic
################################################################################

# select scenarios to compare:
unique(Sub_dat_plot$scenario)

all_diff_warming <- Sub_dat_plot %>%
  filter(scenario %in% c("Rstar inter & intra", "Rstar intercepts","SGH inter & intra", "SGH intercepts") ) %>% # try idea for just abrupt temperature dependence first
  # sum duplicate exlcusion categories to be able to widen dataframe
  group_by(era, theory, category, comp_scenario) %>%
  mutate(category = fct_collapse(category, 'extinction' = c("specialist extinct", "both spp extinct"))) %>%
  mutate(prop = ifelse(n() > 1, sum(prop), prop)) %>%
  distinct() %>%
  # now prepare to widen 
  ungroup() %>%
  group_by(theory, category, comp_scenario) %>%
  distinct() %>%
  pivot_wider(., names_from = 'era', values_from = 'prop', values_fill = 0)
all_diff_warming$scenario <- droplevels(all_diff_warming$scenario )

head(all_diff_warming)

# Calculate differences between consecutive temperature columns
era_diffs2 <- all_diff_warming %>%
  group_by(category, theory, comp_scenario) %>%
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

# PLOT
era_trends2 <- ggplot(filter(era_diffs2, !category  %in% 'priority effect')) + 
  # geom_rect(data = rect_data,
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
  #           fill = 'lightblue', alpha = 0.3, color = 'lightblue') +
  geom_line(mapping = aes(x = warming_step, y = difference*100, color = comp_scenario, group = comp_scenario), linewidth = 1) +
  geom_point(mapping = aes(x = warming_step, y = difference*100, fill = comp_scenario),
             size = 3, shape = 21, color = 'black') +
  theme_light() +
  facet_grid(cols = vars(theory), rows = vars(category), scales = 'free_y') +
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
  guides(fill = "none") +
  # thermal limits
  geom_vline(xintercept = c('historic to 3°C','historic to 4°C'), color = c(rep(c('midnightblue',"steelblue"), times = 6)), linewidth = 1, linetype = 'longdash')
era_trends2

#----------------------------------------------------------------
jpeg(paste0(dir_string,"/Figures-output/Figure 3 - trends.jpeg"), res = 600, width=6, height=8, units="in")
era_trends2
dev.off()

#----------------------------------------------------------------
#############################################
# Results for figure 3
#############################################

era_diffs2 %>%
  filter(category  %in% 'coexist') %>%
  mutate(difference = difference*100) %>%
  view()




