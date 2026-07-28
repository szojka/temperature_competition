
###################################################
# Figures depicting niche & fitness differences 
# on coexistence space, FOR CONSTANT COMPETITION
# and as proportions changing over eras
###################################################

dir_string <- getwd()

source(paste0(dir_string,"/Scripts LV/Source/Source - set basic controls.R"))

###############################
# Prepare parameter_dat
###############################

load(paste0(here::here(),"/Scripts LV/Final dataframes/parameter_dat.Rdata")) # updated 10-27-2025

# Filter niche area (where persistence > 1)
# for coexistence graphs, remove all rows of parameter_dat where a r is < 0,
# as this is indicative of extinction rather than a true exclusion
source(paste0(here::here(), "/Scripts LV/Intrinsic growth/r - generalist v specialist.R"))

#####################################################################
# filter parameters to be within feasible persistence situations

#filter out extinction situations below min and max temperature values (in other words, when r > 0)
df_params <- parameter_dat %>%
  dplyr::filter(env %in% seq(min_spi,max_spi, by = 0.1)) %>%# min_spj max_spj =  temperature of niche breadth for spp j
  dplyr::filter(env %in% seq(min_spj,max_spj, by = 0.1))# min_spi max_spi = temperature of niche breadth for spp i

#########################
# COEXISTENCE PLOT 
#########################

library(viridis)
library(ggplot2)
library(ggside)
library(purrr) # for insets

##################################################
# FULL FIGURE WITH SCENARIOS AS FACETS
temp <- df_params %>% 
  filter(era %in% c('historic','current','1°C','2°C','3°C','4°C','5°C')) %>%
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

# define coex boundary:
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

# set up tag labels
# tags for facets
facet_labels <- data.frame(
  niche_d = NA,
  fit_d_kj = NA,
  theory = rep(c( "Gradual","Abrupt"), each = 7),  
  era = rep(c(levels(temp$era)), times = 2),
  label = c("A","B", "C", "D", "E", "F", "G", 
            "H", "I", "J", "K", "L", "M", "N"))

# facet labels 
facet_labels$theory <- factor(facet_labels$theory, levels = 
                                c("Gradual", "Abrupt"))

# set era order:
facet_labels$era <- factor(facet_labels$era, levels = c(levels(df_params$era)[1],levels(df_params$era)[2],levels(df_params$era)[4],levels(df_params$era)[6],levels(df_params$era)[8],levels(df_params$era)[10],levels(df_params$era)[12]))

# Plot
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
  geom_line(data = df_condition, aes(x = niche_diff, y = one_over_rho), color = 'black', linewidth = .5) +
  # tag labels
  geom_text(data = facet_labels, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, color = "black", fontface = "bold")
coex_graph_all

########################################
# Prepare insets of proportions
########################################

# Source for proportions
load(paste0(here::here(),"/Scripts LV/Final dataframes/proportion_dat.Rdata")) # updated 10-27-2025

temp_pie <- proportion_dat %>% 
  filter(era %in% c('historic','current','1°C','2°C','3°C','4°C','5°C')) %>%
  filter(scenario %in% c('Rstar intercepts', 'SGH intercepts')) %>% # results in 274,090 rows 
  mutate(theory = fct_recode(scenario, 
                             "Abrupt" = 'Rstar intercepts', 
                             "Gradual" = 'SGH intercepts')) %>%
  # simplify categories
  mutate(category = fct_recode(category,
                               "extinction" = "both spp extinct",
                               "extinction" = "specialist extinct",
                               "coexistence" = "coexist",
                               "coexistence" = "priority effect",
                               "exclusion" = "specialist excluded (+)",
                               "exclusion" = "specialist excluded (-)",
                               "exclusion" =  "generalist excluded (+)",
                               "exclusion" =  "generalist excluded (-)")) %>%
  select(era, theory, category, prop) %>%
  group_by(era, theory, category) %>%
  mutate(total_prop = sum(prop)) %>%
  select(-prop) %>%
  distinct()

#----------------------------------------------------------------------
# Insets of pie charts into top left of coexistence chart
#--------------------------------------------------------
# used https://www.blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/

## A function to plot the inset
get_inset <- function(df) {
  ggplot(df, aes(fill = category, y = total_prop, x = "")) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    guides(fill = FALSE) +
    theme_void() +
    scale_fill_manual(values = c("purple", "lightblue", "magenta3"))
}

# This function allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(data = data, stat = StatIdentity, position = PositionIdentity,
        geom = ggplot2:::GeomCustomAnn, inherit.aes = TRUE,
        params = list(grob = grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
}

insets <- temp_pie %>%
  split(f = list(.$theory, .$era)) %>%
  purrr::map(~annotation_custom2(
    grob = ggplotGrob(get_inset(.)),
    data = data.frame(theory = unique(.$theory),
                      era = unique(.$era)),
    ymin = 1.1, ymax = 10, xmin = -0.2, xmax = 0.5 #  ymin = 100, ymax = 1000, xmin = -0.2, xmax = 0.5
  ))

# Full figure!
coex_graph_all + insets

#----------------------------------------------------------------------
path <- paste0(here::here(),"/Figures-output/Supp Figure - coex graph all.jpeg")
jpeg(path, res = 600, width=14, height=7, units="in")
coex_graph_all + insets
Sys.sleep(3)
dev.off()
#--------------------------------------------------------------------
