
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

load(paste0(here::here(),"/Scripts LV/Final dataframes/df_condition.Rdata")) # /January 2025/
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

levels(df_params$scenario)
temp <- df_params %>% 
 # filter(era %in% c('historic','1°C','2°C','3°C')) %>%
  filter(scenario %in% c('Rstar intercepts', 'SGH intercepts')) %>% # results in 274,090 rows 
  mutate(theory = fct_recode(scenario, 
                             "Abrupt constant" = 'Rstar intercepts', 
                             "Gradual constant" = 'SGH intercepts'))
head(temp)
# necessary changes to make insets work later in the script:
temp$era <- droplevels(temp$era)
temp$theory <- droplevels(temp$theory)
temp$scenario <- droplevels(temp$scenario)
temp <- as.data.frame(temp)

# set up tag labels
# tags for facets
facet_labels <- data.frame(
  niche_d = NA,
  fit_d_kj = NA,
  theory = rep(c( "Gradual constant","Abrupt constant"), each = 4),  
  era = rep(c(levels(df_params$era)), times = 2),
  label = c("A","C", "B", "D", "E", "F", "G", "H"))

# facet labels 
facet_labels$theory <- factor(facet_labels$theory, levels = 
                                c("Gradual constant", "Abrupt constant"))

# order the comp_scenarios
facet_labels$theory <- factor(facet_labels$theory, levels = 
                                c("Gradual constant", "Abrupt constant"))
# set era order:
facet_labels$era <- factor(facet_labels$era, levels = c(levels(df_params$era)))

# Plot
coex_graph_all <- 
  ggplot() +
  
  # coexistence box
  geom_ribbon(data = df_condition, aes(x = niche_diff, ymin = rho, ymax = one_over_rho),
              fill = 'lightblue', alpha = 0.3) +
  
  # 2D density contours for parameter draws
  geom_density_2d_filled(
    data = temp,
    aes(x = niche_d, y = fit_d_kj), #  fill = era 
    alpha = 0.8, contour_var = "ndensity",
    breaks = seq(0.1, 1, by = 0.1) 
  ) +
  
  # coexistence boundaries (lay on top of densities)
  geom_line(data = df_condition, aes(x = niche_diff, y = rho), color = 'magenta3') +
  geom_line(data = df_condition, aes(x = niche_diff, y = one_over_rho), color = 'magenta3') +
  
  #scale_fill_brewer(palette = "RdPu", direction = -1) + # GnBu
  scale_fill_grey(start = 0.2,
                  end = 0.8,
                  aesthetics = "fill") + 
  # facet and themes
  facet_grid(rows = vars(theory), cols=vars(era)) +
  scale_y_log10(name = expression(paste("Fitness difference: ", kappa[j]/kappa[i]))) +
  xlab(expression(paste("Niche Difference: ", 1-rho))) +
  labs(fill = "Density") +
  theme_light() +
  theme(legend.position = "right",
        ggside.panel.scale = 0.3,
        axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.x.top   = element_blank(),
        axis.line.y.right = element_blank(),
        text = element_text(size = 16)) +
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
  filter(era %in% c('historic','1°C','2°C','3°C')) %>%
  filter(scenario %in% c('Rstar intercepts', 'SGH intercepts')) %>% # results in 274,090 rows 
  mutate(theory = fct_recode(scenario, 
                             "Abrupt constant" = 'Rstar intercepts', 
                             "Gradual constant" = 'SGH intercepts')) %>%
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
    ymin = 100, ymax = 1000, xmin = -0.2, xmax = 0.5 #  ymin = 100, ymax = 1000, xmin = -0.2, xmax = 0.5
  ))

# Full figure!
coex_graph_all + insets

#----------------------------------------------------------------------
path <- paste0(here::here(),"/Figures-output/Figure 2 - coex graph constants.jpeg")
jpeg(path, res = 600, width=12, height=8, units="in")
coex_graph_all + insets
Sys.sleep(3)
dev.off()
#--------------------------------------------------------------------
