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

library(dplyr)
library(purrr)
library(ggplot2)
library(MASS)      # kde2d
library(grid)       # grobs for insets
library(gridExtra)  # optional, not required

## ------------------------------------------------------------------
## 0. Coexistence envelope (you already compute this as df_condition)
## ------------------------------------------------------------------
niche_diff <- seq(from = -.25, to = 1, by = 0.001)
rho <- 1 - niche_diff

df_condition <- data.frame(
  niche_diff   = niche_diff,
  rho          = rho,
  one_over_rho = 1 / rho
) %>%
  filter(niche_diff >= -0.181 & niche_diff <= 0.986)


## ------------------------------------------------------------------
## 1. Color ramps, keyed to competition_type (not era!)
## ------------------------------------------------------------------
n_contour_levels <- 20   # matches your original breaks = seq(0.1,1,length.out=14)

get_ramp <- function(type, n = n_contour_levels) {
  if (type == "Constant") {
    colorRampPalette(c("grey20", "grey95"))(n)
  } else if (type == "Temperature-dependent") {
    colorRampPalette(c("orangered2", "gold"))(n)
  } else {
    stop("Unrecognized competition_type: ", type)
  }
}

## ------------------------------------------------------------------
## 2. KDE -> contour polygons, one call per (theory, era, competition_type)
## ------------------------------------------------------------------
## kde_n     : grid resolution for kde2d (higher = smoother contour shapes)
## kde_h     : bandwidth passed to kde2d (higher = smoother/blobbier density);
##             leave NULL to use MASS::kde2d's default (~ Silverman-ish rule)
make_contour_polys <- function(df, type, n_levels = n_contour_levels,
                               kde_n = 300, kde_h = NULL) {
  x <- df$niche_d
  y <- log10(df$fit_d_kj)   # KDE on log10 scale since y-axis is log10
  
  if (is.null(kde_h)) {
    kd <- MASS::kde2d(x, y, n = kde_n,
                      lims = c(range(x, na.rm = TRUE), range(y, na.rm = TRUE)))
  } else {
    kd <- MASS::kde2d(x, y, n = kde_n, h = kde_h,
                      lims = c(range(x, na.rm = TRUE), range(y, na.rm = TRUE)))
  }
  
  cl <- grDevices::contourLines(kd$x, kd$y, kd$z, nlevels = n_levels)
  if (length(cl) == 0) return(NULL)
  
  pal <- get_ramp(type, length(cl))
  
  purrr::imap_dfr(cl, function(line, i) {
    data.frame(
      x     = line$x,
      y     = 10^line$y,
      level = i,
      fill  = pal[i]
    )
  })
}

poly_df <- temp %>%
  group_by(theory, era, competition_type) %>%
  group_modify(function(df, key) {
    make_contour_polys(df, type = as.character(key$competition_type))
  }) %>%
  ungroup() %>%
  mutate(poly_id = interaction(theory, era, competition_type, level, drop = TRUE))

## ------------------------------------------------------------------
## 3. Assemble the plot
## ------------------------------------------------------------------

coex_graph_pretty <-
  ggplot() +
  
  # density contour fill (Constant = grey ramp, Temperature-dependent = orange/gold ramp)
  geom_polygon(data = poly_df,
               aes(x = x, y = y, group = poly_id, fill = fill),
               color = NA) +
  scale_fill_identity() +
  
  # dummy layer to recreate the legend (invisible points, manual color key)
  geom_line(data = data.frame(x = NA, y = NA,
                              type = c("constant", "temperature-dependent")),
            aes(x = x, y = y, color = type), linewidth = 0) +
  scale_color_manual(
    values = c("constant" = "grey50", "temperature-dependent" = "orangered2"),
    labels = c("constant competition", "temperature-dependent competition"),
    name = NULL
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  
  # coexistence envelope boundary lines
  geom_line(data = df_condition, aes(x = niche_diff, y = rho),
            color = "black", linewidth = 0.5) +
  geom_line(data = df_condition, aes(x = niche_diff, y = one_over_rho),
            color = "black", linewidth = 0.5) +
  
  facet_grid(rows = vars(theory), cols = vars(era)) +
  
  scale_y_log10(name = expression(paste("Fitness difference: ", kappa[j]/kappa[i])),
                limits = c(0.05, 20)) +
  xlab(expression(paste("Niche Difference: ", 1 - rho))) +
  
  theme_light() +
  theme(legend.position = "top",
        text = element_text(size = 16))

coex_graph_pretty

## ------------------------------------------------------------------
## 4. Save
## ------------------------------------------------------------------
path <- paste0(here::here(), "/Figures-output/Figure 4 - pretty coex graph v2.jpeg")
ggsave(path, coex_graph_pretty, width = 6, height = 7, units = "in", dpi = 600)
