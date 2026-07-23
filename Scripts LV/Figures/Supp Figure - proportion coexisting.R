###########################################################################
# Proportion of runs coexisting, excluded, and priority effects across eras
###########################################################################

# FIGURE 3

#source(paste0(dir_string,"/Scripts LV/Figures/Niche v fitness figures/Source - proportions for figures.R"))
load(here::here("Scripts LV/Final dataframes/proportion_dat.Rdata"))

#####################
# Visualize
#####################

Sub_dat_plot <- proportion_dat %>% 
  filter(comp_scenario %in% c('constant','both \n temperature-dependent')) 

Sub_dat_plot$comp_scenario <- droplevels(Sub_dat_plot$comp_scenario)
# tags for facets
facet_labels <- data.frame(
  category = NA,
  theory = rep(c( "Gradual","Abrupt"), each = 2),  
  comp_scenario = rep(c(levels(Sub_dat_plot$comp_scenario)), times = 2),
  label = c("A","B", "C", "D"))
# order the comp_scenarios
facet_labels$comp_scenario <- factor(facet_labels$comp_scenario, levels = 
                                       c('constant', 
                                         "both \n temperature-dependent"))
facet_labels$theory <- factor(facet_labels$theory, levels = 
                                c("Gradual", "Abrupt"))

# figure
fig3 <- ggplot(Sub_dat_plot, mapping = aes(x= era, y = prop, fill = category)) + 
  geom_col() +
  scale_fill_manual(values = levels(full_dat_plot$colors)) + 
  theme_light() +
  facet_grid(rows = vars(comp_scenario), cols = vars(theory)) +
  labs(x = "", y = "Proportion of simulations", fill = "" ) +
  theme(text = element_text(size = 16),
        legend.position ='top',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(nrow = 2)) +
  geom_text(data = facet_labels, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") 
fig3

# FIXME remove NA's: they exist because of facet_labels.

#-----------------------------------------------------------------
path <- paste0(dir_string,"/Figures-output/LV Figures/proportion_coexisting/Figure_proportions_coex.jpeg")
jpeg(path, res = 600, width=6, height=8, units="in")
fig3
dev.off()

path <- paste0(dir_string,"/Figures-output/LV Figures/proportion_coexisting/Figure_proportions_coex.pdf")
pdf(path, width=6, height=8)
fig3
dev.off()
#---------------------------------------------------------------


######################################
# For supplement, all combinations:
######################################

full_dat_plot$colors <- NA

full_dat_plot$colors[full_dat_plot$category ==  "coexist"] <- "lightblue"
full_dat_plot$colors[full_dat_plot$category ==  "specialist excluded"] <- 'limegreen'
full_dat_plot$colors[full_dat_plot$category == "generalist excluded"] <- "magenta3"
full_dat_plot$colors[full_dat_plot$category == "specialist extinct"] <- 'mediumpurple1'
full_dat_plot$colors[full_dat_plot$category == "both spp extinct"]  <-'purple'
full_dat_plot$colors[full_dat_plot$category == "priority effect"] <- "turquoise"
full_dat_plot$colors <- as.factor(full_dat_plot$colors)

# now make sure they appear in the same order
full_dat_plot$colors <- factor(full_dat_plot$colors, 
                                   levels =  c("lightblue","magenta3","limegreen","mediumpurple1","purple","turquoise"))

full_dat_plot$colors <- droplevels(full_dat_plot$colors) # to keep colors what is filtered

# tags for facets
facet_labels <- data.frame(
  category = NA,
  theory = rep(c( "Gradual","Abrupt"), each = 4),  
  comp_scenario = rep(c(levels(full_dat_plot$comp_scenario)), times = 2),
  label = c("A","C", "E", "G", "B", "D", "F", "H"))
# order the comp_scenarios
facet_labels$comp_scenario <- factor(facet_labels$comp_scenario, levels = 
                                        c('constants', 
                                          "interspecific \n temperature-dependent",
                                          "intraspecific \n temperature-dependent",
                                          "both \n temperature-dependent"))
facet_labels$theory <- factor(facet_labels$theory, levels = 
                                 c("Gradual", "Abrupt"))

# figure
fig3_supplement <- ggplot(full_dat_plot, mapping = aes(x= era, y = prop, fill = category)) + 
  geom_col() +
  scale_fill_manual(values = levels(full_dat_plot$colors)) + 
  theme_light() +
  facet_grid(rows = vars(comp_scenario), cols = vars(theory)) +
  labs(x = "", y = "Proportion of simulations", fill = "" ) +
  theme(text = element_text(size = 16),
        legend.position ='top',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(nrow = 2)) +
  geom_text(data = facet_labels, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.2, vjust = 1.2, size = 5, fontface = "bold") 
fig3_supplement

# FIXME remove NA's

#-----------------------------------------------------------------
path <- paste0(dir_string,"/Figures-output/LV Figures/proportion_coexisting/Supp_all_scenarios_coex_prop.jpeg")
jpeg(path, res = 600, width=6, height=11, units="in")
fig3_supplement
dev.off()

path <- paste0(dir_string,"/Figures-output/LV Figures/proportion_coexisting/Supp_all_scenarios_coex_prop.pdf")
pdf(path, width=6, height=11)
fig3_supplement
dev.off()
#---------------------------------------------------------------

#-----------------------------------------------------
# Loop through to create figures for each scenario
#-----------------------------------------------------
# 
# k <- 1
# list_prop_figs <- list()
# 
# for(i in list_dat){
# 
#   df_cat_summarized <- i
#   
#   # remove replicate entries as it confuses geom_col()
#   df_cat_summarized <- df_cat_summarized %>%
#     select(-combo, -run, -count_cat, -total_rep) %>%
#     distinct()
#   
#   ############################################
#   # Specifications for the plot
#   ############################################
#   
# # order the eras:
# df_cat_summarized$era <- factor(df_cat_summarized$era, 
#                                 levels = c('historic', 'current', '0.5 C','1 C','1.5 C',
#                                            '2 C','2.5 C','3 C',
#                                            '3.5 C','4 C','4.5 C','5 C'))
# 
# ######################################
#   # combine exclusion categories:
#   
#   # convert to character temporarily to allow modifications
#   df_cat_summarized$category <- as.character(df_cat_summarized$category)
#   
#   # search for generalist excluded and change names
#   for(z in 1:length(df_cat_summarized$category)){
#   df_cat_summarized$category[z] <- ifelse(df_cat_summarized$category[z] %in% "generalist excluded (+)" | df_cat_summarized$category[z] %in% "generalist excluded (-)", 
#                                           as.character('generalist excluded'), 
#                                           as.character(df_cat_summarized$category[z]))
# 
#   }
#   
#   # search for specialist excluded and change names
#   for(z in 1:length(df_cat_summarized$category)){
#     df_cat_summarized$category[z] <- ifelse(df_cat_summarized$category[z] %in% "specialist excluded (+)" | df_cat_summarized$category[z] %in% "specialist excluded (-)", 
#                                             as.character('specialist excluded'), 
#                                             as.character(df_cat_summarized$category[z]))
#     
#   }
#   
# # order the categories
# df_cat_summarized$category <- factor(df_cat_summarized$category, 
#                                 levels = c( "coexist","generalist excluded",'specialist excluded', "specialist extinct", "both spp extinct", "priority effect"))
# 
#   #########################
#   # Visualize
#   #########################
# 
# df_cat_summarized$colors <- NA
# 
# df_cat_summarized$colors[df_cat_summarized$category ==  "coexist"] <- "lightblue"
# df_cat_summarized$colors[df_cat_summarized$category ==  "specialist excluded"] <- 'limegreen'
# df_cat_summarized$colors[df_cat_summarized$category == "generalist excluded"] <- "magenta3"
# df_cat_summarized$colors[df_cat_summarized$category == "specialist extinct"] <- 'mediumpurple1'
# df_cat_summarized$colors[df_cat_summarized$category == "both spp extinct"]  <-'purple'
# df_cat_summarized$colors[df_cat_summarized$category == "priority effect"] <- "turquoise"
# df_cat_summarized$colors <- as.factor(df_cat_summarized$colors)
# 
# # now make sure they appear in the same order
# df_cat_summarized$colors <- factor(df_cat_summarized$colors, 
#                                    levels =  c("lightblue","magenta3","limegreen","mediumpurple1","purple","turquoise"))
# 
# df_cat_summarized$colors <- droplevels(df_cat_summarized$colors) # to keep colors what is filtered
# 
# cat_fig <- ggplot(df_cat_summarized, mapping = aes(x= era, y = prop, fill = category)) + 
#   geom_col() +
#   scale_fill_manual(values = levels(df_cat_summarized$colors)) + 
#   theme_classic() +
#   labs(x = "", y = "Proportion", fill = "" ) +
#   theme(text = element_text(size = 16),
#         legend.position ='right',
#         axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ggtitle(as.character(unique(df_cat_summarized$scenario))) 
# 
# # save all as list so you can call into one figure:
# list_prop_figs[[k]] <- cat_fig
# 
# path <- paste0(dir_string,"/Figures-output/LV Figures/proportion_coexisting/", as.character(unique(df_cat_summarized$scenario)), " coex prop.jpeg")
# jpeg(path, res = 600, width=6, height=5, units="in")
# print(cat_fig)
# Sys.sleep(3)
# dev.off()
# 
# k <- k+1
# }
# 
# ############################
# # all scenarios at once:
# 
# library(patchwork)
# library(cowplot)
# 
# all_prop_fig <- list_prop_figs[[1]] + 
#   list_prop_figs[[2]] + 
#   list_prop_figs[[3]] +
#   list_prop_figs[[4]] +
#   list_prop_figs[[5]] +
#   list_prop_figs[[6]] +
#   list_prop_figs[[7]] +
#   list_prop_figs[[8]] + plot_annotation(tag_levels = "A") +
#   plot_layout(ncol = 2, guides = 'collect')
# all_prop_fig # going to have to fix these legends manually. Spent an hour trying to fix.
# 
# #--------------------------------------------------------------------
# path <- paste0(dir_string,"/Figures-output/LV Figures/proportion_coexisting/all_scenarios_coex_prop.jpeg")
# jpeg(path, res = 600, width=10, height=18, units="in")
# all_prop_fig
# dev.off()
# 
# # save pdf in inkscape:
# # pdf(path, width=10, height=18)
# # all_prop_fig
# # dev.off()
# #--------------------------------------------------------------------
# 
