#Need to acquire global CDR sums

#----Re-run_results_as_needed----
# source(here::here("scripts",
#                   "analyze_CDR.R"))


#----Join_technical_potential_and_geophysical_potential----
CDR_total_df <- 
  left_join(CDR_tech_potential_sum,
            CDR_geophysical_potential_sum,
            by = "model") %>% 
  select(c(model,contains("ub"))) %>% 
  pivot_longer(cols = contains("ub"),
               names_to = "method",
               values_to = "CDR") %>% 
  mutate(algae = case_when(str_detect(string = model, pattern = "atkinson") == TRUE ~ "macroalgae",
                           TRUE ~ "microalgae"))


#----Set_plot_details----
y_lims <- c(0, 0.25)
y_breaks <-seq(0,0.25, by = 0.05)

#----Build_microalgal_plot----
microalgae_CDR_sums_plots <-
  CDR_total_df %>% 
  filter(algae == "microalgae") %>% 
  ggplot(aes(x = model)) +
  geom_col(aes(y = CDR, 
               fill = method),
           position = position_dodge2()) +
  scale_fill_viridis(discrete = TRUE,
                     name = "Pumping Depth",
                     labels = depth_category_labels) +
  scale_x_discrete(name = element_blank(),
                   labels = c("Galbraith and Martiny (2015)",
                              "Garcia et al. (2018)",
                              "Redfield et al. (X)",
                              "Redfield P-limited")) +
  scale_y_continuous(name = expression(Carbon~Dioxide~Removal~(Gt~CO[2]~yr^{-1})),
                     limits = y_lims,
                     breaks = y_breaks) +
  ggtitle("Microalgal Growth Models") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -15),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))
  
  

#----Build_macroalgae_plot----
macroalgae_CDR_sums_plots <-
  CDR_total_df %>% 
  filter(algae == "macroalgae") %>% 
  mutate(model = factor(model, levels = c("atkinson_q1",
                                          "atkinson_med",
                                          "atkinson_q3"))) %>% 
  ggplot(aes(x = model)) +
  geom_col(aes(y = CDR, 
               fill = method),
           position = position_dodge2()) +
  scale_fill_viridis(discrete = TRUE,
                     name = "Pumping Depth",
                     labels = c("<500 (m)",
                                "All Depths")) +
  scale_x_discrete(name = element_blank(),
                   labels = c("Q1 N:P",
                              "Median N:P",
                              "Q3 N:P")) +
  scale_y_continuous(name = expression(Carbon~Dioxide~Removal~(Gt~CO[2]~yr^{-1})),
                     limits = y_lims,
                     breaks = y_breaks) +
  ggtitle("Macroalgal Growth Models") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -15),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5))
  

#----Build_multipanel_plot----

#Get the legend
depth_legend <- 
  get_legend(
    microalgae_CDR_sums_plots + theme(
      legend.title.align = 0.5,
      legend.direction = "horizontal",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      # legend.key.width = unit(0.75, "in"),
      legend.justification = "center"
    )
  ) %>% as_ggplot()


#Combine the plots only (no legend)
CDR_total_bar_plots_only <- 
    plot_grid(
      microalgae_CDR_sums_plots + theme(legend.position = "none"),
      macroalgae_CDR_sums_plots + theme(legend.position = "none"),
      ncol = 2,
      align = "hv"
    )

#Add in the legend
CDR_total_bar_plots <- 
  plot_grid(
    CDR_total_bar_plots_only,
    depth_legend,
    nrow = 2,
    align = "hv",
    rel_heights = c(6,1)
  )

#----Export_plot----

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "global_total_CDR.png",
                                  sep = "/"),
                 plot = CDR_total_bar_plots,
                 height = 8,
                 width = 8,
                 units = "in")
  