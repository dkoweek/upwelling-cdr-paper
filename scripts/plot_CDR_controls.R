#----Re-run_results_as_needed----
# source(here::here("scripts",
#                   "analyze_CDR.R"))
# 
# source(here::here("scripts",
#                    "load_GLODAP_data.R"))


#----Assemble_data_sets----
stoichiometries_df <-
    data.frame(
         models,
         model_titles,
         N_P = c(NA,12.5,18.1,16,16,20,31,47),
         C_N = c(NA, 6.9, 6.5, 6.6, 6.6, 22, 18.4, 16.1),
         C_P = c(NA, 86.4, 118, 106, 106, 440, 570, 758)
         ) %>%
    as_tibble()


histograms_dataset <-
  CDR_geophysical_potential_grid %>%
  filter(!grepl("E_k_const", model)) %>%
  left_join(.,
            GLODAP_data %>% select(c(lon, lat, NO3, PO4, depth_m)),
            by = c("lon", "lat", "depth_m")) %>%
  mutate(N_P = NO3 / PO4,
         N_P_Galbraith = N_C(NO3) / P_C(PO4),
         C_N_Galbraith = 1/N_C(NO3),
         C_P_Galbraith = 1/P_C(PO4))

#Fill in C:N and C:P for Galbraith from median values
stoichiometries_df$C_N[1] <-
  histograms_dataset %>% 
  pull(C_N_Galbraith) %>% 
  median()

stoichiometries_df$C_P[1] <-
  histograms_dataset %>% 
  pull(C_P_Galbraith) %>% 
  median()


#----Plot_characteristics----
x_limits <-
  list(c(0,30),
       c(10,20),
       c(10,20),
       c(10,20),
       c(10,20),
       c(10,25),
       c(10,35),
       c(10,50)
    )

y_limits <-
  list(c(0,0.3),
       c(0,1),
       c(0,1),
       c(0,1),
       c(0,1),
       c(0,1),
       c(0,0.75),
       c(0,0.5)
       )

#----Build_histograms----
histogram_plots <- list()

for (i in 1:length(models)) {  
  
histogram_plots[[i]] <- 
  histograms_dataset %>% 
  filter(model == models[i]) %>% 
  ggplot(aes(x = N_P)) +
  geom_histogram(aes(y = ..density..),
                 bins = 25,
                 fill = "blue",
                 colour = "black") +
  scale_x_continuous(limits = x_limits[[i]],
                     name = expression(paste("[",NO[3]^{"-"},"] / [",PO[4]^{"3-"},"]"))) + 
  scale_y_continuous(name = "Density",
                     limits = y_limits[[i]]) +
  ggtitle(model_titles[i]) +
  theme_bw() 
  
}

#----Add_annotations----

#For Galbraith, add histogram of N_P growth stoichiometries
histogram_plots[[1]] <- 
  histogram_plots[[1]] +
  geom_histogram(aes(x = N_P_Galbraith,
                     y = ..density..),
                 bins = 25,
                 colour = "black",
                 fill = "grey",
                 alpha = 0.5)


#For non-Galbraith, non-Redfield: P limited, add N:P stoichiometry line
for (j in c(2,3,4,6,7,8)) {
  
  selected_N_P <- 
    stoichiometries_df %>% 
    filter(models == models[j]) %>% 
    pull(N_P)
  

  histogram_plots[[j]] <- 
    histogram_plots[[j]] +
    geom_vline(xintercept = selected_N_P,
               linetype = "dashed") 
}

#Add labels for each plot
histogram_plots[[2]] <- 
  histogram_plots[[2]] + 
  annotate(geom = "text",
           x = 12,
           y = 0.5,
           label = "N-limited", 
           angle = 90) + 
  annotate(geom = "text",
           x = 13,
           y = 0.5,
           label = "P-limited", 
           angle = 90)

histogram_plots[[3]] <- 
  histogram_plots[[3]] + 
  annotate(geom = "text",
           x = 17.5,
           y = 0.5,
           label = "N-limited", 
           angle = 90) + 
  annotate(geom = "text",
           x = 18.5,
           y = 0.5,
           label = "P-limited", 
           angle = 90)

histogram_plots[[4]] <- 
  histogram_plots[[4]] + 
  annotate(geom = "text",
           x = 15.5,
           y = 0.5,
           label = "N-limited", 
           angle = 90) + 
  annotate(geom = "text",
           x = 16.5,
           y = 0.5,
           label = "P-limited", 
           angle = 90)

histogram_plots[[6]] <- 
  histogram_plots[[6]] + 
  annotate(geom = "text",
           x = 19.25,
           y = 0.5,
           label = "N-limited", 
           angle = 90) + 
  annotate(geom = "text",
           x = 20.75,
           y = 0.5,
           label = "P-limited", 
           angle = 90)

histogram_plots[[7]] <- 
  histogram_plots[[7]] + 
  annotate(geom = "text",
           x = 29.5,
           y = 0.4,
           label = "N-limited", 
           angle = 90) + 
  annotate(geom = "text",
           x = 32.25,
           y = 0.4,
           label = "P-limited", 
           angle = 90)

histogram_plots[[8]] <- 
  histogram_plots[[8]] + 
  annotate(geom = "text",
           x = 45,
           y = 0.25,
           label = "N-limited", 
           angle = 90) + 
  annotate(geom = "text",
           x = 49,
           y = 0.25,
           label = "P-limited", 
           angle = 90)



#----Assemble_panel_plot----
nutrient_limitation_panel <- 
  plot_grid(
    histogram_plots[[1]],
    histogram_plots[[6]],
    histogram_plots[[2]],
    histogram_plots[[7]],
    histogram_plots[[3]],
    histogram_plots[[8]],
    histogram_plots[[4]],
    NULL,
    histogram_plots[[5]],
    NULL,
    ncol = 2,
    align = "hv"
  )

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "nutrient_limitation_panel.png",
                                  sep = "/"),
                 plot = nutrient_limitation_panel,
                 height = 11,
                 width = 8,
                 units = "in")

#----Assemble_control_plots----
macroalgae_controls_plot <-
  CDR_geophysical_potential_sum %>% 
  rename(models = model) %>% 
  right_join(.,
             stoichiometries_df,
             by = "models") %>% 
  select(-CDR_annual_lb) %>% 
  mutate(CDR = CDR_annual_ub * 1e3) %>%  
  filter(grepl("atkinson", models)) %>% 
  mutate(model_titles = factor(model_titles, levels = c("Atkinson Q1",
                                                        "Atkinson Median",
                                                        "Atkinson Q3"))) %>% 
  ggplot(aes(x = C_N,
             y = CDR)) +
  geom_point(aes(colour = model_titles),
             size = 4) +
  scale_colour_viridis(discrete = TRUE,
                       name = element_blank()) + 
  geom_smooth(method = "lm",
              colour = "grey",
              linetype = "dashed",
              se = FALSE) +
  scale_x_continuous(name = "Macroalgae C:N") +
  scale_y_continuous(expression(CDR~Potential~~~(Mt~CO[2]~yr^{-1}))) +
  ggtitle("Macroalgae") +
  theme_bw() +
  theme(legend.position = c(0.72, 0.2),
        legend.background = element_rect(colour = "black"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

microalgae_controls_plot <-
  CDR_geophysical_potential_sum %>% 
  rename(models = model) %>% 
  right_join(.,
             stoichiometries_df,
             by = "models") %>% 
  select(-CDR_annual_lb) %>% 
  mutate(CDR = CDR_annual_ub * 1e3) %>%  
  filter(!grepl("atkinson", models)) %>% 
  ggplot(aes(x = C_N*C_P,
             y = CDR)) +
  geom_point(aes(colour = model_titles),
             size = 4) +
  scale_colour_viridis(discrete = TRUE,
                       name = element_blank(),
                       option = "C") + 
  geom_smooth(method = "lm",
              linetype = "dashed",
              colour = "grey",
              se = FALSE) +
  scale_x_continuous(name = "Microalgae C:N x C:P") +
  scale_y_continuous(expression(CDR~Potential~~~(Mt~CO[2]~yr^{-1}))) +
  ggtitle("Microalgae") +
  theme_bw() +
  theme(legend.position = c(0.25, 0.75),
        legend.background = element_rect(colour = "black"),
        legend.direction = "vertical",
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))


CDR_controls_panel <- 
  plot_grid(
    microalgae_controls_plot,
    macroalgae_controls_plot,
    ncol = 2,
    align = "hv"
  )

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "CDR_controls_panel.png",
                                  sep = "/"),
                 plot = CDR_controls_panel,
                 height = 4,
                 width = 8,
                 units = "in")
