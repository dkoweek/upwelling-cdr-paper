#Need to acquire global grids

#----Re-run_results_as_needed----
# source(here::here("scripts",
#                   "analyze_CDR.R"))


#----Join_technical_potential_and_geophysical_potential----
CDR_grids_combined <- 
  CDR_tech_potential_grid %>% 
  ungroup() %>% 
  mutate(potential = "tech") %>% 
  bind_rows(CDR_geophysical_potential_grid %>% 
              ungroup() %>% 
              mutate(potential = "geophysical")) %>% 
  filter(!grepl("E_k_const", model)) %>% 
  select(c(CDR_annual_ub, model, potential)) %>% 
  mutate(algae = case_when(str_detect(string = model, pattern = "atkinson") == TRUE ~ "macroalgae",
                           TRUE ~ "microalgae")) %>% 
  mutate(potential = factor(potential, levels = c("tech", "geophysical")))


#----Set_common_plot_parameters----
CDR_limits <- c(-0.6,0.9)
CDR_breaks <- seq(CDR_limits[1], CDR_limits[2], by = 0.3)

#----Plot_distributions----
CDR_cdf_plots <- list()
CDR_pdf_plots <- list()

for (i in 1:length(models)) {
  
  CDR_cdf_plots[[i]] <- 
    CDR_grids_combined %>% 
    filter(model == models[i]) %>% 
    mutate(CDR = CDR_annual_ub * m2_to_km2) %>% #ton CO2/m^2/yr -> ton CO2/km^2/yr
    ggplot(aes(x = CDR,
               colour = potential)) +
    stat_ecdf(pad = FALSE,
              size = 1) +
    scale_x_continuous(name = expression(CDR~Potential~~(tons~~CO[2]~~km^{-2}~~yr^{-1})),
                       limits = CDR_limits,
                       breaks = CDR_breaks) +
    scale_y_continuous(name = "Cumulative Frequency") +
    scale_colour_viridis(discrete = TRUE,
                         name = "Pipe Depths Considered",
                         labels = depth_category_labels) +
    ggtitle(model_titles[i]) +
    theme_bw()
  
  CDR_pdf_plots[[i]] <- 
    CDR_grids_combined %>% 
    filter(model == models[i]) %>% 
    mutate(CDR = CDR_annual_ub * m2_to_km2) %>% #ton CO2/m^2/yr -> ton CO2/km^2/yr
    ggplot(aes(x = CDR,
               colour = potential)) +
    geom_density(size = 1) +
    scale_x_continuous(name = expression(CDR~Potential~~(tons~~CO[2]~~km^{-2}~~yr^{-1})),
                       limits = CDR_limits,
                       breaks = CDR_breaks) +
    scale_y_continuous(name = "Density") +
    scale_colour_viridis(discrete = TRUE,
                         name = "Pipe Depths Considered",
                         labels = depth_category_labels) +
    ggtitle(model_titles[i]) +
    theme_bw()
  
  
}


#----Arrange_into_panel----
#Get depth legend
distributions_legend <- 
  get_legend(
    CDR_cdf_plots[[1]] +
      theme(
        legend.title.align = 0.5,
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(1, "in"),
        legend.key.height = unit(0.75, "in")
      )) %>% as_ggplot()


#CDFs
CDR_cdf_microalgae_subpanel_plot <-
  plot_grid(CDR_cdf_plots[[1]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_cdf_plots[[2]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_cdf_plots[[3]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_cdf_plots[[4]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_cdf_plots[[5]] + theme(legend.position = "none"),
            nrow = 5,
            align = "hv") 

CDR_cdf_macroalgae_subpanel_plot <- 
  plot_grid(CDR_cdf_plots[[6]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_cdf_plots[[7]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_cdf_plots[[8]] + theme(legend.position = "none"),
            distributions_legend,
            nrow = 4,
            rel_heights = c(1,1,1,2),
            align = "hv")

CDR_cdf_panel_plot <-
  plot_grid(CDR_cdf_microalgae_subpanel_plot,
            CDR_cdf_macroalgae_subpanel_plot,
            ncol = 2,
            align = "hv")

#PDFs
CDR_pdf_microalgae_subpanel_plot <-
  plot_grid(CDR_pdf_plots[[1]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_pdf_plots[[2]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_pdf_plots[[3]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_pdf_plots[[4]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_pdf_plots[[5]] + theme(legend.position = "none"),
            nrow = 5,
            align = "hv") 

CDR_pdf_macroalgae_subpanel_plot <- 
  plot_grid(CDR_pdf_plots[[6]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_pdf_plots[[7]] + theme(axis.title.x = element_blank(), legend.position = "none"),
            CDR_pdf_plots[[8]] + theme(legend.position = "none"),
            distributions_legend,
            nrow = 4,
            rel_heights = c(1,1,1,2),
            align = "hv")

CDR_pdf_panel_plot <-
  plot_grid(CDR_pdf_microalgae_subpanel_plot,
            CDR_pdf_macroalgae_subpanel_plot,
            ncol = 2,
            align = "hv")



#----Export_plots----
cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "CDR_cdf_panel.png",
                                  sep = "/"),
                 plot = CDR_cdf_panel_plot,
                 height = 11,
                 width = 8,
                 units = "in")

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "CDR_pdf_panel.png",
                                  sep = "/"),
                 plot = CDR_pdf_panel_plot,
                 height = 11,
                 width = 8,
                 units = "in")