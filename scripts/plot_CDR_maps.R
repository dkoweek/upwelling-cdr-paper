#Need to acquire global grids

#----Re-run_results_as_needed----
# source(here::here("scripts",
#                   "analyze_CDR.R"))


#----Set_parameters_for_plots----

CDR_color_limits <- c(-1,1)
CDR_color_breaks <- seq(-1,1, by = 0.5)

depth_color_limits <- 
  depth_surfaces_matchup_table %>% 
  pull(depth_m) %>% 
  range()
depth_color_breaks <- c(1000,2000,3000,4000,5000)
max_pipe_length <- 
  CDR_tech_potential_grid %>% 
  pull(pipe_depth) %>% 
  first()


#----Build_heat_maps----
CDR_heat_maps <- list()
depth_heat_maps <- list()
grids <- list(CDR_tech_potential_grid, CDR_geophysical_potential_grid)
k <- 1

for (j in 1:length(grids)) {
  for (i in 1:length(models)) {
  
    #CDR heat maps
    CDR_heat_maps[[k]] <- 
      grids[[j]] %>% 
      filter(model == models[i]) %>% 
      mutate(across(CDR_annual_lb:CDR_annual_ub, ~ .x * m2_to_km2)) %>%  #tons CO2/m^2/yr -> tons CO2/km^2/yr 
      ggplot(aes(x = lon,
                 y = lat)) +
      geom_tile(aes(fill = CDR_annual_ub)) +
      scale_fill_scico(name = expression(atop(Potential~CDR,(tons~CO[2]~km^{-2}~yr^{-1}))),
                       limits = CDR_color_limits,
                       breaks = CDR_color_breaks,
                       palette = "vik") +
      scale_x_continuous(name = expression(Longitude~(degree~E))) +
      scale_y_continuous(name = expression(Latitude~(degree~N))) +
      ggtitle(model_titles[i]) +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            panel.background = element_rect(fill = "grey"))
    
    #Depth of maximum CDR heat maps
    depth_heat_maps[[k]] <- 
      grids[[j]] %>% 
      filter(model == models[i]) %>% 
      ggplot(aes(x = lon,
                 y = lat)) +
      geom_tile(aes(fill = depth_m)) +
      scale_x_continuous(name = expression(Longitude~(degree~E))) +
      scale_y_continuous(name = expression(Latitude~(degree~N))) +
      ggtitle(model_titles[i]) +
      theme_bw() +
      theme(panel.grid = element_blank(), 
            panel.background = element_rect(fill = "grey"))
    
    #Different color charts for technical and geophysical potentials
    if (j == 1) {
      
      depth_heat_maps[[k]] <-
        depth_heat_maps[[k]] +
        scale_fill_scico(name = expression(atop("Pumping Depth for","Maximum CDR (m)")),
                           limits = c(0, max_pipe_length),
                           breaks = seq(0,max_pipe_length, by = 100),
                           palette = "batlow")
      
    } else {
      
      depth_heat_maps[[k]] <-
        depth_heat_maps[[k]] +
        scale_fill_scico(name = expression(atop("Pumping Depth for","Maximum CDR (m)")),
                         limits = depth_color_limits,
                         breaks = depth_color_breaks,
                         palette = "batlow")
    }
    
    k <- k + 1
    
  }
}


  

#----Build_CDR_panel_plots----
CDR_legend <- 
  get_legend(
    CDR_heat_maps[[1]] +
      theme(
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(1, "in")
        )) %>% as_ggplot()


CDR_microalgae_subpanel_plots <- list()
CDR_macroalgae_subpanel_plots <- list()
CDR_panel_plots <- list()

for (n in 1:2) {
  CDR_microalgae_subpanel_plots[[n]] <- 
    plot_grid(
      CDR_heat_maps[[1 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_heat_maps[[2 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_heat_maps[[3 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_heat_maps[[4 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_heat_maps[[5 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      nrow = 5,
      align = "hv")
  
  CDR_macroalgae_subpanel_plots[[n]] <- 
    plot_grid(
      CDR_heat_maps[[6 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_heat_maps[[7 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_heat_maps[[8 + (length(models)*(n-1))]] + theme(legend.position = "none"),
      CDR_legend,
      nrow = 4,
      rel_heights = c(1,1,1,2),
      align = "h"
    )
  
  CDR_panel_plots[[n]] <- 
    plot_grid(
      CDR_microalgae_subpanel_plots[[n]],
      CDR_macroalgae_subpanel_plots[[n]],
      ncol = 2,
      axis = "v"
    )
      
  
}

#----Build_pumping_depth_panel_plots----

depth_legend_tech_potential <- 
  get_legend(
    depth_heat_maps[[1]] +
      theme(
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(1, "in")
      )) %>% as_ggplot()

depth_legend_geophysical_potential <- 
  get_legend(
    depth_heat_maps[[1 + length(models)]] +
      theme(
        legend.title.align = 0.5,
        legend.direction = "vertical",
        legend.justification = "center",
        legend.key.width = unit(1, "in")
      )) %>% as_ggplot()


depth_tech_potential_microalgae_subpanel <- 
  plot_grid(
    depth_heat_maps[[1]] + theme(legend.position = "none"),
    depth_heat_maps[[2]] + theme(legend.position = "none"),
    depth_heat_maps[[3]] + theme(legend.position = "none"),
    depth_heat_maps[[4]] + theme(legend.position = "none"),
    depth_heat_maps[[5]] + theme(legend.position = "none"),
    nrow = 5,
    align = "hv"
  )

depth_tech_potential_macroalgae_subpanel <- 
  plot_grid(
    depth_heat_maps[[6]] + theme(legend.position = "none"),
    depth_heat_maps[[7]] + theme(legend.position = "none"),
    depth_heat_maps[[8]] + theme(legend.position = "none"),
    depth_legend_tech_potential,
    nrow = 4,
    rel_heights = c(1,1,1,2),
    align = "hv"
  )

depth_tech_potential_panel_plot <- 
  plot_grid(
    depth_tech_potential_microalgae_subpanel,
    depth_tech_potential_macroalgae_subpanel,
    ncol = 2,
    axis = "v"
  )


depth_geophysical_potential_microalgae_subpanel <- 
  plot_grid(
    depth_heat_maps[[1 + length(models)]] + theme(legend.position = "none"),
    depth_heat_maps[[2 + length(models)]] + theme(legend.position = "none"),
    depth_heat_maps[[3 + length(models)]] + theme(legend.position = "none"),
    depth_heat_maps[[4 + length(models)]] + theme(legend.position = "none"),
    depth_heat_maps[[5 + length(models)]] + theme(legend.position = "none"),
    nrow = 5,
    align = "hv"
  )

depth_geophysical_potential_macroalgae_subpanel <- 
  plot_grid(
    depth_heat_maps[[6 + length(models)]] + theme(legend.position = "none"),
    depth_heat_maps[[7 + length(models)]] + theme(legend.position = "none"),
    depth_heat_maps[[8 + length(models)]] + theme(legend.position = "none"),
    depth_legend_geophysical_potential,
    nrow = 4,
    rel_heights = c(1,1,1,2),
    align = "hv"
  )

depth_geophysical_potential_panel_plot <- 
  plot_grid(
    depth_geophysical_potential_microalgae_subpanel,
    depth_geophysical_potential_macroalgae_subpanel,
    ncol = 2,
    axis = "v"
  )


#----Export_plots----
cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "CDR_tech_pot_panel.png",
                                  sep = "/"),
                 plot = CDR_panel_plots[[1]],
                 height = 11,
                 width = 8,
                 units = "in")

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "CDR_geophysical_pot_panel.png",
                                  sep = "/"),
                 plot = CDR_panel_plots[[2]],
                 height = 11,
                 width = 8,
                 units = "in")

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "depth_tech_pot_panel.png",
                                  sep = "/"),
                 plot = depth_tech_potential_panel_plot,
                 height = 11,
                 width = 8,
                 units = "in")

cowplot::ggsave2(filename = str_c(working_data_directory,
                                  "depth_geophysical_pot_panel.png",
                                  sep = "/"),
                 plot = depth_geophysical_potential_panel_plot,
                 height = 11,
                 width = 8,
                 units = "in")