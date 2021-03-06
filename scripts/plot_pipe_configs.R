#Load in summary table of upwelling pipe field tests and modeling uses

pipes_df <- 
  read_xlsx(path = str_c(upwelling_directory,
                         "upwelling_pipe_summary_table.xlsx",
                         sep = "/"))

#Build plot
pipe_configs_plot <- 
  pipes_df %>% 
  filter(Include_in_Analysis == "Y") %>% 
  mutate(Maximum_Pipe_Length_m = as.numeric(Maximum_Pipe_Length_m),
         Volumetric_Flow_m3_s = as.numeric(Volumetric_Flow_m3_s)) %>% 
  ggplot(aes(x = Maximum_Pipe_Length_m,
             y = Volumetric_Flow_m3_s,
             label = Source)) + 
  geom_point(aes(shape = Field_Model,
                 colour = Field_Model),
             size = 6) +
  geom_text_repel(box.padding = 0.6) +
  scale_colour_scico_d(palette = "vik",
                       begin = 0.15, 
                       end = 0.85) +
  scale_y_continuous(name = expression(Volumetric~Flow~per~Pipe~(m^3~s^{-1})),
                     limits = c(0, 0.5),
                     breaks = seq(0,0.5, length.out = 6)) +
  scale_x_continuous(name = "Maximum Pipe Length (meters)",
                     limits = c(0, 1000),
                     breaks = c(0, 250, 500, 750, 1000)) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

#Add in test cases in this study
study_config_df <- data.frame(Maximum_Pipe_Length_m = 500,
                              Volumetric_Flow_m3_s = 0.05,
                              Source = "Tech. Limit: This Study")

pipe_configs_plot <-
  pipe_configs_plot +
  geom_point(data = study_config_df,
             alpha = 0.5,
             colour = "red",
             shape = "square",
             size = 6,
             show.legend = FALSE) +
  geom_text_repel(data = study_config_df,
                  box.padding = 0.75)
  

#----Export_plot----
ggsave(filename = str_c(working_data_directory,
                        "pipe_configurations.png",
                        sep = "/"),
       plot = pipe_configs_plot,
       height = 6.5,
       width = 7,
       units = "in")
