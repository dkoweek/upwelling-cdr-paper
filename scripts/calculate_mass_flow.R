#----Load_the_initialized_grid----
mass_flow_grid <-
  readRDS(file = str_c(working_data_directory,
                       "upwelling_grid_initial.RDS",
                       sep = "/")) %>% 
  select(c(lon, lat, month, MLD, MLD_max, S_ML_xyt, T_ML_xyt, depth_m, T, S))

#----Pipe_mass_flow_scenarios----
#Based on literature review
pipe_depth <- 500 #meters
pipe_volume_flow <- 0.05 #m^3/s/pipe
pipe_density <- c(0.1, 1) #pipes/km^2 
m2_per_km2 <- 1e6 #m^2/km^2
pipe_density <- pipe_density / m2_per_km2 #pipes/m^2


seconds_per_day <- 60*60*24 #seconds per day

mass_flow_grid <- 
  mass_flow_grid %>% 
  mutate.(rho_xyz = map2_dbl.(S, T, swRho, pressure = 0)) %>%  #At P=0 b/c interested in density after upwelling
  mutate.(Q_pipe_xyz = rho_xyz * pipe_volume_flow) %>% #kg/m^3 * m^3/s/pipe = kg/s/pipe
  mutate.(Q_pipe_xyz = case_when.(month %in% c(1,3,5,7,8,10,12) ~ Q_pipe_xyz * seconds_per_day * 31, #kg/month/pipe
                                     month %in% c(4,6,9,11) ~ Q_pipe_xyz * seconds_per_day * 30,
                                     TRUE ~ Q_pipe_xyz * seconds_per_day * 28)) %>%
  mutate.(Q_xyz_lb = Q_pipe_xyz * pipe_density[1],
          Q_xyz_ub = Q_pipe_xyz * pipe_density[2]) %>% #kg/month/pipe * pipe/m^2 = kg/m^2/month
  mutate.(pipe_depth = pipe_depth) #Save for later use

#----Save_the_grid----
saveRDS(mass_flow_grid,
        str_c(working_data_directory,
              "mass_flow_grid.RDS",
              sep = "/"))