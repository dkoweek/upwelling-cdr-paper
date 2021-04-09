#----Load_the_initialized_grid----
mass_flow_grid <-
  readRDS(file = str_c(working_data_directory,
                       "upwelling_grid_initial.RDS",
                       sep = "/")) %>% 
  select(c(lon, lat, month, MLD, MLD_max, S_ML_xyt, T_ML_xyt, depth_m, T, S))

#----Load_custom_functions----
cell_grid_area <- function(lat) {
  
  earth_radius <- 6.371e6 #m
  
  lat_lb <- (lat - 0.5) * (pi / 180)
  
  lat_ub <- (lat + 0.5) * (pi / 180)
  
  lon_diff <- 1 * (pi / 180) #Assume all cells 1x1 degree
  
  area <-
    (sin(lat_ub) -
       sin(lat_lb)) *
    lon_diff *
    earth_radius ^ 2
  
  return(area) #square meters
  
}

sub_ML_rho <- function(data) {
  
  #Acquire the density of the layer directly below the mixed layer
  df <- 
    data[which(depth_m > MLD),]
  
  ifelse(nrow(df) < 1, NA, df[which.min(depth_m),rho_xyz])
  
  
}

#----Maximum_mass_flow_scenarios----
#Maximum mass flow such that ML density does not exceed density of layer...
#...directly below the mixed layer (see Fennel 2008 - MEPS)
mass_flow_grid <- 
  mass_flow_grid %>% 
  mutate.(rho_ML_xyt = map2_dbl.(S_ML_xyt, T_ML_xyt, swRho, pressure = 0),
          rho_xyz = map2_dbl.(S, T, swRho, pressure = 0), #At P=0 b/c interested in density after upwelling
          cell_area = map_dbl.(lat, cell_grid_area),
          V_ML = MLD * cell_area) %>% #m^3
  nest_by.(lon, lat, month) %>% 
  mutate.(rho_sub_ML_xyt = map_dbl.(data, sub_ML_rho)) %>% #density directly below the mixed layer
  unnest.() %>% 
  mutate.(Q_max_xyzt = V_ML * ((rho_sub_ML_xyt - rho_ML_xyt) / (rho_xyz - rho_sub_ML_xyt)), #m^3/month (Fennel 2008)
          Q_max_xyzt = Q_max_xyzt * rho_sub_ML_xyt / cell_area) #kg/m^2/month (At this max mass flow rate, rho_ML = rho_sub_ML) 


#----Pipe_mass_flow_scenarios----
pipe_depth <- 500 #meters (from literature)
pipe_volume_flow <- c(0.01, 0.1) #m^3/s/pipe
pipe_density <- c(0.1, 1) #pipes/km^2 
m2_per_km2 <- 1e6 #m^2/km^2
pipe_density <- pipe_density / m2_per_km2 #pipes/m^2
pipe_scenarios <- 
  expand_grid(pipe_volume_flow,
              pipe_density)

seconds_per_day <- 60*60*24 #seconds per day

mass_flow_grid <- 
  mass_flow_grid %>% 
  mutate.(Q_pipe_xyzt = rho_xyz * pipe_volume_flow * pipe_density) %>% #kg/m^3 * m^3/s/pipe * pipe/m^2 = kg/m^2/s
  mutate.(Q_pipe_xyzt = case_when.(month %in% c(1,3,5,7,8,10,12) ~ Q_pipe_xyzt * seconds_per_day * 31, #kg/m^2/month
                                   month %in% c(4,6,9,11) ~ Q_pipe_xyzt * seconds_per_day * 30,
                                   TRUE ~ Q_pipe_xyzt * seconds_per_day * 28)) %>% 
  #Pipe flow cannot exceed geophysical limit
  mutate.(Q_pipe_xyzt = case_when.(Q_pipe_xyzt > Q_max_xyzt ~ Q_max_xyzt,
                                   TRUE ~ Q_pipe_xyzt))

#----Save_the_grid----
saveRDS(mass_flow_grid,
        str_c(working_data_directory,
              "mass_flow_grid.RDS",
              sep = "/"))