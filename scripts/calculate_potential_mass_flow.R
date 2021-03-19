#----Load_the_initialized_grid----
mass_flow_grid <-
  readRDS(file = str_c(working_data_directory,
                       "delta_CO2_grid_initial.RDS",
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
  
  df <- 
    df[which.min(depth_m),]
  
  return(df[["rho_xyz"]])


}

#----Calculate_maximum_flow----
#Maximum volume addition such that ML density does not exceed density of layer...
#...directly below the mixed layer (see Fennel 2008 - MEPS)

mass_flow_grid <- 
  mass_flow_grid %>% 
  mutate.(rho_ML_xyt = map2_dbl.(S_ML_xyt, T_ML_xyt, swRho, pressure = 0),
          rho_xyz = map2_dbl.(S, T, swRho, pressure = 0), #At P=0 b/c interested in density after upwelling
          cell_area = map_dbl.(lat, cell_grid_area),
          V_ML = MLD * cell_area) %>% #m^3
  nest_by.(lon, lat, month) %>% 
  mutate.(rho_sub_ML_xyt = map_dbl.(data, sub_ML_rho)) %>% 
  unnest.() %>% 
  mutate.(Q_max_xyzt = V_ML * ((rho_sub_ML_xyt - rho_ML_xyt) / (rho_xyz - rho_sub_ML_xyt))) #m^3/month

#----Save_the_grid----
saveRDS(mass_flow_grid,
        str_c(working_data_directory,
              "mass_flow_grid.RDS",
              sep = "/"))