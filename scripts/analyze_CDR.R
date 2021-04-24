# Analyze the grid of annual CDR from upwelling

#----Load_CDR_grid----
CDR_grid <- 
  readRDS(file = str_c(working_data_directory,
                       "CDR_grid.RDS",
                       sep="/"))

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



#----Calculate_technical_potential_CDR----
#Maximum technically available CDR for each model at each grid cell
CDR_tech_potential_grid <- 
  CDR_grid %>% 
  filter(depth_m >= MLD_max,
         depth_m <= pipe_depth) %>% 
  group_by(lon, lat, model) %>% 
  slice_max(CDR_annual_lb, .preserve = TRUE) #At each grid cell, grab the depth with the maximum CDR
                                             #...doesn't matter if choosing 'lb' or 'ub' they scale linearly

#Global sum by adding up maximum all sites *where positive CDR can be generated*...
#...Would not deploy at sites only capable of generating negative CDR
CDR_tech_potential_sum <- 
  CDR_tech_potential_grid %>% 
  filter(CDR_annual_lb > 0) %>% 
  mutate(cell_area = map_dbl(lat, cell_grid_area)) %>% 
  mutate(across(contains("CDR_annual"), ~ .x * cell_area)) %>% #ton CO2/m^2/yr * m^2/cell = ton CO2/year/cell
  group_by(model) %>% 
  summarize(across(contains("CDR_annual"), ~sum(.x)/1e9)) #sum across all cells/1e9 = Gt CO2/year globally

#----Calculate_geophysical_potential_CDR----
#Maximum CDR available if pipe depth was NOT a constraint for each model at each grid 
CDR_geophysical_potential_grid <- 
  CDR_grid %>% 
  filter(depth_m >= MLD_max) %>% #eliminate pipe depth requirement
  group_by(lon, lat, model) %>% 
  slice_max(CDR_annual_lb, .preserve = TRUE) #At each grid cell, grab the depth with the maximum CDR
                                             #...doesn't matter if choosing 'lb' or 'ub' they scale linearly  

CDR_geophysical_potential_sum <- 
  CDR_geophysical_potential_grid %>% 
  filter(CDR_annual_lb > 0) %>% 
  mutate(cell_area = map_dbl(lat, cell_grid_area)) %>% 
  mutate(across(contains("CDR_annual"), ~ .x * cell_area)) %>% #ton CO2/m^2/yr * m^2/cell = ton CO2/year/cell
  group_by(model) %>% 
  summarize(across(contains("CDR_annual"), ~sum(.x)/1e9)) #sum across all cells/1e9 = Gt CO2/year globally

    
