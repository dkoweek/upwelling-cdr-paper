#Load netCDF file
mixed_layer_depth_data <- 
  here::here("data",
             "input_data",
             "Argo_mixedlayers_monthlyclim_12112019.nc") %>% 
  tidync()

#Calculate annual mean MLD data from monthly-resolved MLD data
MLD_annual_mean <- 
  mixed_layer_depth_data %>% 
  hyper_tibble(select_var = c("mld_da_mean", "mld_dt_mean")) %>% 
  rename(lon = iLON,
         lat = iLAT) %>% 
  group_by(lon, lat) %>% 
  summarise(MLD_algorithm = mean(mld_da_mean),
            MLD_threshold = mean(mld_dt_mean),
            months_MLD_data = n_distinct(iMONTH),
            .groups = "keep") %>% 
  ungroup() 

#Wrangle lat/lon data to match GLODAP data
MLD_annual_mean <- 
  MLD_annual_mean %>% 
  mutate(lat = lat - 90.5, #first lat bin spans 90S - 89S, centered at 89.5S
         lon = lon - 180.5) #first lon bin spans 180-179W, centered at 179.5W 